# This function calculates what fraction of the mass of one compartment
# finishes in the final sinks (or whichever compartments are chosen to
# stop at). Useful for calculating how much of a product in use is
# finally recycled or ending up in the environment.
# 
# Author: Delphine Kawecki-Wenger
# Date of last modification: 16.10.2019

find.fc <- function(comp, # the name of the compartment to investigate
                    mass = NULL, # the mass needed to multiply the TCs, if NULL, don't multiply
                    stop.at = NULL, # vector of the names of the compartments at which the calculation needs to stop
                    TC.Distr, # list containing the TCs
                    verbose = T, # logical: whether to comment on each calculation step
                    loop.break = 50) # for safety if there is a problem, number of iterations at which to stop
{
  # need a condition for the while loop, set to TRUE to start
  cond <- T
  
  # need an equivalent list to TC.Distr for storing FC values
  FC.Distr <- TC.Distr[[comp]]
  
  # for counting loops
  i <- 1
  while(any(cond)){
    
    if(verbose){ message(paste0("\n\nIteration #",i)) }
    
    # create a condition vector that will be modified within the for loop
    cond <- sapply(names(FC.Distr), function(x) T)
    
    # loop over destinations
    for(dest in names(FC.Distr)){
      
      # if no outflow from destination compartment, skip as it is a sink
      if(is.null(TC.Distr[[dest]])){
        # change condition to FALSE, so that if everything is FALSE, loop stops
        cond[dest] <- F
        
        if(verbose){ message(paste(comp, "->", dest, ":", "Skipped")) }
        
        # if compartment is in the "stop.at" compartments, stop there
      } else if(dest %in% stop.at){
        # change condition to FALSE, so that if everything is FALSE, loop stops
        cond[dest] <- F
        
        if(verbose){ message(paste(comp, "->", dest, ":", "Ignored")) }
        
        # else calculate
      } else {
        
        # else if the TC from compartment "dest" is non-null, find the compartments
        for(dest2 in names(TC.Distr[[dest]])){
          
          # if dest2 already exists as destination in the list, add the contribution
          if(!is.null(FC.Distr[[dest2]])){
            FC.Distr[[dest2]] <- ( FC.Distr[[dest2]] + 
                                     FC.Distr[[dest]]*TC.Distr[[dest]][[dest2]] )
            if(verbose){ message(paste0(comp, " -> (", dest, ") -> ", dest2, " : ", "Added to preexisting")) }
            
          } else {
            # else add the list element
            FC.Distr[[dest2]] <- FC.Distr[[dest]]*TC.Distr[[dest]][[dest2]]
            
            if(verbose){ message(paste0(comp, " -> (", dest, ") -> ", dest2, " : ", "Added")) }
          }
          
        }
        
        # and remove the first, otherwise it will be added again
        FC.Distr[[dest]] <- NULL
      }
    }
    
    # increase loop iteration number
    i <- i+1
    
    # test for precision
    if(verbose){ 
      total <- apply(do.call("cbind", FC.Distr), 1, sum)
      if(all(abs(total - 1) < 10^-9)){
        message("\nSum of out-TCs is 1 (precision of at least 10^-9)")
      } else {
        message("\nSum of out-TCs is ", mean(total))
      }
    }
    
    if(i>loop.break){
      cond <- F
      message("\n!!! Calculation interrupted artificially after ",loop.break, " iterations.\n... Check if there may be a closed loop in the defined flows, or increase the limit of 'loop.break'.")
    }
  }
  
  # test for precision
  total <- apply(do.call("cbind", FC.Distr), 1, sum)
  if(!all(abs(total - 1) < 10^-9)){
    warning("\n--------------- Sum of out-TCs is ", mean(total), " ---------------")
  }
  
  # multiply with mass
  if(!is.null(mass)){
    for(cc in names(FC.Distr)){
      FC.Distr[[cc]] <- FC.Distr[[cc]]*mass
    }
  }
  
  return(FC.Distr)
}