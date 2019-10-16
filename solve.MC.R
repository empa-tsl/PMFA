# This function performs the Monte-Carlo simulation
# based on the prepared TC and input distributions
# 
# Author: Delphine Kawecki-Wenger
# Date of last modification: 16.10.2019

solve.MC <- function(TC.Distr,
                     inp.Distr,
                     Names,
                     N){
  
  # prepare output
  Mass <- matrix(NA, length(Names), N, dimnames = list(Names, NULL))
  
  # Monte-Carlo iterations
  for(k in 1:N){
    
    # create empty TC matrix
    themat <- matrix(0, length(Names), length(Names), dimnames = list(Names, Names))
    # create empty input vector
    theinput <- sapply(Names, function(x) 0)
    
    for(comp in Names){
      # prepare k-th matrix for equation solving
      if(!is.null(TC.Distr[[comp]])){
        for(dest in 1:length(TC.Distr[[comp]])){
          themat[names(TC.Distr[[comp]])[dest],comp] <- TC.Distr[[comp]][[dest]][k]
        }
      }
      # prepare k-th input for equation solving
      if(!is.null(inp.Distr[[comp]])){
        theinput[comp] <- inp.Distr[[comp]][k]
      }
    }
    
    # transform the matrix
    themat <- -themat
    diag(themat) <- 1
    
    # solve equation
    Mass[,k] <- solve(themat, theinput)
  }
  
  message(paste(format(Sys.time(), "%H:%M:%S"),"Simulation complete."))
  
  return(Mass)
}