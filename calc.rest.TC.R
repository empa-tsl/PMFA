# This function calculates the probability distributions
# for transfer coefficients defined as the remaining fraction.
# 
# Author: Delphine Kawecki-Wenger
# Date of last modification: 16.10.2019

calc.rest.TC <- function(TC){
  
  # for compartments where the flow is defined as the rest, take the rest
  for(comp in 1:length(TC)){
    
    # find the destination compartment that is a 'rest' flow
    restcomp <- which(sapply(TC[[comp]], function(x) any(x == "rest")))
    
    # if there is more than one rest flow, give error
    if(length(restcomp) > 1){
      stop("There can't be more than one compartment with a 'rest' flow.")
      
      # if there is no rest flow, go next
    } else if(length(restcomp) == 0){
      return(TC)
    }
    
    # if one flow is undefined, do nothing
    if(any(sapply(TC[[comp]], function(x) any(is.na(x))))){
      next
    }
    
    # find the other flows from the compartment
    otherflows <- TC[[comp]]
    otherflows[[restcomp]] <- NULL
    
    # in case more than one other flow, sum them up
    otherflows <- apply(do.call(cbind, otherflows), 1, sum)
    if(any(otherflows > 1)){
      message( "WARNING: Sum of outflows from ",
               names(TC)[comp],
               " > 1 (",
               round(length(which(otherflows > 1))/length(otherflows)*100, digits = 2),
               "% of iterations). TC to ",
               names(TC[[comp]])[restcomp],
               " contains artificial zeroes.")
    }
    
    # subtract from one, tadaaa!
    TC[[comp]][[restcomp]] <- ifelse(otherflows > 1, 0, 1 - otherflows)
    
    # redefine other flows to make sure the normalization does not introduce negative values
    if(any(otherflows > 1)){
      
      # redefine the sum of the otherflows to match the rest flow
      redefotherflows <- 1 - TC[[comp]][[restcomp]]
      
      for(dest in names(TC[[comp]])){
        
        # skip rest flow
        if(dest == restcomp){ next }
        
        # else renormalize it
        TC[[comp]][[dest]] <- TC[[comp]][[dest]]*redefotherflows/otherflows
        
      }
      
    }
  }
  
  message(paste(format(Sys.time(), "%H:%M:%S"), "Rest flows calculated.")) 
  return(TC)
  
}