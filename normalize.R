# This function that normalizes transfer coefficient
# distributions together, so that the sum of the outflowing
# transfer coefficients from a same compartment is always equal to 1.
# 
# Author: Delphine Kawecki-Wenger
# Date of last modification: 16.10.2019

normalize <- function(Distr){
  
  # 1. Test the input
  if(!is.list(Distr)){
    stop("Distr should be a list of lists.")
  }
  
  # 2. Normalization step: make sure that all the flows going out of a compartment sum up to 1
  Distr.Norm <- Distr
  
  for(j in 1:length(Distr)){
    
    # if there are no outflows, leave as it is
    if(is.null(Distr[[j]])){
      next
      
    } else {
      # sum the flows that leave a compartment
      thesum <- apply(sapply(Distr[[j]], cbind), 1, sum)
      
      if(all(thesum == 0)){ next }
      
      for(i in 1:length(Distr[[j]])){
        Distr.Norm[[j]][[i]] <- Distr[[j]][[i]]/thesum
      }
    }
  }
  
  message(paste(format(Sys.time(), "%H:%M:%S"),"Normalization complete.")) 
  
  return(Distr.Norm)
}