# This function creates probability distributions based on the transfer
# coefficient data provided. All distributions are truncated below 0 and
# above 1.
# 
# Author: Delphine Kawecki-Wenger
# Date of last modification: 16.10.2019

# function to import data stored in Excel for transfer coefficients
import.TC <- function(Coeff,
                      Names){
  
  TC <- sapply(Names, function(x) NULL)
  
  # loop over compartments from which something flows
  for(comp in unique(Coeff[,"From"])){
    
    # find destination compartments from this compartment
    tocomp <- unique(Coeff[Coeff[,"From"] == comp,"To"])
    
    # if there is no destination compartment, go to next iteration
    if(length(tocomp) == 0){ next }
    
    # else loop over destination compartments
    for(dest in tocomp){
      
      # find the data corresponding to destination
      datapoints <- Coeff[Coeff[,"From"] == comp & Coeff[,"To"] == dest,
                          c("From", "To", "Data", "Spread")]
      
      # in case there is just one datapoint, need to format as a matrix, to avoid errors
      if(is.null(dim(datapoints))){ 
        dim(datapoints) <- c(1,4)
        colnames(datapoints) <- c("From", "To", "Data", "Spread")
      }
      
      if(any(is.na(datapoints))){
        stop(paste("The flow from", comp, "to", dest, "is ill-defined."))
      }
      
      # for compartments where the flow is defined as the rest, fill with NA data and move on to next iteration
      if(any(datapoints[,"Data"] == "rest")){
        TC[[comp]][[dest]] <- rep("rest",SIM)
        next
      }
      
      
      if(dim(datapoints)[1] == 1){ # triangle for a single data point
        TC[[comp]][[dest]] <- rtriang.perc(mmode = as.numeric(datapoints[, "Data"]), 
                                           perc = as.numeric(datapoints[, "Spread"]),
                                           N = SIM,
                                           linf = 0,
                                           lsup = 1,
                                           is.tc = F)
        
        
      } else if(dim(datapoints)[1] == 2){ # trapezoid for double datapoint
        TC[[comp]][[dest]] <- rtrapez.perc(values = as.numeric(datapoints[, "Data"]), 
                                           perc = as.numeric(datapoints[, "Spread"]),
                                           N = SIM,
                                           linf = 0,
                                           lsup = 1)
        
      } else { # not implemented for more
        stop("There are more than 2 datapoints for the TC from '", comp,"' to '",dest,"'.")
      }
      
      
    }
  }
  
  message(paste(format(Sys.time(), "%H:%M:%S"), "TC formatting complete."))
  
  return(TC)
  
}