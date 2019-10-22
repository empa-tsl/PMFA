# This function creates probability distributions based
# on the input data provided
# 
# Author: Delphine Kawecki-Wenger
# Date of last modification: 22.10.2019

import.input <- function(data,
                         SIM,
                         linf = 0){
  
  # create empty lists
  Input <- list()
  
  # take appropriate data for the system
  data <- data[, c("Compartment", "Data", "Spread")]
  
  # take only the data for which there is data and uncertainty provided
  data <- data[apply(data, 1, function(x) all(!(is.na(x) | x == 0))),]
  
  # check if the data contains negative values
  if( any(as.numeric(data[, "Data"]) < 0) ){
    stop("There are negative input values for compartment ",comp,".")
  } 
  
  # loop over compartments with an inflow
  for(comp in unique(data[,"Compartment"])){
    
    # find data corresponding to the compartment and material
    datapoints <- which(data[,"Compartment"] == comp)
    
    # create probability distribution
    if(length(datapoints) == 1){ # triangle for a single data point
      Input[[comp]] <- rtriang.perc(mmode = as.numeric(data[datapoints, "Data"]), 
                                    perc = as.numeric(data[datapoints, "Spread"]),
                                    N = SIM,
                                    linf = linf,
                                    is.tc = F)
      
      
    } else if(length(datapoints) == 2){ # trapezoid for double datapoint
      Input[[comp]] <- rtrapez.perc(values = as.numeric(data[datapoints, "Data"]), 
                                    perc = as.numeric(data[datapoints, "Spread"]),
                                    N = SIM,
                                    linf = linf)
      
    } else { # not implemented for more
      stop("There are more than 2 datapoints for the input in compartment '", comp,"'.")
    }
    
  }
  
  message(paste(format(Sys.time(), "%H:%M:%S"), "Input formatting complete."))
  return(Input)
  
}