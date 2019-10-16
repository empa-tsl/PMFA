# This function samples N values from a trapezoidal probability distribution
# defined using two central values and their corresponding spreads.
# Upper and lower cutoff can be used to truncate the distribution.
# The default spread is 50%.
# 
# Author: Delphine Kawecki-Wenger
# Date of last modification: 16.10.2019

# trapezoidal distribution with percentage (default being 50%)
rtrapez.perc <- function(values,
                         perc = c(0.5,0.5),
                         N = SIM,
                         linf = -Inf,
                         lsup = Inf)
{
  require(mc2d) # for rtrunc
  require(trapezoid)
  
  if(any(is.na(values))){
    return(rep(NA,N))
  }
  
  stopifnot(all(perc[!is.na(perc)] > 0),
            is.numeric(values),
            is.numeric(perc),
            is.numeric(N),
            is.numeric(linf),
            is.numeric(lsup),
            length(values) == 2,
            length(perc) == 2,
            length(N) == 1,
            length(linf) == 1,
            length(lsup) == 1,
            linf < lsup,
            all(linf <= values),
            all(lsup >= values),
            all(is.finite(values)),
            all(is.finite(perc[values > 0])))
  
  # sort data
  perc <- perc[order(values)]
  values <- values[order(values)]
  
  if(is.na(perc[1]) & values[1] == 0){
    mmin <- 0
  } else {
    mmin <- values[1] - abs(values[1]*perc[1])  
  }
  
  mmax <- values[2] + abs(values[2]*perc[2])
  
  return(rtrunc(distr = "rtrapezoid", n = N,
                linf = linf, lsup = lsup,
                min = mmin, max = mmax,
                mode1 = values[1], mode2 = values[2]))
}
