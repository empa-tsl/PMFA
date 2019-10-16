# This function samples N values from a triangular probability
# distribution defined using a central value and a spread.
# Upper and lower cutoff can be used to truncate the distribution.
# The default spread is 50%.
# 
# Author: Delphine Kawecki-Wenger
# Date of last modification: 16.10.2019

rtriang.perc <- function(mmode,
                         perc = 0.5,
                         N = SIM,
                         linf = -Inf,
                         lsup = Inf,
                         is.tc = T)
{
  require(mc2d) # for rtrunc
  
  if(is.na(mmode)){
    return(rep(NA,N))
  }
  
  if(mmode == 0){
    return(rep(0,N))
  }
  
  if(is.tc){
    if(mmode == 1){
      return(rep(1,N))
    }
  }
  
  stopifnot(perc > 0,
            is.numeric(mmode),
            is.numeric(perc),
            is.numeric(N),
            is.numeric(linf),
            is.numeric(lsup),
            length(mmode) == 1,
            length(perc) == 1,
            length(N) == 1,
            length(linf) == 1,
            length(lsup) == 1,
            linf < lsup,
            linf <= mmode,
            lsup >= mmode)
  
  mmin <- mmode - abs(mmode*perc)
  mmax <- mmode + abs(mmode*perc)
  
  return(rtrunc(distr = "rtriang", n = N,
                linf = linf, lsup = lsup,
                min = mmin, mode = mmode, max = mmax))
}