get_vec <- function(x, minfac=.1, maxfac=3, mode=c("fs","sf","fsf"), 
                    c=NULL, hw=1, mustinclude=NULL, addvalues=NULL){
  mode     <- mode[1]   # default arg lists all options as reminder
  basestep <- x[2]-x[1] # step size in x
  
  # x              : evenly spaced array
  # minfac, maxfac : array step size will be in [minfac*basestep,maxfac*basestep]
  # mode           : fast-slow, slow-fast, fast-slow-fast
  # c              : if fast-slow-fast, slow will be at x==c
  # hw             : normalized distance = x-distance / hw
  # mustinclude    : values that must be included once in output array
  # addvalues      : values that will be added to output array
  
  # get log base
  base <- (1/.01)^(1/(maxfac-minfac))
  # minfac-log(.01, base=base)   = maxfac [fs: maxfac reached at relative distance .01]
  # minfac-log(1-.99, base=base) = maxfac [sf: maxfac reached at relative distance .99]
  
  # initiate input array, direction, output array
  if (mode=="fs"){
    xin        <- x
    direction  <- +1
    xout       <- c()
  } else if (mode=="sf"){
    xin        <- rev(x)
    direction  <- -1
    xout       <- c()
  } else if (mode=="fsf"){
    xin1       <- x[x<=c]
    xin2       <- rev(x[c<=x])
    direction1 <- +1
    direction2 <- -1
    xout1      <- c()
    xout2      <- c()
  }
  
  # get output array
  if (mode=="fs" | mode=="sf"){
    # initiate current position tracker
    tracker <- xin[1]
    
    while(TRUE){
      # add latest value to output array
      xout <- c(xout, tracker)
      # absolute distance
      distabs <- abs(tracker - xin[length(xin)])
      # relative distance (max 1)
      distrel <- min(distabs / hw, 1)
      # get step factor
      stepfac <- min(minfac - log(1-distrel, base=base), maxfac)
      # get step
      step <- stepfac * basestep
      # move tracker in right direction
      tracker <- tracker + direction * step
      # break once finished
      if (mode=="fs"){
        if (tracker > max(xin)){
          xout <- sort(unique(c(xout, mustinclude))) # add mustinclude values
          xout <- sort(c(xout, addvalues))           # addvalues
          break
        }
      } else if (mode=="sf"){
        if (tracker < min(xin)){
          xout <- sort(unique(c(xout, mustinclude))) # add mustinclude values
          xout <- sort(c(xout, addvalues))           # addvalues
          break
        }
    }
    
  }
  } else if (mode=="fsf"){
    tracker1 <- xin1[1]
    tracker2 <- xin2[1]
    
    while(TRUE){
      # add latest value to output array
      xout1 <- c(xout1, tracker1)
      xout2 <- c(xout2, tracker2)
      # absolute distance
      distabs1 <- abs(tracker1 - c)
      distabs2 <- abs(tracker2 - c)
      # relative distance (max 1)
      distrel1 <- min(distabs1 / hw, 1)
      distrel2 <- min(distabs2 / hw, 1)
      # get step factor
      stepfac1 <- min(minfac - log(1-distrel1, base=base), maxfac)
      stepfac2 <- min(minfac - log(1-distrel2, base=base), maxfac)
      # get step
      step1 <- stepfac1 * basestep
      step2 <- stepfac2 * basestep
      # move tracker in right direction
      if(tracker1 > max(xin1)) NULL else tracker1 <- tracker1 + direction1 * step1
      if(tracker2 < min(xin2)) NULL else tracker2 <- tracker2 + direction2 * step2
      # break once finished
      if(tracker1 > max(xin1) & tracker2 < min(xin2)){
        xout <- sort(unique(c(mustinclude, xout1, xout2)))
        xout <- sort(c(xout, addvalues))
        break
      }
    }
  }
  return(xout)
}

