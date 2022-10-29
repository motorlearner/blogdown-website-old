gifsec <- function(
    x,
    minfac = .1,        # min factor by which step will be scaled
    maxfac = 1,         # max factor by which step will be scaled
    mode   = "fs",      # fast-slow "fs", slow-fast "sf", fast-slow-fast "fsf"
    slowestat = NULL,   # if "fsf", where should be the slowest point
    halfwidth = NULL,   # range over which slow-down and/or speed-up should occur
    mustinclude = NULL, # output array must include these values once
    addvalues = NULL    # output array will include these values
){
  
  # input array step
  basestep <- x[2]-x[1]
  
  # NOTE
  # --- step size will be basestep * stepfac
  # --- stepfac = minfac + (maxfac-minfac) * distrel
  # --- distrel = min(distabs/halfwidth, 1)
  # --- distabs is the current absolute distance to slowest point
  
  # initiate arrays
  xin  <- list(fs = NULL, sf = NULL)
  xout <- list(fs = NULL, sf = NULL)
  
  # NOTE
  # --- sf  ==> array will be used as-is
  # --- fs  ==> array will be used as-is
  # --- fsf ==> array will be split into "fs" and "sf"
  
  # x-value at which speed is slowest
  s <- switch(mode, "fs"=max(x), "sf"=min(x), "fsf"=slowestat)
  
  # fill arrays (if fs or sf array is just copied; if fsf array is split)
  xin[["fs"]] <- x[x<=s]
  xin[["sf"]] <- x[s<=x]
  
  if (length(xin[["fs"]]>0)) fs <- TRUE else fs <- FALSE
  if (length(xin[["sf"]]>0)) sf <- TRUE else sf <- FALSE
  
  # initiate tracker (starts at fast end)
  tracker <- list(fs = NULL, sf = NULL)
  if (fs) tracker[["fs"]] <- xin[["fs"]][1]
  if (sf) tracker[["sf"]] <- rev(xin[["sf"]])[1]
  
  if (fs) while (TRUE) { id <- "fs"
    # add latest value to output array
    xout[[id]] <- c(xout[[id]], tracker[[id]])
    # absolute and relative distance
    distabs <- abs(tracker[[id]] - s)
    distrel <- min(distabs/halfwidth, 1)
    # step factor
    stepfac <- minfac + (maxfac-minfac) * distrel
    # step (positive!)
    newstep <- +1 * stepfac * basestep
    # move tracker
    tracker[[id]] <- tracker[[id]] + newstep
    # break once finished
    if (tracker[[id]] > s) break
  }
  
  if (sf) while (TRUE) { id <- "sf"
  # add latest value to output array
  xout[[id]] <- c(xout[[id]], tracker[[id]])
  # absolute and relative distance
  distabs <- abs(tracker[[id]] - s)
  distrel <- min(distabs/halfwidth, 1)
  # step factor
  stepfac <- minfac + (maxfac-minfac) * distrel
  # step (negative!)
  newstep <- -1 * stepfac * basestep
  # move tracker
  tracker[[id]] <- tracker[[id]] + newstep
  # break once finished
  if (tracker[[id]] < s) break
  }
  
  # add values that must be included once
  y <- unique(c(xout[["sf"]], xout[["fs"]], mustinclude))
  # add values that must be included
  y <- c(y, addvalues)
  # return sorted array
  return(sort(y))
}
