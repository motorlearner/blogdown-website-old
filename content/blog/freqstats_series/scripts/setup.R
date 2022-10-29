# make data and functions that are used throughout plotting
# .........................................................
source(here::here("content","blog","plotsetup.R"))

set.seed(2020)

# pop sd
sigma <- 1

# vector of n and SEM
nvec   <- c(5,15,50)
semvec <- sigma/sqrt(nvec)

# highlighted n and SEM
n   <- nvec[2]
sem <- semvec[2]

# vector of alpha
alpha <- c(.15, .1, .05)

# sampling distribution densities
denlist <- list()
for (i in seq_along(nvec)){
  semi <- semvec[i]
  denlist[[i]] <- data.frame(x=seq(-3*semi,+3*semi, .01),
                             y=dnorm(seq(-3*semi,+3*semi, .01), 0, semi))
}; den <- denlist[[2]]

# samples (with sample mean constraint)
targetmean <- 0.5   # target mean
dev        <- .001  # allowed deviation
maxabs     <- 3     # max absolute value

while(TRUE){
  sample1 <- rnorm(nvec[1])
  if(abs(mean(sample1)-targetmean)<dev & max(abs(sample1))<maxabs){
    break
  }
}
while(TRUE){
  sample2 <- rnorm(nvec[2])
  if(abs(mean(sample2)-targetmean)<dev & max(abs(sample2))<maxabs){
    break
  }
}
while(TRUE){
  sample3 <- rnorm(nvec[3])
  if(abs(mean(sample3)-targetmean)<dev & max(abs(sample3))<maxabs){
    break
  }
}
samplelist <- list(sample1, sample2, sample3)
colorlist <- list(color$blue, color$red, color$orange)

# plot directory
library(here)
plotdir <- here("content", "blog", "freqstats_series", "plots")

# save as svg function
library(svglite)
savesvg <- function(plot, filename, h, w, scale=.6, directory=plotdir) {
  ggsave(
    plot     = plot,
    filename = paste0(filename, ".svg"),
    path     = directory,
    height   = h,
    width    = w, 
    scale    = scale
  )
}
# save as png
library(png)
savepng <- function(plot, filename, h, w, scale=.6, directory=plotdir) {
  ggsave(
    plot     = plot,
    filename = paste0(filename, ".png"),
    path     = directory,
    height   = h,
    width    = w, 
    scale    = scale
  )
}
