rm(list=ls())

library(here)
library(tidyverse)
library(patchwork)
library(svglite)

source(here("content", "blog", "plotsetup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "setup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_h0h1.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_reddistmu0.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_sample.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_ratefunc.R"))
source(here("content", "blog", "freqstats_series", "scripts", "fun_gifsec.R"))

gifdir  <- here(plotdir, "gif_pvalfunc")
fps     <- 20
sec     <- 8
nframes <- fps*sec

xvec <- seq(-3, 3, .01) # for computing p-value function

pfun1a <- pnorm(mean(sample2), xvec, sem, lower.tail=FALSE)
pfun1b <- 1-pfun1a
pfun2  <- pmin(pfun1a, pfun1b) * 2

xrun_init <- seq(-3, 3, length.out=nframes)
xrun <- gifsec(xrun_init, minfac=.05, mode="fsf", slowestat=mean(sample2), halfwidth=2*sem)

prun1a <- pnorm(mean(sample2), xrun, sem, lower.tail=FALSE) |> round(2)
prun1b <- 1-prun1a
prun2  <- pmin(prun1a, prun1b) * 2

l <- "
ABC
DEF
GHI
JKL
"

# before running, delete all png's in directory
file.remove(list.files(path=gifdir, pattern=".*png$", full.names=TRUE))

for (i in seq_along(xrun)) {
  
  set.seed(2020)

  p.pval1a <- sub.reddistmu0.m %+% (den |> mutate(x = x+xrun[i])) +
    geom_area(aes(x=ifelse(x>=mean(sample2),x,NA_real_), y=y), color=color$grayd, fill=color$grayd) +
    geom_vline(xintercept=mean(sample2)) +
    geom_vline(xintercept=xrun[i], linetype="dashed") +
    annotate_rate(as.expression(bquote(p%~~%.(format(prun1a[i], nsmall=2)))))
  
  p.pval1b <- sub.reddistmu0.m %+% (den |> mutate(x = x+xrun[i])) +
    geom_area(aes(x=ifelse(x<=mean(sample2),x,NA_real_), y=y), color=color$grayd, fill=color$grayd) +
    geom_vline(xintercept=mean(sample2)) +
    geom_vline(xintercept=xrun[i], linetype="dashed") +
    annotate_rate(as.expression(bquote(p%~~%.(format(prun1b[i], nsmall=2)))))
  
  dist <- abs(xrun[i]-mean(sample2))
  
  p.pval2 <- sub.reddistmu0.m %+% (den |> mutate(x = x+xrun[i])) +
    geom_area(aes(x=ifelse(x<=xrun[i]-dist,x,NA_real_), y=y), color=color$grayd, fill=color$grayd) +
    geom_area(aes(x=ifelse(x>=xrun[i]+dist,x,NA_real_), y=y), color=color$grayd, fill=color$grayd) +
    geom_vline(xintercept=mean(sample2)) +
    geom_vline(xintercept=xrun[i], linetype="dashed") +
    annotate_rate(as.expression(bquote(p%~~%.(format(prun2[i], nsmall=2)))))
  
  p.pfunc1a <- sub.pfunc + 
    geom_line(aes(x=xvec, y=pfun1a), color=color$grayd, size=2) +
    geom_vline(xintercept=xrun[i], linetype="dashed")
  
  p.pfunc1b <- sub.pfunc + 
    geom_line(aes(x=xvec, y=pfun1b), color=color$grayd, size=2) +
    geom_vline(xintercept=xrun[i], linetype="dashed") 
  
  p.pfunc2 <- sub.pfunc + 
    geom_line(aes(x=xvec, y=pfun2), color=color$grayd, size=2) +
    geom_vline(xintercept=xrun[i], linetype="dashed")
  
  p <- sub.h0h1.1a + sub.h0h1.1b + sub.h0h1.2 +
    sub.sample2 + sub.sample2 + sub.sample2 +
    p.pval1a + p.pval1b + p.pval2 +
    p.pfunc1a + p.pfunc1b + p.pfunc2 +
    plot_layout(design=l)
  
  savepng(plot=p, filename=paste0("pfunc", sprintf("%03d", i)), w=11, h=6, directory = gifdir)
  
  cat("\r", toString(i), "/", length(xrun))
}

library(gifski)
pngs <- list.files(path=gifdir, pattern=".*png$", full.names=TRUE)
gifski(pngs, gif_file=paste0(plotdir, "/pvalfunc.gif"), delay=1/fps, width=1100, height=600)



  


