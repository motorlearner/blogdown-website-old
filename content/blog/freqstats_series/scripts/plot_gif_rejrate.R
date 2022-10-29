rm(list=ls())
library(here)
library(tidyverse)
library(patchwork)
library(svglite)

source(here("content", "blog", "plotsetup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "setup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_h0h1.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_alpha.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_reddistmu0.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_ratefunc.R"))
source(here("content", "blog", "freqstats_series", "scripts", "fun_gifsec.R"))

gifdir  <- here(plotdir, "gif_rejrate")
fps     <- 20
sec     <- 10
nframes <- fps*sec+1 # +1 -> odd -> includes a frame at mu0

xvec <- seq(-3, 3, .01) # for computing rejection rate

cut1a <- qnorm(.95, 0, sem)
cut1b <- qnorm(.05, 0, sem)
cut2  <- qnorm(c(.025,.975), 0, sem)

rej1a <- pnorm(cut1a, xvec, sem, lower.tail=FALSE)
rej1b <- pnorm(cut1b, xvec, sem, lower.tail=TRUE)
rej2  <- pnorm(cut2[1], xvec, sem, lower.tail=TRUE) + 
  pnorm(cut2[2], xvec, sem, lower.tail=FALSE)

# values to place sampling distribution on
xrun_init <- seq(-3, 3, length.out=nframes)
xrun <- gifsec(xrun_init, minfac=.1, mode="fsf", slowestat=0, halfwidth=1.5*sem); length(xrun)

rejrun1a <- pnorm(cut1a, xrun, sem, lower.tail=FALSE)
rejrun1b <- pnorm(cut1b, xrun, sem, lower.tail=TRUE)
rejrun2  <- pnorm(cut2[1], xrun, sem, lower.tail=TRUE) + 
  pnorm(cut2[2], xrun, sem, lower.tail=FALSE)

l <- "
ABC
DEF
GHI
JKL
"

# before running, delete all png's in directory
file.remove(list.files(path=gifdir, pattern=".*png$", full.names=TRUE))

for (i in seq_along(xrun)) {
  
  p.dist1a <- sub.reddistmu0 %+%
    (den |> mutate(
      x = x + xrun[i],
      r = ifelse(x >= cut1a, x, NA_real_)
     )) + 
    geom_area(aes(x=r, y=y), color=color$grayd, fill=color$grayd) +
    geom_vline(xintercept=cut1a) +
    geom_vline(xintercept=xrun[i], linetype="dashed") +
    annotate_rate(as.expression(bquote("RR"%~~%.(format(round(rejrun1a[i],2),nsmall=2)))))
  
  p.dist1b <- sub.reddistmu0 %+%
    (den |> mutate(
      x = x + xrun[i],
      l = ifelse(x <= cut1b, x, NA_real_)
    )) + 
    geom_area(aes(x=l, y=y), color=color$grayd, fill=color$grayd) +
    geom_vline(xintercept=cut1b) +
    geom_vline(xintercept=xrun[i], linetype="dashed") +
    annotate_rate(as.expression(bquote("RR"%~~%.(format(round(rejrun1b[i],2),nsmall=2)))))
  
  p.dist2 <- sub.reddistmu0 %+%
    (den |> mutate(
      x = x + xrun[i],
      l = ifelse(x <= cut2[1], x, NA_real_),
      r = ifelse(x >= cut2[2], x, NA_real_)
    )) + 
    geom_area(aes(x=l, y=y), color=color$grayd, fill=color$grayd) +
    geom_area(aes(x=r, y=y), color=color$grayd, fill=color$grayd) +
    geom_vline(xintercept=cut2) +
    geom_vline(xintercept=xrun[i], linetype="dashed") +
    annotate_rate(as.expression(bquote("RR"%~~%.(format(round(rejrun2[i],2),nsmall=2)))))
  
  p.rr1a <- sub.rrfunc %+%
    (data.frame(x=xvec, y=rej1a)) +
    geom_line(aes(x=x,y=y), color=color$grayd, size=2) +
    geom_vline(xintercept=xrun[i], linetype="dashed") +
    geom_hline(yintercept=.05, linetype="dotted") +
    annotate("text", label=as.expression(bquote(alpha==0.05)),
             size=3, hjust=0, vjust=0, x=-2.8, y=.05+.01)
  
  p.rr1b <- sub.rrfunc %+%
    (data.frame(x=xvec, y=rej1b)) +
    geom_line(aes(x=x,y=y), color=color$grayd, size=2) +
    geom_vline(xintercept=xrun[i], linetype="dashed") +
    geom_hline(yintercept=.05, linetype="dotted") +
    annotate("text", label=as.expression(bquote(alpha==0.05)),
             size=3, hjust=0, vjust=0, x=-2.8, y=.05+.01)
  
  p.rr2 <- sub.rrfunc %+%
    (data.frame(x=xvec, y=rej2)) +
    geom_line(aes(x=x,y=y), color=color$grayd, size=2) +
    geom_vline(xintercept=xrun[i], linetype="dashed") +
    geom_hline(yintercept=.05, linetype="dotted") +
    annotate("text", label=as.expression(bquote(alpha==0.05)),
             size=3, hjust=0, vjust=0, x=-2.8, y=.05+.01)
  
  p <- sub.h0h1.1a + sub.h0h1.1b + sub.h0h1.2 +
    sub.alpha1a + sub.alpha1b + sub.alpha2 +
    p.dist1a + p.dist1b + p.dist2 +
    p.rr1a + p.rr1b + p.rr2 +
    plot_layout(design=l)
  
  savepng(plot=p, filename=paste0("rejrate", sprintf("%03d", i)), w=12, h=6.5, directory = gifdir)
  
  cat("\r", toString(i), "/", length(xrun))
}

library(gifski)
pngs <- list.files(path=gifdir, pattern=".*png$", full.names=TRUE)
gifski(pngs, gif_file=paste0(plotdir, "/rejrate.gif"), delay=1/fps, width=1200, height=650)