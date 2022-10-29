rm(list=ls())
library(here)
library(tidyverse)
library(patchwork)
library(svglite)

source(here("content", "blog", "plotsetup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "setup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_h0h1.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_reddistmu0.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_alpha.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_ratefunc.R"))
source(here("content", "blog", "freqstats_series", "scripts", "fun_gifsec.R"))

gifdir  <- here(plotdir, "gif_rejrate_h0")
fps     <- 20
sec     <- 4
nframes <- fps*sec+1 # +1 -> odd -> includes a frame at mu0

xvec1a <- seq(-3, 0, .01) # for computing rejection rate
xvec1b <- seq(0, +3, .01)
xvec2  <- 0

cut1a <- qnorm(.95, 0, sem)
cut1b <- qnorm(.05, 0, sem)
cut2  <- qnorm(c(.025,.975), 0, sem)

rej1a <- pnorm(cut1a, xvec1a, sem, lower.tail=FALSE)
rej1b <- pnorm(cut1b, xvec1b, sem, lower.tail=TRUE)
rej2  <- pnorm(cut2[1], xvec2, sem, lower.tail=TRUE) + 
  pnorm(cut2[2], xvec2, sem, lower.tail=FALSE)

# values to place sampling distribution on
xrun_init_1a <- seq(-3, 0, length.out=nframes)
xrun_init_1b <- seq(0, +1, length.out=nframes)
xrun_init_2  <- 0
xrun1a <- gifsec(xrun_init_1a, minfac=.05, mode="fs", halfwidth=2*sem, addvalues=rep(0,20))
xrun1b <- -xrun1a
xrun2  <- xrun_init_2

rejrun1a <- pnorm(cut1a, xrun1a, sem, lower.tail=FALSE)
rejrun1b <- pnorm(cut1b, xrun1b, sem, lower.tail=TRUE)
rejrun2  <- pnorm(cut2[1], xrun2, sem, lower.tail=TRUE) + 
  pnorm(cut2[2], xrun2, sem, lower.tail=FALSE)

l <- "
ABC
DEF
GHI
JKL
"

p.dist2 <- sub.reddistmu0 %+%
  (den |> mutate(
    x = x + xrun2,
    l = ifelse(x <= cut2[1], x, NA_real_),
    r = ifelse(x >= cut2[2], x, NA_real_)
  )) + 
  geom_area(aes(x=l, y=y), color=color$grayd, fill=color$grayd) +
  geom_area(aes(x=r, y=y), color=color$grayd, fill=color$grayd) +
  geom_vline(xintercept=cut2) +
  geom_vline(xintercept=xrun2, linetype="dashed") +
  annotate_rate(as.expression(bquote("RR"==.(format(round(rejrun2,2),nsmall=2)))))

p.rr2 <- sub.rrfunc %+%
  (data.frame(x=xvec2, y=rej2)) +
  #geom_line(aes(x=x,y=y), color=color$grayd, size=2) +
  geom_point(aes(x=0,y=.05), shape=21, size=2, color=color$grayd, fill=color$grayd) +
  geom_vline(xintercept=xrun2, linetype="dashed") +
  geom_hline(yintercept=.05, linetype="dotted") +
  annotate_alphaetc(0.05, as.expression(bquote(alpha==0.05)))

# before running, delete all png's in directory
file.remove(list.files(path=gifdir, pattern=".*png$", full.names=TRUE))

for (i in seq_along(xrun1a)) {
  
  if (xrun1a[i]==0){
    rrnote1a <- as.expression(bquote("RR"==.(format(round(rejrun1a[i],2),nsmall=2))))
  } else {
    rrnote1a <- as.expression(bquote("RR"%~~%.(format(round(rejrun1a[i],2),nsmall=2))))
  }
  if (xrun1b[i]==0){
    rrnote1b <- as.expression(bquote("RR"==.(format(round(rejrun1b[i],2),nsmall=2))))
  } else {
    rrnote1b <- as.expression(bquote("RR"%~~%.(format(round(rejrun1b[i],2),nsmall=2))))
  }
  
  p.dist1a <- sub.reddistmu0 %+%
    (den |> mutate(
      x = x + xrun1a[i],
      r = ifelse(x >= cut1a, x, NA_real_)
    )) + 
    geom_area(aes(x=r, y=y), color=color$grayd, fill=color$grayd) +
    geom_vline(xintercept=cut1a) +
    geom_vline(xintercept=xrun1a[i], linetype="dashed") +
    annotate_rate(rrnote1a)
  
  p.dist1b <- sub.reddistmu0 %+%
    (den |> mutate(
      x = x + xrun1b[i],
      l = ifelse(x <= cut1b, x, NA_real_)
    )) + 
    geom_area(aes(x=l, y=y), color=color$grayd, fill=color$grayd) +
    geom_vline(xintercept=cut1b) +
    geom_vline(xintercept=xrun1b[i], linetype="dashed") +
    annotate_rate(rrnote1b)
  
  p.rr1a <- sub.rrfunc %+%
    (data.frame(x=xvec1a, y=rej1a)) +
    geom_line(aes(x=x,y=y), color=color$grayd, size=2) +
    geom_point(aes(x=0,y=.05), shape=21, size=2, color=color$grayd, fill=color$grayd) +
    geom_vline(xintercept=xrun1a[i], linetype="dashed") +
    geom_hline(yintercept=.05, linetype="dotted") +
    annotate_alphaetc(0.05, as.expression(bquote(alpha==0.05)))
  
  p.rr1b <- sub.rrfunc %+%
    (data.frame(x=xvec1b, y=rej1b)) +
    geom_line(aes(x=x,y=y), color=color$grayd, size=2) +
    geom_point(aes(x=0,y=.05), shape=21, size=2, color=color$grayd, fill=color$grayd) +
    geom_vline(xintercept=xrun1b[i], linetype="dashed") +
    geom_hline(yintercept=.05, linetype="dotted") +
    annotate_alphaetc(0.05, as.expression(bquote(alpha==0.05)))
  
  p <- sub.h0h1.1a + sub.h0h1.1b + sub.h0h1.2 +
    sub.alpha1a + sub.alpha1b + sub.alpha2 +
    p.dist1a + p.dist1b + p.dist2 +
    p.rr1a + p.rr1b + p.rr2 +
    plot_layout(design=l)
  
  savepng(plot=p, filename=paste0("rejrateH0", sprintf("%03d", i)), w=12, h=6.5, directory = gifdir)
  
  cat("\r", toString(i), "/", length(xrun1a))
}

library(gifski)
pngs <- list.files(path=gifdir, pattern=".*png$", full.names=TRUE)
gifski(pngs, gif_file=paste0(plotdir, "/rejrateH0.gif"), delay=1/fps, width=1200, height=650)