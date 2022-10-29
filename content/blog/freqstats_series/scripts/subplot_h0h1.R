library(here)
library(tidyverse)
library(patchwork)
library(gdata)

source(here("content", "blog", "plotsetup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "setup.R"))

# ---

limsx <- c(-3,3)
limsy <- c(0,.5)
recth <- .1
colh0 <- colorlist[[2]]
colh1 <- "gray50"

sub.h0h1.1a <- ggplot() + theme_x1() +
  ax(xl = limsx, yl = limsy, xb = c(0), xt = expression(mu[0]), r=1.25) +
  annotate("rect", xmin=0, xmax=10, ymin=0, ymax=recth, color=NA, fill=colh1, alpha=0.6) +
  annotate("rect", xmin=-10, xmax=0, ymin=0, ymax=recth, color=NA, fill=colh0, alpha=0.6) +
  annotate("text", hjust=.5, vjust=0, x=-1.5, y=recth+.05, label=expression(H[0]), size=3, color=colh0) +
  annotate("text", hjust=.5, vjust=0, x=1.5, y=recth+.05, label=expression(H[1]), size=3, color=colh1) +
  annotate("segment", x=0, xend=0, y=0, yend=recth, size=1, color=colh0) +
  ggtitle(expression(paste(bold("[1a]   "), H[0],": ",mu<=mu[0],",  ",H[1],": ",mu>mu[0])))

sub.h0h1.1b <- ggplot() + theme_x1() +
  ax(xl = limsx, yl = limsy, xb = c(0), xt = expression(mu[0]), r=1.25) +
  annotate("rect", xmin=-10, xmax=0, ymin=0, ymax=recth, color=NA, fill=colh1, alpha=0.6) +
  annotate("rect", xmin=0, xmax=10, ymin=0, ymax=recth, color=NA, fill=colh0, alpha=0.6) +
  annotate("text", hjust=.5, vjust=0, x=1.5, y=recth+.05, label=expression(H[0]), size=3, color=colh0) +
  annotate("text", hjust=.5, vjust=0, x=-1.5, y=recth+.05, label=expression(H[1]), size=3, color=colh1) +
  annotate("segment", x=0, xend=0, y=0, yend=recth, size=1, color=colh0) +
  ggtitle(expression(paste(bold("[1b]   "), H[0],": ",mu>=mu[0],",  ",H[1],": ",mu<mu[0])))

sub.h0h1.2 <- ggplot() + theme_x1() +
  ax(xl = limsx, yl = limsy, xb = c(0), xt = expression(mu[0]), r=1.25) +
  annotate("rect", xmin=-10, xmax=10, ymin=0, ymax=recth, color=NA, fill=colh1, alpha=0.6) +
  annotate("text", hjust=.5, vjust=0, x=0, y=recth+.05, label=expression(H[0]), size=3, color=colh0) +
  annotate("text", hjust=.5, vjust=0, x=-1.5, y=recth+.05, label=expression(H[1]), size=3, color=colh1) +
  annotate("text", hjust=.5, vjust=0, x=1.5, y=recth+.05, label=expression(H[1]), size=3, color=colh1) +
  annotate("segment", x=0, xend=0, y=0, yend=recth, size=1.5, color=colh0) +
  ggtitle(expression(paste(bold("[2]   "), H[0],": ",mu==mu[0],",  ",H[1],": ",mu!=mu[0]))) 

rm(limsx, limsy, recth, colh0)
