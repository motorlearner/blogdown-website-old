library(here)
library(tidyverse)
library(patchwork)

source(here("content", "blog", "plotsetup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "setup.R"))

# ---

sub.sample1 <- ggplot() + theme_x1() +
  ax(xl=c(-3,3), xb=c(0,mean(sample1)), xt=expression(mu[0],m), yl=c(0,1), r=.5) +
  geom_jitter(aes(x=sample1, y=.5), height=.1, shape=21, size=1.5, color=color$grayd, fill=color$grayd) +
  geom_vline(xintercept=mean(sample1)) +
  geom_point(aes(x=mean(sample1),y=.5), shape=22, size=2.5, color=colorlist[[1]], fill=colorlist[[1]])

sub.sample2 <- ggplot() + theme_x1() +
  ax(xl=c(-3,3), xb=c(0,mean(sample2)), xt=expression(mu[0],m), yl=c(0,1), r=.5) +
  geom_jitter(aes(x=sample2, y=.5), height=.1, shape=21, size=1.5, color=color$grayd, fill=color$grayd) +
  geom_vline(xintercept=mean(sample2)) +
  geom_point(aes(x=mean(sample2),y=.5), shape=22, size=2.5, color=colorlist[[2]], fill=colorlist[[2]])

sub.sample3 <- ggplot() + theme_x1() +
  ax(xl=c(-3,3), xb=c(0,mean(sample3)), xt=expression(mu[0],m), yl=c(0,1), r=.5) +
  geom_jitter(aes(x=sample3, y=.5), height=.1, shape=21, size=1.5, color=color$grayd, fill=color$grayd) +
  geom_vline(xintercept=mean(sample3)) +
  geom_point(aes(x=mean(sample1),y=.5), shape=22, size=2.5, color=colorlist[[3]], fill=colorlist[[3]])

sub.sample.list <- list(sub.sample1, sub.sample2, sub.sample3)
