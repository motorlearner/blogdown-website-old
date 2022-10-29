rm(list=ls())
library(here)
library(tidyverse)
library(patchwork)
library(svglite)

source(here("content", "blog", "plotsetup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "setup.R"))

set.seed(111)

k <- 3000

x <- replicate(3, dplyr::cummean(rbinom(k, 1, .5)))


p <- data.frame(x=x, i=1:k) |>
  pivot_longer(starts_with("x"),
               values_to = "p",
               names_to = "s",
               names_prefix = "x.") |>
  
  ggplot(aes(x=i, y=p, group=s)) +
  ax(xl=c(1,k), xb=seq(1000,k,1000), xt=paste0(1:(k/1000),"k"), xname="# Coin Tosses",
     yl=c(0,1), yb=c(0,.5,1), yt=c("0","0.5","1"), yname="Frequency Heads") +
  theme_x2y2() +
  geom_line(size=.5) +
  theme(plot.margin=margin(t=5,r=5,b=2,l=2))

savesvg(plot=p, filename="coin", w=7, h=3)

    
