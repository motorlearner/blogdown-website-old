rm(list=ls())

library(here)
library(tidyverse)
library(patchwork)
library(svglite)

source(here("content", "blog", "plotsetup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "setup.R"))


p <- den |> 
  mutate(x = x+1.5) |> 
  ggplot(aes(x=x, y=y)) +
  theme_x1() + ax(xl=c(0,6), xb=1.5, xt="x", yl=c(0,1.55), r=1.25) +
  geom_area(color=colorlist[[2]], fill=colorlist[[2]]) +
  geom_vline(xintercept=1.5, linetype="dashed") +
  annotate("text", vjust=.5, hjust=0, x=2.3, y=1.0, size=3.5,
           label=as.expression(bquote(bold("If")~~mu==x~","))) +
  annotate("text", vjust=.5, hjust=0, x=2.3, y=0.8, size=3.5,
           label=as.expression(bquote(bold("then")~italic("this")~"is how"~~m~~"would be distributed,"))) +
  annotate("text", vjust=.5, hjust=0, x=2.3, y=0.6, size=3.5,
           label=as.expression(bquote("in the long run."))) 
  
savesvg(plot=p, filename="sampdistexplicit", h=3, w=7)
