library(here)
library(tidyverse)
library(patchwork)

source(here("content", "blog", "plotsetup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "setup.R"))

# ---

sub.reddistmu0 <- den |> ggplot() + theme_x1() +
  ax(xl=c(-3,3), xb=0, xt=expression(mu[0]), yl=c(0,1.55), r=1.25) +
  geom_area(aes(x=x, y=y), color=colorlist[[2]], fill=colorlist[[2]])

sub.reddistmu0.m <- den |> ggplot() + theme_x1() +
  ax(xl=c(-3,3), xb=c(0,mean(sample2)), xt=expression(mu[0],m), yl=c(0,1.55), r=1.25) +
  geom_area(aes(x=x, y=y), color=colorlist[[2]], fill=colorlist[[2]]) 

# function to annotate rate 
annotate_rate <- function(e){
  annotate("label", label.size=NA, size=2.25, label.padding=unit(0.1,"lines"), 
             fill=color$grayd, vjust=.5, x=3, y=.8, hjust=1, label=e)
}

# function to annotate h0 true/false
annotate_true <- function(e){
  annotate("label", label.size=NA, size=2.25, label.padding=unit(0.1,"lines"), 
           fill=NA, vjust=1, x=3, y=1.55, hjust=1, label=e, alpha=0)
}

# how to shift distribution: sub.reddistmu0 %+% (den |> mutate(x=x+1)) 
