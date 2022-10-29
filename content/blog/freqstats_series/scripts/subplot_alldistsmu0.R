library(here)
library(tidyverse)
library(patchwork)

source(here("content", "blog", "plotsetup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "setup.R"))

# ---

sub.alldistmu0.list <- list()
sub.alldistmu0.m.list <- list()

for(i in 1:3){
  
  sub.alldistmu0.list[[i]] <- denlist[[i]] |> ggplot() + theme_x1() +
    ax(xl=c(-3,3), xb=0, xt=expression(mu[0]), yl=c(0,2.83), r=1.25) +
    geom_area(aes(x=x, y=y), color=colorlist[[i]], fill=colorlist[[i]])
  
  sub.alldistmu0.m.list[[i]] <- denlist[[i]] |> ggplot() + theme_x1() +
    ax(xl=c(-3,3), xb=c(0,mean(samplelist[[i]])), xt=expression(mu[0],m), yl=c(0,2.83), r=1.25) +
    geom_area(aes(x=x, y=y), color=colorlist[[i]], fill=colorlist[[i]])
}

# function to annotate rate 
annotate_rate <- function(e){
  annotate("label", label.size=NA, size=2.25, label.padding=unit(0.1,"lines"), 
           fill=color$grayd, vjust=.5, x=3, y=.8, hjust=1, label=e)
}