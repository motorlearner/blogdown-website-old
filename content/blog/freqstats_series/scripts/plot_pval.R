rm(list=ls())

library(here)
library(tidyverse)
library(patchwork)
library(svglite)

source(here("content", "blog", "plotsetup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "setup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_reddistmu0.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_sample.R"))

l <- "
ABC
DEF
GHI
"

pval1a <- pnorm(mean(sample2),0,sem,lower.tail=FALSE) |> round(2)
pval1b <- pnorm(mean(sample2),0,sem,lower.tail=TRUE)  |> round(2)
pval2  <- min(c(pval1a, pval1b))*2 |> round(2)

p.pval1a <- sub.reddistmu0.m +
  geom_area(aes(x=ifelse(x>=mean(sample2),x,NA), y=y), color=color$grayd, fill=color$grayd) +
  geom_vline(xintercept=mean(sample2)) +
  geom_vline(xintercept=0, linetype="dashed") +
  annotate_rate(parse(text=paste0("p%~~%",pval1a)))

p.pval1b <- sub.reddistmu0.m +
  geom_area(aes(x=ifelse(x<=mean(sample2),x,NA), y=y), color=color$grayd, fill=color$grayd) +
  geom_vline(xintercept=mean(sample2)) +
  geom_vline(xintercept=0, linetype="dashed") +
  annotate_rate(parse(text=paste0("p%~~%",pval1b))) 

p.pval2 <- sub.reddistmu0.m +
  geom_area(aes(x=ifelse(x>=mean(sample2),x,NA), y=y), color=color$grayd, fill=color$grayd) +
  geom_area(aes(x=ifelse(x<=-mean(sample2),x,NA), y=y), color=color$grayd, fill=color$grayd) +
  geom_vline(xintercept=mean(sample2)) +
  geom_vline(xintercept=0, linetype="dashed") +
  annotate_rate(parse(text=paste0("p%~~%",pval2)))

p <- sub.h0h1.1a + sub.h0h1.1b + sub.h0h1.2 + 
  sub.sample2 + sub.sample2 + sub.sample2 +
  p.pval1a + p.pval1b + p.pval2 + 
  plot_layout(design=l)

savesvg(plot=p, filename="pval", w=10, h=4)

           