rm(list=ls())
library(here)
library(tidyverse)
library(patchwork)
library(svglite)

source(here("content", "blog", "plotsetup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "setup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_sample.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_ratefunc.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_alldistsmu0.R"))

l <- "
ABC
DEF
GHI
JKL
"

xvec  <- seq(-3, 3, .01)

p.sample <- list()
p.cidistn.l <- list()
p.cidistn.r <- list()
p.pfuncn <- list()

for(i in 1:3){
  mi   <- mean(samplelist[[i]])
  semi <- semvec[i]
  
  pfun1a <- pnorm(mi, xvec, semi, lower.tail=FALSE)
  pfun1b <- 1-pfun1a
  pfun2  <- pmin(pfun1a, pfun1b) * 2
  
  cl <- qnorm(c(.025, .975), mi, semi)
  hw <- qnorm(.975, 0, semi)
  
  p.sample[[i]] <- sub.sample.list[[i]] +
    ggtitle(as.expression(bquote(N==.(nvec[i])))) +
    theme(plot.title = element_text(hjust=.5))
  
  p.cidistn.l[[i]] <- sub.alldistmu0.m.list[[i]] %+% 
    (denlist[[i]] |> mutate(x = x+cl[1], 
                            l = ifelse(x<=cl[1]-hw, x, NA_real_),
                            r = ifelse(x>=mi, x, NA_real_))) +
    geom_area(aes(x=l, y=y), color=color$grayd, fill=color$grayd) +
    geom_area(aes(x=r, y=y),color=color$grayd, fill=color$grayd) +
    geom_vline(xintercept=mi) +
    annotate_rate(as.expression(bquote({p==alpha}==0.05)))
  
  p.cidistn.r[[i]] <- sub.alldistmu0.m.list[[i]] %+% 
    (denlist[[i]] |> mutate(x = x+cl[2],
                            l = ifelse(x>=cl[2]+hw, x, NA_real_),
                            r = ifelse(x<=mi, x, NA_real_))) +
    geom_area(aes(x=l, y=y), color=color$grayd, fill=color$grayd) +
    geom_area(aes(x=r, y=y),color=color$grayd, fill=color$grayd) +
    geom_vline(xintercept=mi) +
    annotate_rate(as.expression(bquote({p==alpha}==0.05)))
  
  p.pfuncn[[i]] <- sub.pfunc %+%
    (data.frame(x=xvec, y=pfun2)) +
    geom_line(aes(x=x, y=y), color=color$grayd, size=2) +
    geom_vline(xintercept=cl, linetype="dashed") +
    geom_hline(yintercept=.05, linetype="dotted") +
    annotate("segment", y=.05, yend=.05, size=1, color=colorlist[[i]],
             x=cl[1], xend=cl[2]) +
    annotate("text", size=3, label=as.expression(bquote(alpha==0.05)),
             hjust=0, vjust=0, x=-2.8, y=.05+.01)
  
}

p <- p.sample[[1]] + p.sample[[2]] + p.sample[[3]] +
  p.cidistn.l[[1]] + p.cidistn.l[[2]] + p.cidistn.l[[3]] +
  p.cidistn.r[[1]] + p.cidistn.r[[2]] + p.cidistn.r[[3]] +
  p.pfuncn[[1]] + p.pfuncn[[2]] + p.pfuncn[[3]] +
  plot_layout(design=l)

savesvg(plot=p, filename="ci_n", w=11, h=8)