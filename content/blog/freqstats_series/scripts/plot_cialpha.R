rm(list=ls())
library(here)
library(tidyverse)
library(patchwork)
library(svglite)

source(here("content", "blog", "plotsetup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "setup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_reddistmu0.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_sample.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_ratefunc.R"))

l <- "
ABC
DEF
GHI
JKL
"

xvec  <- seq(-3, 3, .01)
pfun1a <- pnorm(mean(sample2), xvec, sem, lower.tail=FALSE)
pfun1b <- 1-pfun1a
pfun2 <- pmin(pfun1a, pfun1b) * 2

p.sample <- list()
p.cidistalpha.l <- list()
p.cidistalpha.r <- list()
p.pfuncalpha <- list()

cl <- qnorm(matrix(c(alpha/2, 1-alpha/2), ncol=2), mean(sample2), sem)
hw <- qnorm(1-alpha/2, 0, sem)

for(i in 1:3){
  
  p.sample[[i]] <- sub.sample2 +
    ggtitle(as.expression(bquote(alpha==.(alpha[i])))) +
    theme(plot.title = element_text(hjust=.5))
  
  p.cidistalpha.l[[i]] <- sub.reddistmu0.m %+% 
    (den |> mutate(x = x+cl[i,1], 
                   l = ifelse(x<=cl[i,1]-hw[i], x, NA_real_),
                   r = ifelse(x>=mean(sample1), x, NA_real_))) +
    geom_area(aes(x=l, y=y), color=color$grayd, fill=color$grayd) +
    geom_area(aes(x=r, y=y),color=color$grayd, fill=color$grayd) +
    geom_vline(xintercept=cl[i,1], linetype="dashed") +
    geom_vline(xintercept=mean(sample2)) +
    annotate_rate(as.expression(bquote({p==alpha}==.(alpha[i]))))
  
  p.cidistalpha.r[[i]] <- sub.reddistmu0.m %+% 
    (den |> mutate(x = x+cl[i,2],
                   l = ifelse(x>=cl[i,2]+hw[i], x, NA_real_),
                   r = ifelse(x<=mean(sample1), x, NA_real_))) +
    geom_area(aes(x=l, y=y), color=color$grayd, fill=color$grayd) +
    geom_area(aes(x=r, y=y),color=color$grayd, fill=color$grayd) +
    geom_vline(xintercept=cl[i,2], linetype="dashed") +
    geom_vline(xintercept=mean(sample2)) +
    annotate_rate(as.expression(bquote({p==alpha}==.(alpha[i]))))
  
  p.pfuncalpha[[i]] <- sub.pfunc + 
    geom_line(aes(x=xvec, y=pfun2), color=color$grayd, size=2) +
    geom_vline(xintercept=cl[i,], linetype="dashed") +
    geom_hline(yintercept=alpha[i], linetype="dotted") +
    annotate("segment", y=alpha[i], yend=alpha[i], size=1, color=colorlist[[2]],
             x=cl[i,1], xend=cl[i,2]) +
    annotate("text", size=3, label=as.expression(bquote(alpha==.(alpha[i]))),
             hjust=0, vjust=0, x=-2.8, y=alpha[i]+.01)
}

p <- p.sample[[1]] + p.sample[[2]] + p.sample[[3]] +
  p.cidistalpha.l[[1]] + p.cidistalpha.l[[2]] + p.cidistalpha.l[[3]] +
  p.cidistalpha.r[[1]] + p.cidistalpha.r[[2]] + p.cidistalpha.r[[3]] +
  p.pfuncalpha[[1]] + p.pfuncalpha[[2]] + p.pfuncalpha[[3]] +
  plot_layout(design=l)

savesvg(plot=p, filename="ci_alpha", w=11, h=6.5)
