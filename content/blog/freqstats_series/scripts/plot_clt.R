rm(list=ls())
library(here)
library(tidyverse)
library(patchwork)
library(svglite)

source(here("content", "blog", "plotsetup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "setup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_sample.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_alldistsmu0.R"))

set.seed(2020)

howmany <- 80

dflist <- list()
p.samples <- list()
p.sample <- list()
p.dists <- list()

for (i in 1:3) {
  dflist[[i]] <- data.frame(k=1:howmany, x=rnorm(nvec[i]*howmany)) |>
    arrange(k) |> 
    group_by(k) |> 
    mutate(m=mean(x)) |> 
    ungroup()
}

for (i in 1:3) {
  
  dat <- data.frame(x=rnorm(nvec[i]), y=.5) |> mutate(m=mean(x)) 
  
  p.sample[[i]] <- dat |> 
    ggplot(aes(x=x, y=y)) +
    theme_x1() +
    ax(xl=c(-3,3), xb=c(0), xt=expression(mu), yl=c(0,1)) +
    geom_jitter(aes(x=x), shape=16, color=color$grayd, height=.15) +
    geom_point(aes(x=m), shape=15, color=colorlist[[i]], size=2) +
    ggtitle(as.expression(bquote(N==.(nvec[i])))) +
    theme(plot.title=element_text(hjust=.5))
  
  p.samples[[i]] <- dflist[[i]] |> 
    ggplot(aes(y=k, group=k)) +
    theme_x1() +
    ax(xl=c(-3,3), xb=0, xt=expression(mu),
       yl=c(0,howmany+1)) +
    geom_point(aes(x=x), shape=16, color=color$grayd, size=.5) +
    geom_vline(xintercept=0, linetype="dashed") +
    geom_point(aes(x=m), shape=15, color=colorlist[[i]], size=.8) +
    ggtitle(as.expression(bquote(N==.(nvec[i])))) +
    theme(plot.title=element_text(hjust=.5))
  
  p.dists[[i]] <- sub.alldistmu0.list[[i]] +
    ax(xl=c(-3,3), yl=c(0,2.83), xb=0, xt=expression(mu), r=1.25) +
    geom_vline(xintercept=0, linetype="dashed") +
    theme(plot.title=element_text(hjust=.5)) +
    ggtitle(as.expression(bquote(N==.(nvec[i])))) +
    theme(plot.title=element_text(hjust=.5))
}

p.one <- p.sample[[1]] + p.sample[[2]] + p.sample[[3]] + plot_layout(ncol=3)
p.many <- p.samples[[1]] + p.samples[[2]] + p.samples[[3]] + plot_layout(ncol=3)
p.sdis <- p.dists[[1]] + p.dists[[2]] + p.dists[[3]] + plot_layout(ncol=3)

savesvg(plot=p.one, filename="clt_onesample", w=8, h=1.4)
savesvg(plot=p.many, filename="clt_manysamples", w=8, h=8)
savesvg(plot=p.sdis, filename="clt_sampdist", w=8, h=2.5)
