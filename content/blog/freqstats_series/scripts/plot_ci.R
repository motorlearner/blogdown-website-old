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
MNO
"

xvec <- seq(-3, 3, .01)
pfun1a <- pnorm(mean(sample2), xvec, sem, lower.tail=FALSE)
pfun1b <- 1-pfun1a
pfun2  <- pmin(pfun1a, pfun1b) * 2

cl1a <- qnorm(.05, mean(sample2), sem)
cl1b <- qnorm(.95, mean(sample2), sem)
cl2  <- qnorm(c(.025,.975), mean(sample2), sem)
ciwidth <- abs(cl2[2]-cl2[1])

p.cidist1a <- sub.reddistmu0.m %+% (den |> mutate(x = x+cl1a)) +
  geom_area(aes(x=ifelse(x>=mean(sample2),x,NA_real_), y=y), 
            color=color$grayd, fill=color$grayd) +
  geom_vline(xintercept=cl1a, linetype="dashed") +
  geom_vline(xintercept=mean(sample2)) +
  annotate_rate(expression({p==alpha}==0.05))

p.cidist1b <- sub.reddistmu0.m %+% (den |> mutate(x = x+cl1b)) +
  geom_area(aes(x=ifelse(x<=mean(sample2),x,NA_real_), y=y), 
            color=color$grayd, fill=color$grayd) +
  geom_vline(xintercept=cl1b, linetype="dashed") +
  geom_vline(xintercept=mean(sample2)) +
  annotate_rate(expression({p==alpha}==0.05))

p.cidist2l <- sub.reddistmu0.m %+% (den |> mutate(x = x+cl2[1])) +
  geom_area(aes(x=ifelse(x>=mean(sample2),x,NA_real_), y=y), 
            color=color$grayd, fill=color$grayd) +
  geom_area(aes(x=ifelse(x<=mean(sample2)-ciwidth,x,NA_real_), y=y),
            color=color$grayd, fill=color$grayd) +
  geom_vline(xintercept=cl2[1], linetype="dashed") +
  geom_vline(xintercept=mean(sample2)) +
  annotate_rate(expression({p==alpha}==0.05))

p.cidist2r <- sub.reddistmu0.m %+% (den |> mutate(x = x+cl2[2])) +
  geom_area(aes(x=ifelse(x<=mean(sample2),x,NA_real_), y=y), 
            color=color$grayd, fill=color$grayd) +
  geom_area(aes(x=ifelse(x>=mean(sample2)+ciwidth,x,NA_real_), y=y),
            color=color$grayd, fill=color$grayd) +
  geom_vline(xintercept=cl2[2], linetype="dashed") +
  geom_vline(xintercept=mean(sample2)) +
  annotate_rate(expression({p==alpha}==0.05))

p.pfunc1a <- sub.pfunc + 
  geom_line(aes(x=xvec, y=pfun1a), color=color$grayd, size=2) +
  geom_vline(xintercept=cl1a, linetype="dashed") +
  geom_hline(yintercept=.05, linetype="dotted") +
  annotate("segment", y=.05, yend=.05, size=1, color=colorlist[[2]],
           x=cl1a, xend=3, arrow=arrow(length=unit(0.4, "lines"), type="closed")) +
  annotate_alphaetc(y=0.05, e=as.expression(bquote(alpha==0.05)))

p.pfunc1b <- sub.pfunc + 
  geom_line(aes(x=xvec, y=pfun1b), color=color$grayd, size=2) +
  geom_vline(xintercept=cl1b, linetype="dashed") +
  geom_hline(yintercept=.05, linetype="dotted") +
  annotate("segment", y=.05, yend=.05, size=1, color=colorlist[[2]],
           x=cl1b, xend=-3, arrow=arrow(length=unit(0.4, "lines"), type="closed")) +
  annotate_alphaetc(y=0.05, e=as.expression(bquote(alpha==0.05)))

p.pfunc2 <- sub.pfunc + 
  geom_line(aes(x=xvec, y=pfun2), color=color$grayd, size=2) +
  geom_vline(xintercept=cl2, linetype="dashed") +
  geom_hline(yintercept=.05, linetype="dotted") +
  annotate("segment", y=.05, yend=.05, size=1, color=colorlist[[2]],
           x=cl2[1], xend=cl2[2]) +
  annotate_alphaetc(y=0.05, e=as.expression(bquote(alpha==0.05)))

p <- sub.h0h1.1a + sub.h0h1.1b + sub.h0h1.2 +
  sub.sample2 + sub.sample2 + sub.sample2 +
  p.cidist1a + plot_spacer() + p.cidist2l +
  plot_spacer() + p.cidist1b + p.cidist2r +
  p.pfunc1a + p.pfunc1b + p.pfunc2 +
  plot_layout(design=l)

savesvg(plot=p, filename="ci", w=11, h=8)

