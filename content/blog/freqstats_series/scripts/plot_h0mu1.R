rm(list=ls())

library(here)
library(tidyverse)
library(patchwork)
library(svglite)

source(here("content", "blog", "plotsetup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "setup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_h0h1.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_reddistmu0.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_alpha.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_ratefunc.R"))

l = "
ABC
DEF
GHI
JKL
"

xvec <- seq(-3,3,.01)

cut1a <- qnorm(.95, 0, sem)
cut1b <- qnorm(.05, 0, sem)
cut2  <- qnorm(c(.025,.975), 0, sem)

rej1a <- pnorm(cut1a, xvec, sem, lower.tail=FALSE)
rej1b <- pnorm(cut1b, xvec, sem, lower.tail=TRUE)
rej2  <- pnorm(cut2[1], xvec, sem, lower.tail=TRUE) + 
  pnorm(cut2[2], xvec, sem, lower.tail=FALSE)

mu11a <- qnorm(.95, cut1a, sem)
mu11b <- qnorm(.05, cut1b, sem)
mu12  <- cut2 + c(-1,1) * qnorm(.95, 0, sem)

p.h0h1.1a <- sub.h0h1.1a +
  ax(xl=c(-3,3), xb=c(0,mu11a), xt=expression(mu[0],mu[1]), yl=c(0,0.5), r=1.25)

p.h0h1.1b <- sub.h0h1.1b +
  ax(xl=c(-3,3), xb=c(mu11b,0), xt=expression(mu[1],mu[0]), yl=c(0,0.5), r=1.25)

p.h0h1.2 <- sub.h0h1.2 +
  ax(xl=c(-3,3), xb=c(mu12[1],0,mu12[2]), yl=c(0,0.5), r=1.25,
     xt=expression(mu[paste(1,"L")], mu[0], mu[paste(1,"U")]))

p.beta1a <- den |> 
  mutate(x = x + mu11a, g = ifelse(x>cut1a, x, NA_real_)) |> 
  ggplot() + theme_x1() +
  ax(xl=c(-3,3), xb=c(mu11a), xt=expression(mu[1]), yl=c(0,1.55), r=1.25) +
  geom_area(aes(x=x, y=y), color=colorlist[[2]], fill=colorlist[[2]]) +
  geom_area(aes(x=g, y=y), color=color$grayd, fill=color$grayd) +
  geom_vline(xintercept=cut1a) +
  geom_vline(xintercept=mu11a, linetype="dashed") +
  annotate("text", size=2.25, 
           label=as.expression(bquote("incompatible w/"~mu>=mu[1])),
           vjust=1, y=1.5, hjust=1, x=cut1a-.1) +
  annotate("label", label.size=NA, size=2.25, label.padding=unit(0.1,"lines"),
           label=as.expression(bquote(beta==0.05)),
           color="white", fill=colorlist[[2]], vjust=.5, x=3, y=.8, hjust=1)

p.beta1b <- den |> 
  mutate(x = x + mu11b, g = ifelse(x<=cut1b, x, NA_real_)) |> 
  ggplot() + theme_x1() +
  ax(xl=c(-3,3), xb=c(mu11b), xt=expression(mu[1]), yl=c(0,1.55), r=1.25) +
  geom_area(aes(x=x, y=y), color=colorlist[[2]], fill=colorlist[[2]]) +
  geom_area(aes(x=g, y=y), color=color$grayd, fill=color$grayd) +
  geom_vline(xintercept=cut1b) +
  geom_vline(xintercept=mu11b, linetype="dashed") +
  annotate("text", size=2.25, 
           label=as.expression(bquote("incompatible w/"~mu<=mu[1])),
           vjust=1, y=1.5, hjust=0, x=cut1b+.1) +
  annotate("label", label.size=NA, size=2.25, label.padding=unit(0.1,"lines"),
           label=as.expression(bquote(beta==0.05)),
           color="white", fill=colorlist[[2]], vjust=.5, x=3, y=.8, hjust=1)

p.beta2 <- den |> 
  mutate(x1 = x + mu12[1],
         x2 = x + mu12[2],
         g1 = ifelse(x1<=cut2[1], x1, NA_real_),
         g2 = ifelse(x2>=cut2[2], x2, NA_real_)) |> 
  ggplot() + theme_x1() +
  ax(xl=c(-3,3), xb=c(mu12[1],mu12[2]), xt=expression(mu[paste(1,"L")], mu[paste(1,"U")]), 
     yl=c(0,1.55), r=1.25) +
  geom_area(aes(x=x1, y=y), color=colorlist[[2]], fill=colorlist[[2]]) +
  geom_area(aes(x=x2, y=y), color=colorlist[[2]], fill=colorlist[[2]]) +
  geom_area(aes(x=g1, y=y), color=color$grayd, fill=color$grayd) +
  geom_area(aes(x=g2, y=y), color=color$grayd, fill=color$grayd) +
  geom_vline(xintercept=cut2) +
  geom_vline(xintercept=mu12, linetype="dashed") +
  annotate("text", size=2.25, label="inc. w/",
           vjust=1, y=1.5, hjust=.5, x=0) +
  annotate("text", size=2.25, label=expression(mu<=mu[paste(1,"L")]),
           vjust=1, y=1.5-.3, hjust=.5, x=0) +
  annotate("text", size=2.25, label=expression(mu>=mu[paste(1,"U")]),
           vjust=1, y=1.5-.3-.3, hjust=.5, x=0) +
  annotate("label", label.size=NA, size=2.25, label.padding=unit(0.1,"lines"),
           label=as.expression(bquote(beta==0.05)),
           color="white", fill=colorlist[[2]], vjust=.5, x=3, y=.8, hjust=1)

p.rr1a <- sub.rrfunc %+%
  (data.frame(x=xvec, y=rej1a)) +
  ax(r=3,
     xl=c(-3,3), xb=c(0,mu11a), xt=expression(mu[0], mu[1]),
     yl=c(0,1), yb=c(0,1), yt=c(0,1), yname="Rejection Rate") +
  geom_line(aes(x=x,y=y), color=color$grayd, size=2) +
  geom_vline(xintercept=c(0,mu11a), linetype="dashed") +
  geom_hline(yintercept=c(.05,.95), linetype="dotted") +
  annotate_alphaetc(y=.05, e=as.expression(bquote(alpha==0.05))) +
  annotate_alphaetc(y=.95, e=as.expression(bquote(1-beta==0.95)))

p.rr1b <- sub.rrfunc %+%
  (data.frame(x=xvec, y=rej1b)) +
  ax(r=3, 
     xl=c(-3,3), xb=c(mu11b,0), xt=expression(mu[1], mu[0]), 
     yl=c(0,1), yb=c(0,1), yt=c(0,1), yname="Rejection Rate") +
  geom_line(aes(x=x,y=y), color=color$grayd, size=2) +
  geom_vline(xintercept=c(0,mu11b), linetype="dashed") +
  geom_hline(yintercept=c(.05,.95), linetype="dotted") +
  annotate_alphaetc(y=.05, e=as.expression(bquote(alpha==0.05))) +
  annotate_alphaetc(y=.95, e=as.expression(bquote(1-beta==0.95)))

p.rr2 <- sub.rrfunc %+%
  (data.frame(x=xvec, y=rej2)) +
  ax(r=3, 
     xl=c(-3,3), xb=c(mu12[1],0,mu12[2]), 
     xt=expression(mu[paste(1,"L")], mu[0], mu[paste(1,"U")]), 
     yl=c(0,1), yb=c(0,1), yt=c(0,1), yname="Rejection Rate") +
  geom_line(aes(x=x,y=y), color=color$grayd, size=2) +
  geom_vline(xintercept=c(0,mu12), linetype="dashed") +
  geom_hline(yintercept=c(.05,.95), linetype="dotted") +
  annotate_alphaetc(y=.05, e=as.expression(bquote(alpha==0.05))) +
  annotate_alphaetc(y=.95, e=as.expression(bquote(1-beta==0.95)))

p <- p.h0h1.1a + p.h0h1.1b + p.h0h1.2 + 
  sub.alpha1a + sub.alpha1b + sub.alpha2 + 
  p.beta1a + p.beta1b + p.beta2 +
  p.rr1a + p.rr1b + p.rr2 +
  plot_layout(design=l)

savesvg(plot=p, filename="h0mu1", w=12, h=7)
