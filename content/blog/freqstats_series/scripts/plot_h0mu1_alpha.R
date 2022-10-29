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
"

xvec <- seq(-3,3,.01)

# alpha along rows, lower and upper limit in col 1 and 2 (don't ask why)
cut <- qnorm(matrix(c(alpha/2, 1-alpha/2), byrow=F, ncol=2), 0, sem)

# rejection rate list
rej <- list(); for(i in 1:3) {rej[[i]] <- pnorm(cut[i,1], xvec, sem, lower.tail=T) +
                                          pnorm(cut[i,2], xvec, sem, lower.tail=F)}
# mu1 list
mu1 <- list(); for(i in 1:3) {mu1[[i]] <- cut[i, ] + c(-1,1) * qnorm(1-alpha[[i]],0,sem)}


p.alpha.list <- p.beta.list <- p.rr.list <- list()

for (i in 1:3) {
  
  
  p.alpha.list[[i]] <- sub.reddistmu0 %+% (den |> mutate(
    x = x,
    g1 = ifelse(x <= cut[i,1], x, NA_real_),
    g2 = ifelse(x >= cut[i,2], x, NA_real_)
  )) +
    geom_area(aes(x=g1, y=y), color=color$grayd, fill=color$grayd) +
    geom_area(aes(x=g2, y=y), color=color$grayd, fill=color$grayd) +
    geom_vline(xintercept=0, linetype="dashed") +
    geom_vline(xintercept=cut[i, ]) +
    annotate_rate(as.expression(bquote(alpha==.(alpha[[i]])))) +
    annotate("text", size=2.25, label=expression(paste("incompatible w/ ", H[0])),
             vjust=1, y=1.5, hjust=1, x=cut[i,1]-.1) +
    annotate("text", size=2.25, label=expression(paste("incompatible w/ ", H[0])),
             vjust=1, y=1.5, hjust=0, x=cut[i,2]+.1) +
    ggtitle(as.expression(bquote(alpha==.(alpha[i])))) +
    theme(plot.title=element_text(hjust=.5))
  
  p.beta.list[[i]] <- sub.reddistmu0 %+% (den |> mutate(
    x1 = x + mu1[[i]][1],
    x2 = x + mu1[[i]][2],
    g1 = ifelse(x1 <= cut[i,1], x1, NA_real_),
    g2 = ifelse(x2 >= cut[i,2], x2, NA_real_),
    x = NA_real_
  )) +
    geom_area(aes(x=x1, y=y), color=colorlist[[2]], fill=colorlist[[2]]) +
    geom_area(aes(x=x2, y=y), color=colorlist[[2]], fill=colorlist[[2]]) +
    geom_area(aes(x=g1, y=y), color=color$grayd, fill=color$grayd) +
    geom_area(aes(x=g2, y=y), color=color$grayd, fill=color$grayd) +
    geom_vline(xintercept=mu1[[i]], linetype="dashed") +
    geom_vline(xintercept=cut[i, ]) +
    ax(r=1.25,
       xl=c(-3,3), xb=mu1[[i]], xt=expression(mu[paste(1,"L")], mu[paste(1,"U")]),
       yl=c(0,1.55)) +
    annotate("label", label.size=NA, size=2.25, label.padding=unit(0.1,"lines"),
             label=as.expression(bquote(beta==.(alpha[i]))),
             color="white", fill=colorlist[[2]], vjust=.5, x=3, y=.8, hjust=1) +
    annotate("text", size=2.25, label="inc. w/",
             vjust=1, y=1.5, hjust=.5, x=0) 
  
  p.rr.list[[i]] <- sub.rrfunc %+%
    (data.frame(x=xvec, y=rej[[i]])) +
    ax(r=3, 
       xl=c(-3,3), xb=c(mu1[[i]][[1]], 0, mu1[[i]][[2]]), 
       xt=expression(mu[paste(1,"L")], mu[0], mu[paste(1,"U")]), 
       yl=c(0,1), yb=c(0,1), yt=c(0,1), yname="Rejection Rate") +
    geom_line(aes(x=x,y=y), color=color$grayd, size=2) +
    geom_vline(xintercept=c(0, mu1[[i]]), linetype="dashed") +
    geom_hline(yintercept=c(alpha[i], 1-alpha[i]), linetype="dotted") +
    annotate_alphaetc(y=alpha[i], e=as.expression(bquote(alpha==.(alpha[i])))) +
    annotate_alphaetc(y=1-alpha[i], e=as.expression(bquote(1-beta==.(1-alpha[i]))))
}


for (i in 1:2) {
  p.beta.list[[i]] <- p.beta.list[[i]] +
    annotate("text", size=2.25, label="...",
             vjust=1, y=1.5-.2, hjust=.5, x=0) 
}


p.beta.list[[3]] <- p.beta.list[[3]] +
  annotate("text", size=2.25, label=expression(mu<=mu[paste(1,"L")]),
           vjust=1, y=1.5-.3, hjust=.5, x=0) +
  annotate("text", size=2.25, label=expression(mu>=mu[paste(1,"U")]),
           vjust=1, y=1.5-.3-.3, hjust=.5, x=0) 


p <- p.alpha.list[[1]] + p.alpha.list[[2]] + p.alpha.list[[3]] +
  p.beta.list[[1]] + p.beta.list[[2]] + p.beta.list[[3]] +
  p.rr.list[[1]] + p.rr.list[[2]] + p.rr.list[[3]] +
  plot_layout(design=l)

savesvg(plot=p, filename="h0mu1_alpha", w=12, h=6)


