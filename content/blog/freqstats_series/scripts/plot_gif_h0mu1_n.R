rm(list=ls())
library(here)
library(tidyverse)
library(patchwork)
library(svglite)

source(here("content", "blog", "plotsetup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "setup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_h0h1.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_alldistsmu0.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_alpha.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_ratefunc.R"))
source(here("content", "blog", "freqstats_series", "scripts", "fun_gifsec.R"))

gifdir  <- here(plotdir, "gif_rejrate_h0h1_n")
fps     <- 20
sec     <- 4
nframes <- fps*sec+1

xvec <- seq(-3, 3, .01)
xrun_init <- gifsec(
  seq(0, 3, length.out = nframes),
  minfac=.05, halfwidth=2*sem, mode="sf", addvalues=rep(0,20)
)
xrun <- c(xrun_init, -xrun_init)

# N along rows, lower and upper limit in col 1 and 2
cut <- qnorm(
  matrix(rep(c(.025,.975),3), ncol=2, byrow=T),
  0,
  matrix(rep(semvec,2), ncol=2)
)

# rejection rate list
rej <- list(); for(i in 1:3) {rej[[i]] <- pnorm(cut[i,1], xvec, semvec[i], lower.tail=T) +
  pnorm(cut[i,2], xvec, semvec[i], lower.tail=F)}
rejrun <- list(); for(i in 1:3) {rejrun[[i]] <- pnorm(cut[i,1], xrun, semvec[i], lower.tail=T) +
  pnorm(cut[i,2], xrun, semvec[i], lower.tail=F)}

p.alpha.list <- p.dist.list <- p.rr.list <- list()

l <- "
ABC
DEF
GHI
"

# static parts
for (i in 1:3) {
  
  # static
  p.alpha.list[[i]] <- sub.alldistmu0.list[[i]] %+% (denlist[[i]] |> mutate(
    x = x,
    g1 = ifelse(x <= cut[i,1], x, NA_real_),
    g2 = ifelse(x >= cut[i,2], x, NA_real_)
  )) +
    geom_area(aes(x=g1, y=y), color=color$grayd, fill=color$grayd) +
    geom_area(aes(x=g2, y=y), color=color$grayd, fill=color$grayd) +
    geom_vline(xintercept=0, linetype="dashed") +
    geom_vline(xintercept=cut[i, ]) +
    annotate_rate(as.expression(bquote(alpha==0.05))) +
    annotate("text", size=2.25, label=expression(paste("incompatible w/ ", H[0])),
             vjust=1, y=1.5, hjust=1, x=cut[i,1]-.1) +
    annotate("text", size=2.25, label=expression(paste("incompatible w/ ", H[0])),
             vjust=1, y=1.5, hjust=0, x=cut[i,2]+.1) +
    ggtitle(as.expression(bquote(N==.(nvec[i])))) +
    theme(plot.title=element_text(hjust=.5))
}

# dynamic
# before running, delete all png's in directory
file.remove(list.files(path=gifdir, pattern=".*png$", full.names=TRUE))

for (j in seq_along(xrun)) {
  
  for(i in 1:3){
    
    if (xrun[j]==0){
      rrnote <- as.expression(bquote("RR"==.(format(round(rejrun[[i]][j],2),nsmall=2))))
      h0note <- as.expression(bquote("["~H[0]~"true ]"))
    } else {
      rrnote <- as.expression(bquote("RR"%~~%.(format(round(rejrun[[i]][j],2),nsmall=2))))
      h0note <- as.expression(bquote("["~H[1]~"true ]"))
    }
    
    p.dist.list[[i]] <- sub.alldistmu0.list[[i]] %+% (denlist[[i]] |> mutate(
      x  = x + xrun[j],
      g1 = ifelse(x <= cut[i,1], x, NA_real_),
      g2 = ifelse(x >= cut[i,2], x, NA_real_)
    )) +
      geom_area(aes(x=g1, y=y), color=color$grayd, fill=color$grayd) +
      geom_area(aes(x=g2, y=y), color=color$grayd, fill=color$grayd) +
      geom_vline(xintercept=xrun[j], linetype="dashed") +
      geom_vline(xintercept=cut[i, ]) +
      annotate_rate(rrnote) +
      annotate_true(h0note)
    
    p.rr.list[[i]] <- sub.rrfunc %+%
      (data.frame(x=xvec, y=rej[[i]])) +
      geom_line(aes(x=x,y=y), color=color$grayd, size=2) +
      geom_hline(yintercept=0.05, linetype="dotted") +
      annotate_alphaetc(y=0.05, e=as.expression(bquote(alpha==0.05))) + 
      geom_vline(xintercept=xrun[j], linetype="dashed")
  }
  
  p <- p.alpha.list[[1]] + p.alpha.list[[2]] + p.alpha.list[[3]] +
    p.dist.list[[1]] + p.dist.list[[2]] + p.dist.list[[3]] +
    p.rr.list[[1]] + p.rr.list[[2]] + p.rr.list[[3]] +
    plot_layout(design=l)
  
  savepng(plot=p, filename=paste0("rejrateH0H1_n", sprintf("%03d", j)), w=12, h=8, directory=gifdir)
  
  cat("\r", toString(j), "/", length(xrun))
}

library(gifski)
pngs <- list.files(path=gifdir, pattern=".*png$", full.names=TRUE)
gifski(pngs, gif_file=paste0(plotdir, "/rejrateH0H1_n.gif"), delay=1/fps, width=1200, height=800)