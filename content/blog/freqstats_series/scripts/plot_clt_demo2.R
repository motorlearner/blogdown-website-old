rm(list=ls())
library(here)
library(tidyverse)
library(patchwork)
library(svglite)

source(here("content", "blog", "plotsetup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "setup.R"))

set.seed(111)

nsamples <- 1000

x <- seq(0,1,.01)

# list of densities: dim1 (1:4) is popdist
dlist <- list(
  dnorm(x, mean=.5, sd=.15),
  dbeta(x, shape1=2, shape2=5),
  dbeta(x, shape1=1, shape2=3),
  dcauchy(x, location=.5, scale=.15)
)

beta_meansd <- function(a,b){
  m <- a / (a+b)
  sd <- sqrt((a*b) / ((a+b)^2 * (a+b+1)))
  return(c(m,sd))
}

# list of means and sd
musigma <- list(
  c(.5, .15),
  beta_meansd(2,5),
  beta_meansd(1,3),
  rep(NA_real_,2)
)

# list of means: dim1 (1:3) is n, dim2 (1:4) is popdist
mlist_pn <- list() 

for (j in 1:4){
  
  mlist_currentp <- list()
  
  for (i in 1:3){
    
    r_calls <- list(
      call("rnorm",   n=nvec[i], mean=.5, sd=.15),
      call("rbeta",   n=nvec[i], shape1=2, shape2=5),
      call("rbeta",   n=nvec[i], shape1=1, shape2=3),
      call("rcauchy", n=nvec[i], location=.5, scale=.1)
    )
    
    m <- colMeans(replicate(nsamples, eval(r_calls[[j]])))
    mlist_currentp[[i]] <- m
  }
  
  mlist_pn[[j]] <- mlist_currentp
}

# list of popdist plots: dim1 (1:4) is popdist
p.popdist <- list()

# list of samdist plots: dim1 (1:4) is popdist, dim2 (1:3) is n
p.samdist <- list()

for (j in 1:4){
  
  p.popdist[[j]] <- data.frame(x=x, y=dlist[[j]]) |> 
    ggplot(aes(x=x, y=y)) +
    geom_area(color=color$grayd, fill=color$grayd) +
    geom_vline(xintercept=musigma[[j]][1], linetype="dashed") +
    theme_x1() +
    ax(xl=c(0,1), yl=c(0,3.1), r=.25) 
  
  p.samdist_currentp <- list()
  
  for (i in 1:3){
    
    p.samdist_currentp[[i]] <- data.frame(
      m = mlist_pn[[j]][[i]],
      x = seq(0,1,length.out=nsamples),
      y = dnorm(seq(0,1,length.out=nsamples),musigma[[j]][1], musigma[[j]][2]/sqrt(nvec[i]))
    ) |> 
      ggplot(aes(x=m)) +
      theme_x0() +
      ax(xl=c(0,1)) +
      geom_histogram(aes(y=..density..), binwidth=.01,
                     color=colorlist[[i]], fill=colorlist[[i]]) +
      geom_area(aes(x=x, y=y), color="black", fill="white",
                linetype="solid", alpha=.2, size=.2) +
      geom_vline(xintercept = mean(mlist_pn[[j]][[i]]), linetype="dashed")
    
  }
  
  p.samdist[[j]] <- p.samdist_currentp
} 


p <- p.popdist[[1]] + p.popdist[[2]] + p.popdist[[3]] + p.popdist[[4]] +
  p.samdist[[1]][[1]] + p.samdist[[2]][[1]] + p.samdist[[3]][[1]] + p.samdist[[4]][[1]] +
  p.samdist[[1]][[2]] + p.samdist[[2]][[2]] + p.samdist[[3]][[2]] + p.samdist[[4]][[2]] +
  p.samdist[[1]][[3]] + p.samdist[[2]][[3]] + p.samdist[[3]][[3]] + p.samdist[[4]][[3]] +
  plot_layout(ncol=4)

savesvg(plot=p, filename="clt_demo2", w=8, h=7.5)
