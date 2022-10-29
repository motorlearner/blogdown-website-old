library(here)
library(tidyverse)
library(patchwork)

source(here("content", "blog", "plotsetup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "setup.R"))

# ---

source(here("content", "blog", "freqstats_series", "scripts", "subplot_reddistmu0.R"))

sub.alpha1a <- sub.reddistmu0 +
  geom_area(aes(x=ifelse(x>=qnorm(.95,0,sem),x,NA), y=y), color=color$grayd, fill=color$grayd) +
  geom_vline(xintercept=qnorm(.95,0,sem)) +
  geom_vline(xintercept=0, linetype="dashed")+
  annotate("text", size=2.25, label=expression(paste("incompatible w/ ", H[0])),
         vjust=1, y=1.5, hjust=0, x=qnorm(.95,0,sem)+.1) +
  annotate_rate(expression(alpha==0.05))

sub.alpha1b <- sub.reddistmu0 +
  geom_area(aes(x=ifelse(x<=qnorm(.05,0,sem),x,NA), y=y), color=color$grayd, fill=color$grayd) +
  geom_vline(xintercept=qnorm(.05,0,sem)) +
  geom_vline(xintercept=0, linetype="dashed") +
  annotate("text", size=2.25, label=expression(paste("incompatible w/ ", H[0])),
           vjust=1, y=1.5, hjust=1, x=qnorm(.05,0,sem)-.1) +
  annotate_rate(expression(alpha==0.05))

sub.alpha2 <- sub.reddistmu0 +
  geom_area(aes(x=ifelse(x>=qnorm(.975,0,sem),x,NA), y=y), color=color$grayd, fill=color$grayd) +
  geom_area(aes(x=ifelse(x<=qnorm(.025,0,sem),x,NA), y=y), color=color$grayd, fill=color$grayd) +
  geom_vline(xintercept=qnorm(.975,0,sem)) +
  geom_vline(xintercept=qnorm(.025,0,sem)) +
  geom_vline(xintercept=0, linetype="dashed") +
  annotate("text", size=2.25, label=expression(paste("incompatible w/ ", H[0])),
         vjust=1, y=1.5, hjust=1, x=qnorm(.025,0,sem)-.1) +
  annotate("text", size=2.25, label=expression(paste("incompatible w/ ", H[0])),
           vjust=1, y=1.5, hjust=0, x=qnorm(.975,0,sem)+.1) +
  annotate_rate(expression(alpha==0.05))