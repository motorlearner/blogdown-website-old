library(here)
library(tidyverse)
library(patchwork)

source(here("content", "blog", "plotsetup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "setup.R"))

# ---

sub.pfunc <- ggplot() + theme_x1y2() +
  theme(axis.title.y = element_text(size=textsz, color=textco, angle=00, margin=margin(r=0))) +
  ax(r=3, xl=c(-3,3), yl=c(0,1), xb=c(0), xt=expression(mu[0]), yb=c(0,1), yt=c(0,1), yname="p")

sub.rrfunc <- ggplot() + theme_x1y2() +
  theme(axis.title.y = element_text(size=textsz, color=textco, angle=90, margin=margin(r=0))) +
  ax(r=3, xl=c(-3,3), yl=c(0,1), xb=c(0), xt=expression(mu[0]), yb=c(0,1), yt=c(0,1), yname="Rejection Rate")

# function to annotate alpha/1-beta
annotate_alphaetc <- function(y, e){
  if (y <= .5) {vj <- 0; mar <- .02} else {vj <- 1; mar <- -.02}

  list(
    annotate("text", label=e, size=2.5, hjust=0, vjust=vj, x=-2.8, y=y+mar)
  )
}