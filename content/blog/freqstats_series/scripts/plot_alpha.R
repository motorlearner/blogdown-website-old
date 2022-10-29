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

l = "
ABC
DEF
"

p <- sub.h0h1.1a + sub.h0h1.1b + sub.h0h1.2 + sub.alpha1a + sub.alpha1b + sub.alpha2 + plot_layout(design=l)

savesvg(plot=p, filename="alpha", h=3, w=10)
