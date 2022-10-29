rm(list=ls())

library(here)
library(tidyverse)
library(patchwork)
library(svglite)

source(here("content", "blog", "plotsetup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "setup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "subplot_h0h1.R"))

l = "
ABC
"

p <- sub.h0h1.1a + sub.h0h1.1b + sub.h0h1.2 + plot_layout(design=l)

savesvg(plot=p, filename="h0h1", h=1.5, w=10)
