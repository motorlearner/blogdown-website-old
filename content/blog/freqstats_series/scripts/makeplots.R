# this scrip runs all the plotting scripts 
# ........................................

library(here)
library(purrr)

# directory where scripts  and plots are saved
scriptdir <- here("content", "blog", "freqstats_series", "scripts")

# list all files that start with plot, and run all of them
files <- list.files(path = scriptdir, pattern = "^plot")

# get full paths to each file
paths <- paste(scriptdir, files, sep="/")

# run all
purrr::walk(paths, source)
