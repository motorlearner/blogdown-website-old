# Generate pic that goes into the sidebar (if exists) of the series, for all posts.
# Must be saved as "sidebar-featured" dot file-extension. 
library(here)
library(ggplot2)
library(patchwork)
library(svglite)

# colors
colors <- list(
  gray = "#cccccc",
  red  = "#8a0900"
)

# 