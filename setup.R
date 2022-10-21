# README -----------------------------------------------------------------------------
# This script contains things that you often need to load when writing a blogpost:
#   - custom ggplot2 theme
#   - plotting colors
# ......................................................................................

# list of colors
color <- list(
  purple = "#5a008a",
  red    = "#8a0900",
  orange = "#ba7c00",
  blue   = "#004c8a",
  green  = "#007529",
  black  = "#000000",
  gray   = "#cccccc",
  gray2  = "#8c8c8c"
)

# ggplot theme
library(ggplot2)
theme_blog <- function(textsz=9){
  # params
  textsz = textsz
  linesz = 0.5
  textco = "black"
  lineco = "black"
  gridco = "gray70"
  ticksz = unit(0.3, "lines") 
  
  # this adds all elements 
  theme_void() + theme(
    # plot margin
    plot.margin = unit(c(.8,.2,.8,.2), "lines"),
    # axes
    axis.line.x  = element_line(size=linesz, color=lineco),
    axis.line.y  = element_line(size=linesz, color=lineco),
    axis.ticks.x = element_line(size=linesz, color=lineco),
    axis.ticks.y = element_line(size=linesz, color=lineco),
    axis.ticks.length = ticksz,
    axis.text.x  = element_text(size=textsz, color=textco, margin=margin(t=1)),
    axis.text.y  = element_text(size=textsz, color=textco, margin=margin(r=1)),
    axis.title.x = element_text(size=textsz, color=textco, margin=margin(t=10), angle=00),
    axis.title.y = element_text(size=textsz, color=textco, margin=margin(r=10), angle=90),
    # box
    panel.border = element_rect(size=linesz, color=lineco, fill=NA),
    # grid
    panel.grid.major = element_line(size=linesz, color=gridco),
    # facet params
    strip.text = element_text(size=textsz, color=textco, margin=margin(b=3)),
    panel.spacing = unit(1.2, "lines"),
    # plot title
    plot.title = element_text(size=textsz, color=textco, margin=margin(b=6), hjust=0),
    # legend
    legend.text = element_text(size=textsz, color=textco),
    legend.text.align = 0,
    legend.title = element_text(size=textsz, color=textco),
    legend.box.spacing = unit(0.5, "lines")
  )
}

