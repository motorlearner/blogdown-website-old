# load this script when creating plots for blogposts
# contains:
# - set of colors
# - ggplot2 themes
# - ggsave params

# colors --------------------------------------------------------------------------------

color <- list(
  purple = "#5a008a",
  red    = "#8a0900",
  orange = "#ba7c00",
  blue   = "#004c8a",
  green  = "#007529",
  black  = "#000000",
  grayd  = "#cccccc",
  grayl  = "#e5e5e5",
  bg     = "#fafafa"
)

# ggplot 2 themes -----------------------------------------------------------------------

library(ggplot2)

textsz = 9
linesz = .5
textco = "black" 
lineco = "black"
ticksz = unit(0.3, "lines")
bgfill = color[["bg"]]

# theme_blog: no axes lines
theme_blog <- function(){

  theme_void() + theme(
    # background
    plot.background = element_rect(size=NA, fill=bgfill),
    # margin
    plot.margin = unit(c(.2,.2,.2,.2), "lines"),
    # facets
    panel.spacing = unit(1.2, "lines"),
    strip.text    = element_text(size=textsz, color=textco, margin=margin(b=3)),
    # title
    plot.title = element_text(size=textsz, color=textco, margin=margin(b=6), hjust=0),
    # legend
    legend.title       = element_text(size=textsz, color=textco),
    legend.text        = element_text(size=textsz, color=textco),
    legend.text.align  = 0,
    legend.box.spacing = unit(0.5, "lines")
  )
}

# theme_x0: x axis line
theme_x0 <- function() theme_blog() + theme(axis.line.x = element_line(size=linesz, color=lineco))

# theme_x1: x axis line + text
theme_x1 <- function() {
  theme_x0() + theme(
    axis.text.x  = element_text(size=textsz, color=textco, margin=margin(t=1)),
    axis.ticks.x = element_line(size=linesz, color=lineco),
    axis.ticks.length = ticksz
  )
}

# theme_x2: x axis line + text + label
theme_x2 <- function() {
  theme_x1() + theme(
    axis.title.x = element_text(size=textsz, color=textco, margin=margin(t=8), angle=00)
  )
}

# theme_x0y0: x axis line, y axis line
theme_x0y0 <- function() theme_x0() + theme(axis.line.y = element_line(size=linesz, color=lineco))

# theme_x1y0: x axis line + text, y axis line
theme_x1y0 <- function() theme_x1() + theme(axis.line.y = element_line(size=linesz, color=lineco))

# theme_x1y2: x axis line + text, y axis line + text + label
theme_x1y2 <- function() {
  theme_x1y0() + theme(
    axis.text.y  = element_text(size=textsz, color=textco, margin=margin(r=1)),
    axis.ticks.y = element_line(size=linesz, color=lineco),
    axis.ticks.length = ticksz,
    axis.title.y = element_text(size=textsz, color=textco, margin=margin(r=8), angle=90)
  )
}

# theme_x2y0: x axis line + text + label, y axis line
theme_x2y0 <- function() theme_x2() + theme(axis.line.y = element_line(size=linesz, color=lineco))

# theme_x2y1: x axis line + text + label, y axis line + text
theme_x2y1 <- function() {
  theme_x2y0() + theme(
    axis.text.y  = element_text(size=textsz, color=textco, margin=margin(r=1)),
    axis.ticks.y = element_line(size=linesz, color=lineco),
    axis.ticks.length = ticksz
  )
}

# theme_x2y2: x axis line + text + label, y axis line + text + label
theme_x2y2 <- function() {
  theme_x2y1() + theme(
    axis.title.y = element_text(size=textsz, color=textco, margin=margin(r=8), angle=90)
  )
}

# add rectangle
addrect <- function() theme(panel.border = element_rect(size=linesz, color=lineco, fill=NA))

# customize axis (l = limit, b = breaks, tl = tick labels, n = axis name, e=expansion)
ax <- function(xname=NULL, xl=NULL, xb=NULL, xt=NULL, ex=0,
               yname=NULL, yl=NULL, yb=NULL, yt=NULL, ey=0,
               r=NULL){
  if (is.null(r)){
    list(
      coord_cartesian(xlim=xl, ylim=yl),
      scale_x_continuous(name=xname, breaks=xb, labels=xt, expand=expansion(ex)),
      scale_y_continuous(name=yname, breaks=yb, labels=yt, expand=expansion(ey))
    )
  } else {
    list(
      coord_fixed(xlim=xl, ylim=yl, ratio=r),
      scale_x_continuous(name=xname, breaks=xb, labels=xt, expand=expansion(ex)),
      scale_y_continuous(name=yname, breaks=yb, labels=yt, expand=expansion(ey))
    )
  }
}
