library(here)
library(ggplot2)
library(svglite)

source(here("content", "blog", "plotsetup.R"))

tlabs1 <- expression(italic(T[1]), italic(T[2]), italic(T[3]), "...")
olabs1 <- expression(italic(O[1]), italic(O[2]), italic(O[3]), "...")

tlabs2 <- expression(italic(T[1]), italic(T[2]))
olabs2 <- expression(italic(O[1]), italic(O[2]))

p1 <- ggplot() +
  theme_blog() +
  ax(xl=c(0,1), yl=c(0,1)) +
  annotate("rect", fill=NA, color="black", xmin=.1, xmax=.9, ymin=c(.1,.6), ymax=c(.4,.9)) +
  annotate("text", label=olabs1, hjust=.5, vjust=.5, size=4, x=c(1/5, 2/5, 3/5, 4/5), y=.25) +
  annotate("text", label=tlabs1, hjust=.5, vjust=.5, size=4, x=c(1/5, 2/5, 3/5, 4/5), y=.75) +
  annotate("segment", y=.75-.1, yend=.25+.1, x=c(1/5,2/5,2/5,3/5), xend=c(1/5,2/5, 3/5, 3/5))

p2 <- ggplot() +
  theme_blog() +
  ax(xl=c(0,1), yl=c(0,1)) +
  annotate("rect", fill=NA, color="black", xmin=.1, xmax=.9, ymin=c(.1,.6), ymax=c(.4,.9)) +
  annotate("text", label=olabs2, hjust=.5, vjust=.5, size=4, x=c(1/3, 2/3), y=.25) +
  annotate("text", label=tlabs2, hjust=.5, vjust=.5, size=4, x=c(1/3, 2/3), y=.75) +
  annotate("segment", y=.75-.1, yend=.25+.1, x=c(1/3,2/3,2/3), xend=c(1/3,1/3,2/3))

ggsave(
  plot = p1, 
  filename = "compatible1.svg",
  path = here("content", "blog", "freqstats_series", "02_rejection"),
  scale  = .5,
  width  = 4, 
  height = 2
)
ggsave(
  plot = p2, 
  filename = "compatible2.svg",
  path = here("content", "blog", "freqstats_series", "02_rejection"),
  scale  = .5,
  width  = 2.5, 
  height = 2
)
