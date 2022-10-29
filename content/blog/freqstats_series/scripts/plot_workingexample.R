rm(list=ls())

library(here)
library(tidyverse)
library(patchwork)
library(svglite)

source(here("content", "blog", "plotsetup.R"))
source(here("content", "blog", "freqstats_series", "scripts", "setup.R"))

x <- seq(0,1,.001)
s <- rnorm(20, .5, 0.8*0.5/3)

p.base <- ggplot() + theme_blog() +
  ax(xl = c(0,1), yl=c(0,1)) +
  addrect()

p.poplvl <- p.base +
  ggtitle(expression(bold("Population"))) +
  theme(plot.title = element_text(hjust=.5)) +
  annotate("text", hjust=.5, vjust=1, x=.5, y=.9, size=3,
           label = as.expression(bquote("Some question about"~mu~"..."))) +
  annotate("text", hjust=.5, vjust=0, x=.5, y=.1, size=3,
           label = as.expression(bquote("...some inference about"~mu~".")))

p.popdist <- data.frame(
  x = x,
  y = dnorm(x, .5, 0.8*0.5/3)) |> 
  mutate(y = y/max(y)) |> 
  
  ggplot(aes(x=x,y=y)) +
  theme_x2() +
  ax(xl=c(0,1), xb=.5, xt=expression(mu),
     yl=c(0,1.5)) +
  geom_area(color=color$grayd, fill=color$grayd) +
  annotate("segment", x=.5, xend=.5, y=0, yend=1, linetype="dashed") +
  annotate("segment", x=.5, xend=.5+0.8*0.5/3, y=1.2, yend=1.2) + 
  annotate("segment", x=.5+c(0,0.8*0.5/3), xend=.5+c(0,0.8*0.5/3), y=1.25, yend=1.15) +
  annotate("text", hjust=.5, vjust=1, x=.5+0.4*0.5/3, y=1.15,
           size=3.5, label=expression(sigma))

p.samlvl <- p.base +
  ggtitle(expression(bold("Random Sample (Size N)"))) +
  theme(plot.title = element_text(hjust=.5))

p.sample <- data.frame(
  x = s,
  y = .5
) |> 
  ggplot(aes(x=x, y=y)) +
  theme_x1() +
  ax(xb=mean(s), xt="m", ex=.05,
     yl=c(0,1.5)) +
  geom_jitter(color=color$grayd, shape=16, height=.05, size=2) +
  geom_point(x=mean(s), y=.5, shape=15, color="black", fill="black", size=2) +
  annotate("segment", x=mean(s), xend=mean(s), y=0, yend=1) +
  annotate("segment", y=1.25, yend=1.25, x=mean(s), xend=mean(s)+sd(s)) +
  annotate("segment", y=1.25+.1, yend=1.25-.1, x=mean(s)+c(0,sd(s)), xend=mean(s)+c(0,sd(s))) +
  annotate("text", label="s", size=3.5, hjust=.5, vjust=1, y=1.25-.02, x=mean(s)+0.5*sd(s))
  
p.pop <- p.poplvl + 
  inset_element(p.popdist, 0.1, 0.24, 0.9, 0.84, align_to="plot")

p.sam <- p.samlvl +
  inset_element(p.sample, 0.1, 0.1, 0.9, 0.8, align_to="plot")

arrow1 <- ggplot() +
  theme_blog() +
  ax(xl=c(0,1), yl=c(0,1)) +
  geom_curve(aes(x=0, xend=1, y=.5, yend=0), angle=50, curvature=-.35, size=1,
             arrow = arrow(length = unit(.3, "npc"), type="closed")); arrow1

arrow2 <- ggplot() +
  theme_blog() +
  ax(xl=c(0,1), yl=c(0,1)) +
  geom_curve(aes(x=1, xend=0, y=1, yend=.5), angle=-50, curvature=-.35, size=1,
             arrow = arrow(length = unit(.3, "npc"), type="closed")); arrow2

l <- "
AAAAAB#####
AAAAA#CCCCC
AAAAA#CCCCC
AAAAA#CCCCC
AAAAAD#####
"

p <- p.pop + arrow1 + p.sam + arrow2 + plot_layout(design=l)

savesvg(plot=p, filename="workingexample", w=10, h=4)

