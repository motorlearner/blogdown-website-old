# SETUP ---------------------------------------------------------------------------------

# packages
library(here)
library(tidyverse); theme_set(theme_void())
library(patchwork)
library(svglite)

# load colors and ggplot theme♦
source(here("setup.R"))

# seed
set.seed(2020)

# plot saving directory
plotdir = here("content", "blog", "freqstatsprimer", "plots")

# initialize params
n <- c(5,15,50)               # sample sizes
while(T){                     # sample with sample mean constraint
  sample = rnorm(n[2])
  if(mean(sample)>0.3){
    break()
  }
}
while(T){
  sample1 = rnorm(n[1])
  if(abs(mean(sample)-mean(sample1)) < 0.001 & max(abs(sample1)) <= 3){
    break
  }
}
while(T){
  sample3 = rnorm(n[3])
  if(abs(mean(sample)-mean(sample3)) < 0.001 & max(abs(sample3)) <= 3){
    break
  }
}

# initialize functions
getnd <- function(mu,sigma){  # get data frame with x,y for normal dist
  x = seq(mu-3.5*sigma, mu+3.5*sigma, 0.001)
  y = dnorm(x, mu, sigma)
  return(data.frame(x,y))
}

# colors, for now
c <- c(color$blue, color$red, color$orange, color$gray2, color$gray2)

# PLOTS ---------------------------------------------------------------------------------

# Parameters are:
#   - scale  = 0.6 (so everything is nicely visible)
#   - width  = 10  (can go slightly wider if needed)
#   - height = __  (adjust so there is no excessive top/bottom margin)

## 1. Overview --------------------------------------------------------------------------

plot1 <- ggplot() + theme_void() + theme(plot.margin = unit(c(.8,.2,.8,.2), "lines")) +
  # axis limits
  scale_x_continuous(limits=c(-1,1), expand=expansion(mult=c(0.01,0.01))) +
  scale_y_continuous(limits=c(0,1), expand=expansion(mult=c(0,0))) +
  # population and sample box
  annotate("rect", xmin=c(-1,0.1), xmax=c(-0.1,1), ymin=c(0.1,0.3), ymax=c(0.9,0.7), color="black", fill=NA) +
  # box ticks and axis annotation
  geom_segment(aes(y=0.10, yend=0.08, x=-1*seq(0.1,1,0.15), xend=-1*seq(0.1,1,0.15))) +
  geom_segment(aes(y=0.30, yend=0.28, x=1*seq(0.1,1,0.15), xend=1*seq(0.1,1,0.15))) +
  annotate("text", label="X", hjust=0.5, vjust=1, y=c(0.05,0.25), x=c(-0.55, 0.55), size=3) +
  # box titles
  annotate("label", label="Population", hjust=0.5, vjust=0.5, x=-0.55, y=0.9, size=3, fontface=2, fill="white") +
  annotate("label", label="Random Sample of Size N", hjust=0.5, vjust=0.5, x=0.55, y=0.7, size=3, fontface=2, fill="white") +
  # arrows
  geom_curve(aes(x=-0.1, xend=0.1, y=0.8, yend=0.7), size=1, arrow=arrow(length=unit(0.2, "cm")), curvature=-0.25, ncp=100) +
  geom_curve(aes(x=0.1, xend=-0.1, y=0.3, yend=0.2), size=1, arrow=arrow(length=unit(0.2, "cm")), curvature=-0.25, ncp=100) +
  # population box content
  geom_ribbon(aes(x=seq(-0.9,-0.2,0.001), ymax=dnorm(seq(-0.9,-0.2,0.001),-0.55,0.1)/20+0.4, ymin=0.4), color=c[5], fill=c[5], size=1) +
  geom_segment(aes(y=0.4, yend=max(dnorm(seq(-0.9,-0.2,0.001),-0.55,0.1)/20+0.4), x=-0.55, xend=-0.55), linetype="dashed") +
  annotate("text", label=expression(mu), hjust=0.5, vjust=1, x=-0.55, y=0.38)+
  annotate("text", label=expression(paste("Some hypothesis about ", mu,"...")), hjust=0.5, vjust=0.5, x=-0.55, y=0.75, size=3) +
  annotate("text", label=expression(paste("...some inference about ", mu,".")), hjust=0.5, vjust=0.5, x=-0.55, y=0.25, size=3) +
  annotate("text", label=expression(paste(mu," (mean)")), hjust=0, vjust=1, x=-0.4, y=0.60, size=3) +
  annotate("text", label=expression(paste(sigma," (stdev)")), hjust=0, vjust=1, x=-0.4, y=0.54, size=3) +
  # sample box content
  geom_jitter(aes(x=rnorm(30,0.55,0.1), y=rep(0.45,30)), height=0.02, shape=21, size=1.5, color="black", fill=c[5]) +
  geom_segment(aes(x=0.56, xend=0.56, y=0.53, yend=0.37)) +
  geom_point(aes(x=0.56, y=0.45), shape=22, size=2.5, color="black", fill="black") +
  annotate("text", label="m", hjust=0.5, vjust=1, x=0.56, y=0.37, size=3) +
  annotate("text", label=expression(paste("M (mean)")), hjust=0, vjust=1, x=0.75, y=0.60, size=3) +
  annotate("text", label=expression(paste("S  (stdev)")), hjust=0, vjust=1, x=0.75, y=0.54, size=3)

ggsave(
  plot     = plot1,
  filename = "01_overview.svg",
  path     = plotdir,
  scale    = 0.6,
  width    = 10,
  height   = 5
)

## 2. CLT Sampling ----------------------------------------------------------------------

# number of samples shown 
ns = 20

plot2 <- list( # create dataframes
  n1 = as.data.frame(replicate(ns, rnorm(n[1]))),
  n2 = as.data.frame(replicate(ns, rnorm(n[2]))),
  n3 = as.data.frame(replicate(ns, rnorm(n[3])))
) %>% 
  # combine into one dataframe
  bind_rows(.id = "n") %>% 
  # sample size identifier ...
  mutate(n = case_when(
    n == "n1" ~ 5,
    n == "n2" ~ 15,
    n == "n3" ~ 50
  )) %>% 
  # sample identifier
  pivot_longer(
    cols = starts_with("V"),
    names_to = "id",
    names_prefix = "V",
    values_to = "x"
  ) %>% 
  # get means
  group_by(n, id) %>% 
  mutate(mean = mean(x)) %>% 
  ungroup() %>% 
  # plot
  ggplot() +
  facet_wrap(~n, ncol=3, labeller=label_bquote(N==.(n))) +
  geom_vline(xintercept=0, 
             size=0.5, color="black", linetype="dashed") +
  geom_jitter(aes(x=x, y=as.numeric(id)), height=0.05, 
              size=1.5, shape=21, color=c[5], fill=c[5]) +
  geom_point(aes(x=mean, y=as.numeric(id), color=factor(n), fill=factor(n)), 
             shape=22, size=2.5) +
  scale_color_manual(values=c) +
  scale_fill_manual(values=c) +
  # axes
  scale_x_continuous(name=expression(paste("Distance from ", mu)),
                     limits=c(-3.5,3.5), 
                     breaks=seq(-3,3,3),
                     labels=expression(-3~sigma, 0, +3~sigma),
                     expand=expansion(0)) +
  scale_y_continuous(name=NA,
                     limits=c(0,ns+1), breaks=seq(0,100,1),
                     expand=expansion(mult=c(0,0))) +
  # plot design
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  ) +
  # aspect_ratio
  theme(aspect.ratio = 1.5)

ggsave(
  plot     = plot2,
  filename = "02_clt_sampling.svg",
  path     = plotdir,
  scale    = 0.6,
  width    = 10,
  height   = 6.5
)

## 3. CLT Distributions -----------------------------------------------------------------

plot3 <- list( # create dataframes
  n1 = getnd(0,1/sqrt(n[1])),
  n2 = getnd(0,1/sqrt(n[2])),
  n3 = getnd(0,1/sqrt(n[3]))
) %>% 
  # bind 
  bind_rows(.id = "n") %>% 
  # sample size identifier ...
  mutate(n = case_when(
    n == "n1" ~ 5,
    n == "n2" ~ 15,
    n == "n3" ~ 50
  )) %>% 
  # plot 
  ggplot(aes(x=x, y=y, color=factor(n), fill=factor(n))) +
  facet_wrap(~ n, ncol=3, labeller=label_bquote(N==.(n))) +
  geom_area(data=getnd(0,1), aes(x=x, y=y),
            color=c[5], fill=c[5]) +
  geom_area(size=1, alpha=0.9) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_color_manual(values=c) +
  scale_fill_manual(values=c) +
  scale_x_continuous(name=expression(paste("Distance from ", mu)),
                     limits=c(-3.5,3.5), 
                     breaks=seq(-3,3,3),
                     labels=expression(-3~sigma, 0, +3~sigma),
                     expand=expansion(0)) +
  scale_y_continuous(name=NA,
                     limits=c(0,NA), breaks=NULL,
                     expand=expansion(0)) +
  # plot design
  coord_fixed(ratio=1.5) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none"
  )

ggsave(
  plot     = plot3,
  filename = "03_clt_distributions.svg",
  path     = plotdir,
  scale    = 0.6,
  height   = 4,
  width    = 10
)

## 25. Nonzero Distribution --------------------------------------------------------------

plot25 <- getnd(0,1/sqrt(15)) |> ggplot() +
  geom_area(aes(x,y), color=c[2], fill=c[2]) +
  # design
  theme_blog() +
  theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  # axes
  scale_x_continuous(breaks=0, labels=expression(mu[0])) +
  scale_y_continuous(expand=expansion(0)) +
  coord_fixed(ratio=1.25, xlim=c(-3,3), ylim=c(0,1.6), clip="off") +
  # annotations
  # boxes
  annotate("rect", color="black", fill=NA,
           xmin=-2, xmax=-1.8, ymin=-0.05, ymax=0.05) +
  annotate("rect", color="black", fill=NA,
           xmin=1.8, xmax=2, ymin=-0.05, ymax=0.05) +
  # zoom content
  annotate("rect", color=c[2], fill=c[2],
           xmin=c(-2.9,0.9), xmax=c(-0.9,2.9), ymin=1, ymax=1.03) +
  annotate("segment", color="black", size=1,
           x=c(-2.9,2.9), xend=c(-0.9,0.9), y=1, yend=1) +
  # zoom box
  annotate("rect", color="black", fill=NA,
           xmin=-2.9, xmax=-0.9, ymin=0.5, ymax=1.5) +
  annotate("rect", color="black", fill=NA,
           xmin=0.9, xmax=2.9, ymin=0.5, ymax=1.5) +
  # connections
  annotate("segment", linetype="dotted",
           x=c(-2,-1.8,1.8,2), xend=c(-2.9,-0.9,0.9,2.9), y=0.05, yend=0.5) 

ggsave(
  plot     = plot25,
  filename = "25_nonzero_distribution.svg",
  path     = plotdir,
  scale    = 0.6,
  height   = 3,
  width    = 7
)

## 26. Nonzero Distribution alpha -------------------------------------------------------

plot26 <- getnd(0,1/sqrt(15)) |> ggplot() +
  geom_area(aes(x,y), 
            color=c[2], fill=c[2]) +
  geom_area(aes(ifelse(x < qnorm(0.03, 0, 1/sqrt(15), lower.tail=T), x, NA), y),
            color=c[4], fill=c[4]) +
  geom_area(aes(ifelse(x > qnorm(0.03, 0, 1/sqrt(15), lower.tail=F), x, NA), y),
            color=c[4], fill=c[4]) +
  # design
  theme_blog() +
  theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  # axes
  scale_x_continuous(breaks=0, labels=expression(mu[0])) +
  scale_y_continuous(expand=expansion(0)) +
  coord_fixed(ratio=1.25, xlim=c(-3,3), ylim=c(0,1.6), clip="off") +
  # annotations
  # alpha 
  annotate("segment", arrow=arrow(type="closed", length=unit(0.4,"lines")),
           x=0.6, xend=c(-.55,.55), y=0.6, yend=0.08) +
  annotate("text", size=3.5, label=expression(alpha),
           hjust=0, vjust=0, x=0.6, y=0.62) +
  # 1-alpha
  annotate("segment", arrow=arrow(type="closed", length=unit(0.4,"lines")),
           x=0.3, xend=0, yend=0.7, y=1.2) +
  annotate("text", size=3.5, label=expression(1-alpha),
           hjust=0, vjust=0.5, x=0.3, y=1.22) +
  # boxes
  annotate("rect", color="black", fill=NA,
           xmin=-2, xmax=-1.8, ymin=-0.05, ymax=0.05) +
  annotate("rect", color="black", fill=NA,
           xmin=1.8, xmax=2, ymin=-0.05, ymax=0.05) +
  # zoom content
  annotate("rect", color=c[4], fill=c[4],
           xmin=c(-2.9,0.9), xmax=c(-0.9,2.9), ymin=1, ymax=1.03) +
  annotate("segment", color="black", size=1,
           x=c(-2.9,2.9), xend=c(-0.9,0.9), y=1, yend=1) +
  # zoom box
  annotate("rect", color="black", fill=NA,
           xmin=-2.9, xmax=-0.9, ymin=0.5, ymax=1.5) +
  annotate("rect", color="black", fill=NA,
           xmin=0.9, xmax=2.9, ymin=0.5, ymax=1.5) +
  # connections
  annotate("segment", linetype="dotted",
           x=c(-2,-1.8,1.8,2), xend=c(-2.9,-0.9,0.9,2.9), y=0.05, yend=0.5)
  

ggsave(
  plot     = plot26,
  filename = "26_nonzero_distribution_alpha.svg",
  path     = plotdir,
  scale    = 0.6,
  height   = 3,
  width    = 7
)

## 6. H0H1 -----------------------------------------------------------------------------

layout <- "
ABC
DEF
"

p1 <- ggplot() +
  scale_x_continuous(name="X", limits=c(-3,3), breaks=0, labels=expression(mu[0])) +
  scale_y_continuous(limits=c(0, .5), expand=expansion(0)) +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  # annotation
  geom_segment(aes(x=0, xend=0, y=0, yend=0.3), linetype="dashed") +
  geom_segment(aes(x=0, xend=-3, y=0.38, yend=0.38), arrow=arrow(length=unit(0.4, "lines"), type="closed")) +
  geom_segment(aes(x=0, xend=0, y=0.38, yend=0.30)) +
  geom_segment(aes(x=.05, xend=3, y=0.18, yend=0.18), arrow=arrow(length=unit(0.4, "lines"), type="closed"), color=c[5]) +
  annotate("label", hjust=.5, vjust=1,
           x=-1.5, y=0.5,
           label.size=NA, fill="white", size=3,
           label=expression(paste(H[0]))) +
  annotate("label", hjust=0.5, vjust=1,
           x=1.5, y=0.5,
           label.size=NA, fill="white", size=3,
           label=expression(paste(H[1])),
           color=c[5]) +
  ggtitle(expression(paste(H[0],": ",mu<=mu[0],",  ",H[1],": ",mu>mu[0])))

p2 <- ggplot() +
  scale_x_continuous(name="X", limits=c(-3,3), breaks=0, labels=expression(mu[0])) +
  scale_y_continuous(limits=c(0, .5), expand=expansion(0)) +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  # annotation
  geom_segment(aes(x=0, xend=0, y=0, yend=0.3), linetype="dashed") +
  geom_segment(aes(x=0, xend=3, y=0.38, yend=0.38), arrow=arrow(length=unit(0.4, "lines"), type="closed")) +
  geom_segment(aes(x=0, xend=0, y=0.38, yend=0.30)) +
  geom_segment(aes(x=-.05, xend=-3, y=0.18, yend=0.18), arrow=arrow(length=unit(0.4, "lines"), type="closed"), color=c[5]) +
  annotate("label", hjust=.5, vjust=1,
           x=-1.5, y=0.5,
           label.size=NA, fill="white", size=3,
           label=expression(paste(H[1])),
           color=c[5]) +
  annotate("label", hjust=0.5, vjust=1,
           x=1.5, y=0.5,
           label.size=NA, fill="white", size=3,
           label=expression(paste(H[0]))) +
  ggtitle(expression(paste(H[0],": ",mu>=mu[0],",  ",H[1],": ",mu<mu[0])))

p3 <- ggplot() +
  scale_x_continuous(name="X", limits=c(-3,3), breaks=0, labels=expression(mu[0])) +
  scale_y_continuous(limits=c(0, .5), expand=expansion(0)) +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  # annotation
  geom_segment(aes(x=0, xend=0, y=0, yend=0.3), linetype="dashed") +
  geom_segment(aes(x=.05, xend=3, y=0.18, yend=0.18), arrow=arrow(length=unit(0.4, "lines"), type="closed"), color=c[5]) +
  geom_segment(aes(x=-.05, xend=-3, y=0.18, yend=0.18), arrow=arrow(length=unit(0.4, "lines"), type="closed"), color=c[5]) +
  annotate("label", hjust=.5, vjust=1,
           x=-1.5, y=0.5,
           label.size=NA, fill="white", size=3,
           label=expression(paste(H[1])),
           color=c[5]) +
  annotate("label", hjust=0.5, vjust=1,
           x=1.5, y=0.5,
           label.size=NA, fill="white", size=3,
           label=expression(paste(H[1])),
           color=c[5]) +
  annotate("label", hjust=.5, vjust=1,
           x=0.05, y=0.5,
           label.size=NA, fill="white", size=2.5,
           label=expression(paste(H[0]))) +
  geom_segment(aes(x=0, xend=0, y=0.15, yend=0.05), arrow=arrow(length=unit(0.2, "lines"), type="closed")) +
  ggtitle(expression(paste(H[0],": ",mu==mu[0],",  ",H[1],": ",mu!=mu[0])))

p4 <- getnd(0, 1/sqrt(15)) %>%
  ggplot() +
  geom_area(aes(x,y), 
            color=c[2], fill=c[2]) +
  geom_area(aes(ifelse(x > qnorm(0.05, 0, 1/sqrt(15), lower.tail=F), x, NA), y),
            color=c[4], fill=c[4]) +
  scale_y_continuous(limits=c(0, 2.05), expand=expansion(0)) +
  scale_x_continuous(name=expression(paste("Distance from ",mu[0])),
                     limits=c(-3, 3), breaks=-2:2, 
                     expand=expansion(0)) +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank()
  ) +
  # annotations
  geom_vline(xintercept=0, linetype="dashed") +
  geom_vline(xintercept=qnorm(0.05, 0, 1/sqrt(15), lower.tail=F)) +
  geom_segment(color="white", size=2, x=0, xend=0, 
               y=dnorm(0,0,1/sqrt(15))+0.01, yend=2.5) +
  annotate("label", hjust=0, vjust=1,
           x=qnorm(0.05, 0, 1/sqrt(15), lower.tail=F)+0.1, y=2.05,
           label.size=NA, fill="white", size=2.5,
           label=expression(paste("icw ",H[0]))) +
  annotate("label", hjust=1, vjust=1,
           x=qnorm(0.05, 0, 1/sqrt(15), lower.tail=F)-0.1, y=2.05,
           label.size=NA, fill="white", size=2.5,
           label=expression(paste("cw ",H[0]))) +
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0.9, ymin=0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0.8, 
           label=expression(paste(alpha,"=0.05=")))

p5 <- getnd(0, 1/sqrt(15)) %>%
  ggplot() +
  geom_area(aes(x,y), 
            color=c[2], fill=c[2]) +
  geom_area(aes(ifelse(x < qnorm(0.05, 0, 1/sqrt(15), lower.tail=T), x, NA), y),
            color=c[4], fill=c[4]) +
  scale_y_continuous(limits=c(0, 2.05), expand=expansion(0)) +
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3, 3), breaks=-2:2, 
                     expand=expansion(0)) +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank()
  ) +
  # annotations
  geom_vline(xintercept=0, linetype="dashed") +
  geom_vline(xintercept=qnorm(0.05, 0, 1/sqrt(15), lower.tail=T)) +
  geom_segment(color="white", size=2, x=0, xend=0, 
               y=dnorm(0,0,1/sqrt(15))+0.01, yend=2.5) +
  annotate("label", hjust=1, vjust=1,
           x=qnorm(0.05, 0, 1/sqrt(15), lower.tail=T)-0.1, y=2.05,
           label.size=NA, fill="white", size=2.5,
           label=expression(paste("icw ",H[0]))) +
  annotate("label", hjust=0, vjust=1,
           x=qnorm(0.05, 0, 1/sqrt(15), lower.tail=T)+0.1, y=2.05,
           label.size=NA, fill="white", size=2.5,
           label=expression(paste("cw ",H[0]))) +
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0.9, ymin=0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0.8, 
           label=expression(paste(alpha,"=0.05=")))

p6 <- getnd(0, 1/sqrt(15)) %>%
  ggplot() +
  geom_area(aes(x,y), 
            color=c[2], fill=c[2]) +
  geom_area(aes(ifelse(x < qnorm(0.025, 0, 1/sqrt(15), lower.tail=T), x, NA), y),
            color=c[4], fill=c[4]) +
  geom_area(aes(ifelse(x > qnorm(0.025, 0, 1/sqrt(15), lower.tail=F), x, NA), y),
            color=c[4], fill=c[4]) +
  scale_y_continuous(limits=c(0, 2.05), expand=expansion(0)) +
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3, 3), breaks=-2:2, 
                     expand=expansion(0)) +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank()
  ) +
  # annotations
  geom_vline(xintercept=0, linetype="dashed") +
  annotate("label", hjust=1, vjust=1,
           x=qnorm(0.025, 0, 1/sqrt(15), lower.tail=T)-0.1, y=2.05,
           label.size=NA, fill="white", size=2.5,
           label=expression(paste("icw ",H[0]))) +
  annotate("label", hjust=0, vjust=1,
           x=qnorm(0.025, 0, 1/sqrt(15), lower.tail=F)+0.1, y=2.05,
           label.size=NA, fill="white", size=2.5,
           label=expression(paste("icw ",H[0]))) +
  geom_segment(color="white", size=2, x=0, xend=0, 
               y=dnorm(0,0,1/sqrt(15))+0.01, yend=2.5) +
  annotate("label", hjust=0.5, vjust=1,
           x=0, y=2.05,
           label.size=NA, fill="white", size=2.5,
           label=expression(paste("cw ",H[0]))) +
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0.9, ymin=0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0.8, 
           label=expression(paste(alpha,"=0.05="))) +
  geom_vline(xintercept=qnorm(0.025, 0, 1/sqrt(15), lower.tail=T)) +
  geom_vline(xintercept=qnorm(0.025, 0, 1/sqrt(15), lower.tail=F)) 

plot6 <- p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(design=layout)

ggsave(
  plot     = plot6,
  filename = "06_h0h1.svg",
  path     = plotdir,
  scale    = 0.6,
  height   = 4.5,
  width    = 10
)

## 7 p value ----------------------------------------------------------------------------

layout <- "
ABC
DEF
"

p1 <-  ggplot() +
  geom_jitter(aes(x=sample, y=0), height=0.1,
              shape=21, size=1.5, color=c[5], fill=c[5]) +
  geom_vline(xintercept=mean(sample)) +
  geom_point(aes(x=mean(sample), y=0),
             shape=22, size=2.5, color=c[2], fill=c[2]) +
  scale_x_continuous(limits=c(-3,3), breaks=-2:2, expand=expansion(0),
                     name=expression(paste(X))) +
  scale_y_continuous(limits=c(-.5,.5)) +
  coord_fixed(ratio=0.5) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  )

p4 <- getnd(0, 1/sqrt(15)) %>%
  ggplot() +
  geom_area(aes(x,y), 
            color=c[2], fill=c[2]) +
  geom_area(aes(ifelse(x > mean(sample), x, NA), y),
            color=c[4], fill=c[4]) +
  geom_vline(xintercept=0, linetype="dashed") +
  geom_vline(xintercept=mean(sample)) +
  scale_y_continuous(limits=c(0, 2), expand=expansion(0)) +
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3,3), breaks=-2:2, 
                     expand=expansion(0)) +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank()
  ) +
  # annotations
  annotate("label", hjust=0, vjust=1,
           x=mean(sample)+0.1, y=2,
           label.size=NA, fill="white", size=2.5,
           label=expression(paste("as large / larger"))) +
  annotate("label", hjust=1, vjust=1,
           x=mean(sample)-0.1, y=2,
           label.size=NA, fill="white", size=2.5,
           label=expression(paste("smaller"))) +
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0.9, ymin=0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0.8, 
           label=expression(paste("p=0.10=")))

p5 <- getnd(0, 1/sqrt(15)) %>%
  ggplot() +
  geom_area(aes(x,y), 
            color=c[2], fill=c[2]) +
  geom_area(aes(ifelse(x < mean(sample), x, NA), y),
            color=c[4], fill=c[4]) +
  geom_vline(xintercept=0, linetype="dashed") +
  geom_vline(xintercept=mean(sample)) +
  scale_y_continuous(limits=c(0, 2), expand=expansion(0)) +
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3,3), breaks=-2:2, 
                     expand=expansion(0)) +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank()
  ) +
  # annotations
  annotate("label", hjust=0, vjust=1,
           x=mean(sample)+0.1, y=2,
           label.size=NA, fill="white", size=2.5,
           label=expression(paste("larger"))) +
  annotate("label", hjust=1, vjust=1,
           x=mean(sample)-0.1, y=2,
           label.size=NA, fill="white", size=2.5,
           label=expression(paste("as small / smaller"))) +
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0.9, ymin=0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0.8, 
           label=expression(paste("p=0.90=")))

p6 <- getnd(0, 1/sqrt(15)) %>%
  ggplot() +
  geom_area(aes(x,y), 
            color=c[2], fill=c[2]) +
  geom_area(aes(ifelse(x > mean(sample), x, NA), y),
            color=c[4], fill=c[4]) +
  geom_area(aes(ifelse(x < -mean(sample), x, NA), y),
            color=c[4], fill=c[4]) +
  geom_vline(xintercept=0, linetype="dashed") +
  geom_vline(xintercept=mean(sample)) +
  geom_vline(xintercept=-mean(sample), linetype="dotted") +
  scale_y_continuous(limits=c(0, 2), expand=expansion(0)) +
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3,3), breaks=-2:2, 
                     expand=expansion(0)) +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank()
  ) +
  # annotations
  geom_segment(color="white", size=2, x=0, xend=0, 
               y=dnorm(0,0,1/sqrt(15))+0.01, yend=2.5) +
  annotate("label", hjust=.5, vjust=1,
           x=0, y=2,
           label.size=NA, fill=NA, size=2.5,
           label=expression(paste("clsr"))) +
  annotate("label", hjust=0, vjust=1,
           x=mean(sample)+0.1, y=2,
           label.size=NA, fill="white", size=2.5,
           label=expression(paste("...from ", mu[0]))) +
  annotate("label", hjust=1, vjust=1,
           x=-mean(sample)-0.1, y=2,
           label.size=NA, fill="white", size=2.5,
           label=expression(paste("as far / farther..."))) +
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0.9, ymin=0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0.8, 
           label=expression(paste("p=0.20=")))

plot7 <- p1 + p1 + p1 + p4 + p5 + p6 + plot_layout(design=layout)

ggsave(
  plot     = plot7,
  filename = "07_pvalue.svg",
  path     = plotdir,
  scale    = 0.6,
  height   = 4.3,
  width    = 10
)

# 14. P Curves --------------------------------------------------------------------------

layout <- "
ABC
DEF
GHI
"

p1 <-  ggplot() +
  geom_jitter(aes(x=sample, y=0), height=0.1,
              shape=21, size=1.5, color=c[5], fill=c[5]) +
  geom_vline(xintercept=mean(sample)) +
  geom_point(aes(x=mean(sample), y=0),
             shape=22, size=2.5, color=c[2], fill=c[2]) +
  scale_x_continuous(limits=c(-3,3), breaks=-2:2, expand=expansion(0),
                     name=expression(paste(X))) +
  scale_y_continuous(limits=c(-.5,.5)) +
  coord_fixed(ratio=0.5) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  )

p4 <- getnd(0, 1/sqrt(15)) %>% 
  mutate(x0 = x - 0.5,
         x1 = x,
         x2 = x + 0.5,
         y0 = y,
         y1 = y + 1*1.7,
         y2 = y + 2*1.7) %>% 
  ggplot() +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M", limits=c(-3,3), breaks=-2:2, expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 2*1.7+1.7), expand=expansion(0)) +
  # data mu0 = 0.5
  geom_ribbon(aes(x=x2, ymax=y2, ymin=2*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x2 >= mean(sample), x2, 11111), ymax=y2, ymin=2*1.7),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               y=2*1.7+1.7, yend=0, x=0.5, xend=0.5) +
  # data mu0 = 0
  geom_ribbon(aes(x=x1, ymax=y1, ymin=1*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x1 >= mean(sample), x1, 11111), ymax=y1, ymin=1*1.7),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               y=1*1.7+1.7, yend=0, x=0, xend=0) +
  # data mu = -0.5
  geom_ribbon(aes(x=x0, ymax=y0, ymin=0*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x0 >= mean(sample), x0, 11111), ymax=y0, ymin=0*1.7),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               y=0*1.7+1.7, yend=0, x=-0.5, xend=-0.5) +
  # lines
  geom_vline(linetype="solid",
             xintercept=mean(sample)) +
  geom_hline(yintercept=1.7*0:2) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=2*1.7+0.9, ymin=2*1.7+0.7) +
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=1*1.7+0.9, ymin=1*1.7+0.7) +
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=2*1.7+0.8, 
           label=expression(paste("p=0.74="))) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=1*1.7+0.8, 
           label=expression(paste("p=0.10="))) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste("p=0.00="))) 

p5 <- getnd(0, 1/sqrt(15)) %>% 
  mutate(x0 = x - 0.5,
         x1 = x,
         x2 = x + 0.5,
         y0 = y,
         y1 = y + 1*1.7,
         y2 = y + 2*1.7) %>% 
  ggplot() +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M", limits=c(-3,3), breaks=-2:2, expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 2*1.7+1.7), expand=expansion(0)) +
  # data mu0 = 0.5
  geom_ribbon(aes(x=x2, ymax=y2, ymin=2*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x2 <= mean(sample), x2, 11111), ymax=y2, ymin=2*1.7),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               y=2*1.7+1.7, yend=0, x=0.5, xend=0.5) +
  # data mu0 = 0
  geom_ribbon(aes(x=x1, ymax=y1, ymin=1*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x1 <= mean(sample), x1, 11111), ymax=y1, ymin=1*1.7),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               y=1*1.7+1.7, yend=0, x=0, xend=0) +
  # data mu = -0.5
  geom_ribbon(aes(x=x0, ymax=y0, ymin=0*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x0 <= mean(sample), x0, 11111), ymax=y0, ymin=0*1.7),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               y=0*1.7+1.7, yend=0, x=-0.5, xend=-0.5) +
  # lines
  geom_vline(linetype="solid",
             xintercept=mean(sample)) +
  geom_hline(yintercept=1.7*0:2) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=2*1.7+0.9, ymin=2*1.7+0.7) +
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=1*1.7+0.9, ymin=1*1.7+0.7) +
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=2*1.7+0.8, 
           label=expression(paste("p=0.26="))) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=1*1.7+0.8, 
           label=expression(paste("p=0.90="))) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste("p=1.00="))) 

p6 <- getnd(0, 1/sqrt(15)) %>% 
  mutate(x0 = x - 0.5,
         x1 = x,
         x2 = x + 0.5,
         y0 = y,
         y1 = y + 1*1.7,
         y2 = y + 2*1.7) %>% 
  ggplot() +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M", limits=c(-3,3), breaks=-2:2, expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 2*1.7+1.7), expand=expansion(0)) +
  # data mu0 = 0.5
  geom_ribbon(aes(x=x2, ymax=y2, ymin=2*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x2 >= 0.5+abs(0.5-mean(sample)), x2, 11111), ymax=y2, ymin=2*1.7),
              color=c[4], fill=c[4]) +
  geom_ribbon(aes(x=ifelse(x2 <= 0.5-abs(0.5-mean(sample)), x2, 11111), ymax=y2, ymin=2*1.7),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               y=2*1.7+1.7, yend=0, x=0.5, xend=0.5) +
  # data mu0 = 0
  geom_ribbon(aes(x=x1, ymax=y1, ymin=1*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x1 >= mean(sample), x1, 11111), ymax=y1, ymin=1*1.7),
              color=c[4], fill=c[4]) +
  geom_ribbon(aes(x=ifelse(x1 <= -mean(sample), x1, 11111), ymax=y1, ymin=1*1.7),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               y=1*1.7+1.7, yend=0, x=0, xend=0) +
  # data mu = -0.5
  geom_ribbon(aes(x=x0, ymax=y0, ymin=0*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x0 >= -0.5+abs(-0.5-mean(sample)), x0, 11111), ymax=y0, ymin=0*1.7),
              color=c[4], fill=c[4]) +
  geom_ribbon(aes(x=ifelse(x0 <= -0.5-abs(-0.5-mean(sample)), x0, 11111), ymax=y0, ymin=0*1.7),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               y=0*1.7+1.7, yend=0, x=-0.5, xend=-0.5) +
  # lines
  geom_vline(linetype="solid",
             xintercept=mean(sample)) +
  geom_hline(yintercept=1.7*0:2) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=2*1.7+0.9, ymin=2*1.7+0.7) +
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=1*1.7+0.9, ymin=1*1.7+0.7) +
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=2*1.7+0.8, 
           label=expression(paste("p=0.52="))) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=1*1.7+0.8, 
           label=expression(paste("p=0.20="))) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste("p=0.00=")))

p7 <- data.frame(mu0 = seq(-3, 3, 0.001)) %>% 
  mutate(y = pnorm(mean(sample), mu0, 1/sqrt(15), lower.tail=F)) %>%
  ggplot(aes(mu0, y)) +
  geom_line(size=1, color=c[4]) +
  # axes
  scale_x_continuous(name=expression(paste("Distance from", mu[0])),
                     limits=c(-3, 3), breaks=-2:2, expand=expansion(0)) +
  scale_y_continuous(name="p",
                     limits=c(0,1), breaks=seq(0,1,0.2)) +
  # annotations
  geom_vline(xintercept=c(0.5,0,-0.5), linetype="dashed") +
  geom_point(aes(x=0.5, y=pnorm(mean(sample), 0.5, 1/sqrt(15), lower.tail=F)),
             shape=22, color=c[4], fill=c[4], size=2) +
  geom_point(aes(x=0, y=pnorm(mean(sample), 0, 1/sqrt(15), lower.tail=F)),
             shape=22, color=c[4], fill=c[4], size=2) +
  geom_point(aes(x=-0.5, y=pnorm(mean(sample), -0.5, 1/sqrt(15), lower.tail=F)),
             shape=22, color=c[4], fill=c[4], size=2) +
  # theme
  theme_blog() + theme(
    aspect.ratio=0.5,
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_text(angle=0)
  )

p8 <- data.frame(mu0 = seq(-3, 3, 0.001)) %>% 
  mutate(y = pnorm(mean(sample), mu0, 1/sqrt(15), lower.tail=T)) %>%
  ggplot(aes(mu0, y)) +
  geom_line(size=1, color=c[4]) +
  # axes
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3, 3), breaks=-2:2, expand=expansion(0)) +
  scale_y_continuous(name="p",
                     limits=c(0,1), breaks=seq(0,1,0.2)) +
  # annotations
  geom_vline(xintercept=c(0.5,0,-0.5), linetype="dashed") +
  geom_point(aes(x=0.5, y=pnorm(mean(sample), 0.5, 1/sqrt(15), lower.tail=T)),
             shape=22, color=c[4], fill=c[4], size=2) +
  geom_point(aes(x=0, y=pnorm(mean(sample), 0, 1/sqrt(15), lower.tail=T)),
             shape=22, color=c[4], fill=c[4], size=2) +
  geom_point(aes(x=-0.5, y=pnorm(mean(sample), -0.5, 1/sqrt(15), lower.tail=T)),
             shape=22, color=c[4], fill=c[4], size=2) +
  # theme
  theme_blog() + theme(
    aspect.ratio=0.5,
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )

p9 <- data.frame(mu0 = seq(-3, 3, 0.001)) %>%
  rowwise() %>%  
  mutate(y = 2 * min(
    pnorm(mean(sample), mu0, 1/sqrt(15), lower.tail=T),
    pnorm(mean(sample), mu0, 1/sqrt(15), lower.tail=F)
  )) %>%
  ungroup() %>% 
  ggplot(aes(mu0, y)) +
  geom_line(size=1, color=c[4]) +
  # axes
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3, 3), breaks=-2:2, expand=expansion(0)) +
  scale_y_continuous(name="p",
                     limits=c(0,1), breaks=seq(0,1,0.2)) +
  # annotations
  geom_vline(xintercept=c(0.5,0,-0.5), linetype="dashed") +
  geom_point(aes(x=0.5, y=2*min(pnorm(mean(sample), 0.5, 1/sqrt(15), lower.tail=T), pnorm(mean(sample), 0.5, 1/sqrt(15), lower.tail=F))),
             shape=22, color=c[4], fill=c[4], size=2) +
  geom_point(aes(x=0, y=2*min(pnorm(mean(sample), 0, 1/sqrt(15), lower.tail=T), pnorm(mean(sample), 0, 1/sqrt(15), lower.tail=F))),
             shape=22, color=c[4], fill=c[4], size=2) +
  geom_point(aes(x=-0.5, y=2*min(pnorm(mean(sample), -0.5, 1/sqrt(15), lower.tail=T), pnorm(mean(sample), -0.5, 1/sqrt(15), lower.tail=F))),
             shape=22, color=c[4], fill=c[4], size=2) +
  # theme
  theme_blog() + theme(
    aspect.ratio=0.5,
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )

plot14 <- p1 + p1 + p1 + p4 + p5 + p6 + p7 + p8 + p9 + plot_layout(design=layout)

ggsave(
  plot     = plot14,
  filename = "14_p_curves.svg",
  path     = plotdir,
  scale    = 0.6,
  height   = 8,
  width    = 10
)

# 15. Confidence Intervals --------------------------------------------------------------

layout <- "
ABC
DEF
GHI
"

p1 <-  ggplot() +
  geom_jitter(aes(x=sample, y=0), height=0.1,
              shape=21, size=1.5, color=c[5], fill=c[5]) +
  geom_vline(xintercept=mean(sample)) +
  geom_point(aes(x=mean(sample), y=0),
             shape=22, size=2.5, color=c[2], fill=c[2]) +
  scale_x_continuous(limits=c(-3,3), breaks=-2:2, expand=expansion(0),
                     name=expression(paste(X))) +
  scale_y_continuous(limits=c(-.5,.5)) +
  coord_fixed(ratio=0.5) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  )

p4 <- getnd(0, 1/sqrt(15)) %>% 
  mutate(x0 = x + mean(sample) - qnorm(0.95)/sqrt(15),
         x1 = x + mean(sample) + qnorm(0.95)/sqrt(15),
         y0 = y,
         y1 = y + 1.7) %>%
  ggplot() +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M", limits=c(-3,3), breaks=-2:2, expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 1.7+1.7), expand=expansion(0)) +
  # data mu0 = lower bound
  geom_ribbon(aes(x=x0, ymax=y1, ymin=1*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x0 >= mean(sample), x0, NA), ymax=y1, ymin=1*1.7),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               y=1*1.7+1.7, yend=0, x=mean(sample)-qnorm(0.95)/sqrt(15), xend=mean(sample)-qnorm(0.95)/sqrt(15)) +
  # lines
  geom_vline(linetype="solid",
             xintercept=mean(sample)) +
  geom_hline(yintercept=1.7) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=1*1.7+0.9, ymin=1*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=1*1.7+0.8, 
           label=expression(paste("p=",alpha,"=")))

p5 <- getnd(0, 1/sqrt(15)) %>% 
  mutate(x0 = x + mean(sample) - qnorm(0.95)/sqrt(15),
         x1 = x + mean(sample) + qnorm(0.95)/sqrt(15),
         y0 = y,
         y1 = y + 1.7) %>%
  ggplot() +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M", limits=c(-3,3), breaks=-2:2, expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 1.7+1.7), expand=expansion(0)) +
  # data mu0 = upper bound
  geom_ribbon(aes(x=x1, ymax=y0, ymin=0*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x1 <= mean(sample), x1, NA), ymax=y0, ymin=0*1.7),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               y=0*1.7+1.7, yend=0, x=mean(sample)+qnorm(0.95)/sqrt(15), xend=mean(sample)+qnorm(0.95)/sqrt(15)) +
  # lines
  geom_vline(linetype="solid",
             xintercept=mean(sample)) +
  geom_hline(yintercept=1.7) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste("p=",alpha,"=")))


p6 <- getnd(0, 1/sqrt(15)) %>% 
  mutate(x0 = x + mean(sample) - qnorm(0.975)/sqrt(15),
         x1 = x + mean(sample) + qnorm(0.975)/sqrt(15),
         y0 = y,
         y1 = y + 1.7) %>%
  ggplot() +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M", limits=c(-3,3), breaks=-2:2, expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 1.7+1.7), expand=expansion(0)) +
  # data mu0 = upper bound
  geom_ribbon(aes(x=x1, ymax=y0, ymin=0*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x1 >= mean(sample)+2*qnorm(0.975)/sqrt(15), x1, NA), ymax=y0, ymin=0*1.7),
              color=c[4], fill=c[4]) +
  geom_ribbon(aes(x=ifelse(x1 <= mean(sample), x1, NA), ymax=y0, ymin=0*1.7),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               y=0*1.7+1.7, yend=0, x=mean(sample)+qnorm(0.975)/sqrt(15), xend=mean(sample)+qnorm(0.975)/sqrt(15)) +
  # data mu0 = lower bound
  geom_ribbon(aes(x=x0, ymax=y1, ymin=1*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x0 >= mean(sample), x0, NA), ymax=y1, ymin=1*1.7),
              color=c[4], fill=c[4]) +
  geom_ribbon(aes(x=ifelse(x0 <= mean(sample)-2*qnorm(0.975)/sqrt(15), x0, NA), ymax=y1, ymin=1*1.7),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               y=1*1.7+1.7, yend=0, x=mean(sample)-qnorm(0.975)/sqrt(15), xend=mean(sample)-qnorm(0.975)/sqrt(15)) +
  # lines
  geom_vline(linetype="solid",
             xintercept=mean(sample)) +
  geom_hline(yintercept=1.7) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=1*1.7+0.9, ymin=1*1.7+0.7) +
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=1*1.7+0.8, 
           label=expression(paste("p=",alpha,"="))) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste("p=",alpha,"=")))

p7 <- data.frame(mu0 = seq(-3, 3, 0.001)) %>% 
  mutate(y = pnorm(mean(sample), mu0, 1/sqrt(15), lower.tail=F)) %>%
  ggplot(aes(mu0, y)) +
  geom_line(size=1, color=c[4]) +
  # axes
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3, 3), breaks=-2:2, expand=expansion(0)) +
  scale_y_continuous(name="p",
                     limits=c(0,1), breaks=seq(0,1,0.2)) +
  # annotations
  geom_vline(xintercept=mean(sample)-qnorm(0.95)/sqrt(15), linetype="dashed") +
  geom_hline(yintercept=0.05, linetype="dotted") +
  geom_segment(y=0.05, yend=0.05, x=mean(sample)-qnorm(0.95)/sqrt(15), xend=2.8,
               color=c[2], size=1.25, arrow=arrow(length=unit(0.4, "lines"), type="open")) +
  annotate("text", label=expression(paste(alpha==0.05)),
           hjust=0, vjust=0, x=-2.9, y=0.06, size=3) +
  # theme
  theme_blog() + theme(
    aspect.ratio=0.5,
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_text(angle=0)
  )

p8 <- data.frame(mu0 = seq(-3, 3, 0.001)) %>% 
  mutate(y = pnorm(mean(sample), mu0, 1/sqrt(15), lower.tail=T)) %>%
  ggplot(aes(mu0, y)) +
  geom_line(size=1, color=c[4]) +
  # axes
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3, 3), breaks=-2:2, expand=expansion(0)) +
  scale_y_continuous(name="p",
                     limits=c(0,1), breaks=seq(0,1,0.2)) +
  # annotations
  geom_vline(xintercept=mean(sample)+qnorm(0.95)/sqrt(15), linetype="dashed") +
  geom_hline(yintercept=0.05, linetype="dotted") +
  geom_segment(y=0.05, yend=0.05, x=mean(sample)+qnorm(0.95)/sqrt(15), xend=-2.8,
               color=c[2], size=1.25, arrow=arrow(length=unit(0.4, "lines"), type="open")) +
  annotate("text", label=expression(paste(alpha==0.05)),
           hjust=0, vjust=0, x=-2.9, y=0.06, size=3) +
  # theme
  theme_blog() + theme(
    aspect.ratio=0.5,
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )

p9 <- data.frame(mu0 = seq(-3, 3, 0.001)) %>% 
  rowwise() %>% 
  mutate(y = 2 * min(
    pnorm(mean(sample), mu0, 1/sqrt(15), lower.tail=T),
    pnorm(mean(sample), mu0, 1/sqrt(15), lower.tail=F)
  )) %>%
  ungroup() %>% 
  ggplot(aes(mu0, y)) +
  geom_line(size=1, color=c[4]) +
  # axes
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3, 3), breaks=-2:2, expand=expansion(0)) +
  scale_y_continuous(name="p",
                     limits=c(0,1), breaks=seq(0,1,0.2),) +
  # annotations
  geom_vline(xintercept=mean(sample) + c(1,-1) * qnorm(0.975)/sqrt(15), linetype="dashed") +
  geom_hline(yintercept=0.05, linetype="dotted") +
  geom_segment(y=0.05, yend=0.05, x=mean(sample)-qnorm(0.975)/sqrt(15), xend=mean(sample)+qnorm(0.975)/sqrt(15),
               color=c[2], size=1.25) +
  annotate("text", label=expression(paste(alpha==0.05)),
           hjust=0, vjust=0, x=-2.9, y=0.06, size=3) +
  # theme
  theme_blog() + theme(
    aspect.ratio=0.5,
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ) 


plot15 <- p1 + p1 + p1 + p4 + p5 + p6 + p7 + p8 + p9 + plot_layout(design=layout) 

ggsave(
  plot     = plot15,
  filename = "15_confidence_intervals.svg",
  path     = plotdir,
  scale    = 0.6,
  height   = 7,
  width    = 10
)

# 16. Confidence Intervaly by N ---------------------------------------------------------

layout <- "
ABC
DEF
GHI
"

p1 <-  ggplot() +
  geom_jitter(aes(x=sample1, y=0), height=0.1,
              shape=21, size=1.5, color=c[5], fill=c[5]) +
  geom_vline(xintercept=mean(sample)) +
  geom_point(aes(x=mean(sample), y=0),
             shape=22, size=2.5, color=c[1], fill=c[1]) +
  scale_x_continuous(limits=c(-3,3), breaks=-2:2, expand=expansion(0),
                     name=expression(paste(X))) +
  scale_y_continuous(limits=c(-.5,.5)) +
  coord_fixed(ratio=0.5) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  ggtitle(expression(N==5)) + 
  theme(plot.title=element_text(hjust=0.5))

p2 <-  ggplot() +
  geom_jitter(aes(x=sample, y=0), height=0.1,
              shape=21, size=1.5, color=c[5], fill=c[5]) +
  geom_vline(xintercept=mean(sample)) +
  geom_point(aes(x=mean(sample), y=0),
             shape=22, size=2.5, color=c[2], fill=c[2]) +
  scale_x_continuous(limits=c(-3,3), breaks=-2:2, expand=expansion(0),
                     name=expression(paste(X))) +
  scale_y_continuous(limits=c(-.5,.5)) +
  coord_fixed(ratio=0.5) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  ggtitle(expression(N==15)) + 
  theme(plot.title=element_text(hjust=0.5))

p3 <-  ggplot() +
  geom_jitter(aes(x=sample3, y=0), height=0.1,
              shape=21, size=1.5, color=c[5], fill=c[5]) +
  geom_vline(xintercept=mean(sample)) +
  geom_point(aes(x=mean(sample), y=0),
             shape=22, size=2.5, color=c[3], fill=c[3]) +
  scale_x_continuous(limits=c(-3,3), breaks=-2:2, expand=expansion(0),
                     name=expression(paste(X))) +
  scale_y_continuous(limits=c(-.5,.5)) +
  coord_fixed(ratio=0.5) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  ggtitle(expression(N==50)) + 
  theme(plot.title=element_text(hjust=0.5))

p4 <- getnd(0, 1/sqrt(5)) %>% 
  mutate(x0 = x + mean(sample) - qnorm(0.975)/sqrt(5),
         x1 = x + mean(sample) + qnorm(0.975)/sqrt(5),
         y0 = y,
         y1 = y + 2.83) %>%
  ggplot() +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M", limits=c(-3,3), breaks=-2:2, expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 2.83+2.83), expand=expansion(0)) +
  # data mu0 = upper bound
  geom_ribbon(aes(x=x1, ymax=y0, ymin=0*2.83),
              color=c[1], fill=c[1]) +
  geom_ribbon(aes(x=ifelse(x1 >= mean(sample)+2*qnorm(0.975)/sqrt(5), x1, NA), ymax=y0, ymin=0*2.83),
              color=c[4], fill=c[4]) +
  geom_ribbon(aes(x=ifelse(x1 <= mean(sample), x1, NA), ymax=y0, ymin=0*2.83),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               y=0*2.83+2.83, yend=0, x=mean(sample)+qnorm(0.975)/sqrt(5), xend=mean(sample)+qnorm(0.975)/sqrt(5)) +
  # data mu0 = lower bound
  geom_ribbon(aes(x=x0, ymax=y1, ymin=1*2.83),
              color=c[1], fill=c[1]) +
  geom_ribbon(aes(x=ifelse(x0 >= mean(sample), x0, NA), ymax=y1, ymin=1*2.83),
              color=c[4], fill=c[4]) +
  geom_ribbon(aes(x=ifelse(x0 <= mean(sample)-2*qnorm(0.975)/sqrt(5), x0, NA), ymax=y1, ymin=1*2.83),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               y=1*2.83+2.83, yend=0, x=mean(sample)-qnorm(0.975)/sqrt(5), xend=mean(sample)-qnorm(0.975)/sqrt(5)) +
  # lines
  geom_vline(linetype="solid",
             xintercept=mean(sample)) +
  geom_hline(yintercept=2.83) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=1*2.83+0.9, ymin=1*2.83+0.7) +
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*2.83+0.9, ymin=0*2.83+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=1*2.83+0.8, 
           label=expression(paste("p=",alpha,"="))) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*2.83+0.8, 
           label=expression(paste("p=",alpha,"=")))

p5 <- getnd(0, 1/sqrt(15)) %>% 
  mutate(x0 = x + mean(sample) - qnorm(0.975)/sqrt(15),
         x1 = x + mean(sample) + qnorm(0.975)/sqrt(15),
         y0 = y,
         y1 = y + 2.83) %>%
  ggplot() +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M", limits=c(-3,3), breaks=-2:2, expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 2.83+2.83), expand=expansion(0)) +
  # data mu0 = upper bound
  geom_ribbon(aes(x=x1, ymax=y0, ymin=0*2.83),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x1 >= mean(sample)+2*qnorm(0.975)/sqrt(15), x1, NA), ymax=y0, ymin=0*2.83),
              color=c[4], fill=c[4]) +
  geom_ribbon(aes(x=ifelse(x1 <= mean(sample), x1, NA), ymax=y0, ymin=0*2.83),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               y=0*2.83+2.83, yend=0, x=mean(sample)+qnorm(0.975)/sqrt(15), xend=mean(sample)+qnorm(0.975)/sqrt(15)) +
  # data mu0 = lower bound
  geom_ribbon(aes(x=x0, ymax=y1, ymin=1*2.83),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x0 >= mean(sample), x0, NA), ymax=y1, ymin=1*2.83),
              color=c[4], fill=c[4]) +
  geom_ribbon(aes(x=ifelse(x0 <= mean(sample)-2*qnorm(0.975)/sqrt(15), x0, NA), ymax=y1, ymin=1*2.83),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               y=1*2.83+2.83, yend=0, x=mean(sample)-qnorm(0.975)/sqrt(15), xend=mean(sample)-qnorm(0.975)/sqrt(15)) +
  # lines
  geom_vline(linetype="solid",
             xintercept=mean(sample)) +
  geom_hline(yintercept=2.83) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=1*2.83+0.9, ymin=1*2.83+0.7) +
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*2.83+0.9, ymin=0*2.83+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=1*2.83+0.8, 
           label=expression(paste("p=",alpha,"="))) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*2.83+0.8, 
           label=expression(paste("p=",alpha,"=")))

p6 <- getnd(0, 1/sqrt(50)) %>% 
  mutate(x0 = x + mean(sample) - qnorm(0.975)/sqrt(50),
         x1 = x + mean(sample) + qnorm(0.975)/sqrt(50),
         y0 = y,
         y1 = y + 2.83) %>%
  ggplot() +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M", limits=c(-3,3), breaks=-2:2, expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 2.83+2.83), expand=expansion(0)) +
  # data mu0 = upper bound
  geom_ribbon(aes(x=x1, ymax=y0, ymin=0*2.83),
              color=c[3], fill=c[3]) +
  geom_ribbon(aes(x=ifelse(x1 >= mean(sample)+2*qnorm(0.975)/sqrt(50), x1, NA), ymax=y0, ymin=0*2.83),
              color=c[4], fill=c[4]) +
  geom_ribbon(aes(x=ifelse(x1 <= mean(sample), x1, NA), ymax=y0, ymin=0*2.83),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               y=0*2.83+2.83, yend=0, x=mean(sample)+qnorm(0.975)/sqrt(50), xend=mean(sample)+qnorm(0.975)/sqrt(50)) +
  # data mu0 = lower bound
  geom_ribbon(aes(x=x0, ymax=y1, ymin=1*2.83),
              color=c[3], fill=c[3]) +
  geom_ribbon(aes(x=ifelse(x0 >= mean(sample), x0, NA), ymax=y1, ymin=1*2.83),
              color=c[4], fill=c[4]) +
  geom_ribbon(aes(x=ifelse(x0 <= mean(sample)-2*qnorm(0.975)/sqrt(50), x0, NA), ymax=y1, ymin=1*2.83),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               y=1*2.83+2.83, yend=0, x=mean(sample)-qnorm(0.975)/sqrt(50), xend=mean(sample)-qnorm(0.975)/sqrt(50)) +
  # lines
  geom_vline(linetype="solid",
             xintercept=mean(sample)) +
  geom_hline(yintercept=2.83) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=1*2.83+0.9, ymin=1*2.83+0.7) +
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*2.83+0.9, ymin=0*2.83+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=1*2.83+0.8, 
           label=expression(paste("p=",alpha,"="))) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*2.83+0.8, 
           label=expression(paste("p=",alpha,"=")))

p7 <- data.frame(mu0 = seq(-3, 3, 0.001)) %>% 
  rowwise() %>% 
  mutate(y = 2 * min(
    pnorm(mean(sample), mu0, 1/sqrt(5), lower.tail=T),
    pnorm(mean(sample), mu0, 1/sqrt(5), lower.tail=F)
  )) %>%
  ungroup() %>% 
  ggplot(aes(mu0, y)) +
  geom_line(size=1, color=c[4]) +
  # axes
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3, 3), breaks=-2:2, expand=expansion(0)) +
  scale_y_continuous(name="p",
                     limits=c(0,1), breaks=seq(0,1,0.2),) +
  # annotations
  geom_vline(xintercept=mean(sample) + c(1,-1) * qnorm(0.975)/sqrt(5), linetype="dashed") +
  geom_hline(yintercept=0.05, linetype="dotted") +
  geom_segment(y=0.05, yend=0.05, x=mean(sample)-qnorm(0.975)/sqrt(5), xend=mean(sample)+qnorm(0.975)/sqrt(5),
               color=c[1], size=1.25) +
  annotate("text", label=expression(paste(alpha==0.05)),
           hjust=0, vjust=0, x=-2.9, y=0.06, size=3) +
  # theme
  theme_blog() + theme(
    aspect.ratio=0.5,
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_text(angle=0),
    #axis.text.y = element_blank()
  ) 

p8 <- data.frame(mu0 = seq(-3, 3, 0.001)) %>% 
  rowwise() %>% 
  mutate(y = 2 * min(
    pnorm(mean(sample), mu0, 1/sqrt(15), lower.tail=T),
    pnorm(mean(sample), mu0, 1/sqrt(15), lower.tail=F)
  )) %>%
  ungroup() %>% 
  ggplot(aes(mu0, y)) +
  geom_line(size=1, color=c[4]) +
  # axes
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3, 3), breaks=-2:2, expand=expansion(0)) +
  scale_y_continuous(name="p",
                     limits=c(0,1), breaks=seq(0,1,0.2),) +
  # annotations
  geom_vline(xintercept=mean(sample) + c(1,-1) * qnorm(0.975)/sqrt(15), linetype="dashed") +
  geom_hline(yintercept=0.05, linetype="dotted") +
  geom_segment(y=0.05, yend=0.05, x=mean(sample)-qnorm(0.975)/sqrt(15), xend=mean(sample)+qnorm(0.975)/sqrt(15),
               color=c[2], size=1.25) +
  annotate("text", label=expression(paste(alpha==0.05)),
           hjust=0, vjust=0, x=-2.9, y=0.06, size=3) +
  # theme
  theme_blog() + theme(
    aspect.ratio=0.5,
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ) 

p9 <- data.frame(mu0 = seq(-3, 3, 0.001)) %>% 
  rowwise() %>% 
  mutate(y = 2 * min(
    pnorm(mean(sample), mu0, 1/sqrt(50), lower.tail=T),
    pnorm(mean(sample), mu0, 1/sqrt(50), lower.tail=F)
  )) %>%
  ungroup() %>% 
  ggplot(aes(mu0, y)) +
  geom_line(size=1, color=c[4]) +
  # axes
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3, 3), breaks=-2:2, expand=expansion(0)) +
  scale_y_continuous(name="p",
                     limits=c(0,1), breaks=seq(0,1,0.2),) +
  # annotations
  geom_vline(xintercept=mean(sample) + c(1,-1) * qnorm(0.975)/sqrt(50), linetype="dashed") +
  geom_hline(yintercept=0.05, linetype="dotted") +
  geom_segment(y=0.05, yend=0.05, x=mean(sample)-qnorm(0.975)/sqrt(50), xend=mean(sample)+qnorm(0.975)/sqrt(50),
               color=c[3], size=1.25) +
  annotate("text", label=expression(paste(alpha==0.05)),
           hjust=0, vjust=0, x=-2.9, y=0.06, size=3) +
  # theme
  theme_blog() + theme(
    aspect.ratio=0.5,
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ) 


plot16 <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + plot_layout(design=layout)

ggsave(
  plot     = plot16,
  filename = "16_confidence_intervals_by_n.svg",
  path     = plotdir,
  scale    = 0.6,
  height   = 9,
  width    = 10
)

# 17. Rejection Rate --------------------------------------------------------------------

layout <- "
ABC
DEF
GHI
"

p1 <- getnd(0, 1/sqrt(15)) %>% 
  ggplot() +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M",
                     limits=c(-3, 3), breaks=-2:2,
                     expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 1.7), expand=expansion(0)) +
  # data
  geom_area(aes(x=x, y=y),
            color=c[2], fill=c[2]) +
  geom_area(aes(x=ifelse(x>=qnorm(0.05,0,1/sqrt(15),lower.tail=F), x, NA), y=y),
            color=c[4], fill=c[4]) +
  #lines
  geom_segment(linetype="dashed", x=0, xend=0, y=0, yend=1.7) +
  geom_vline(linetype="solid", xintercept=qnorm(0.05,0,1/sqrt(15),lower.tail=F)) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste(alpha,"=0.05=")))

p2 <- getnd(0, 1/sqrt(15)) %>% 
  ggplot() +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M",
                     limits=c(-3, 3), breaks=-2:2, 
                     expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 1.7), expand=expansion(0)) +
  # data
  geom_area(aes(x=x, y=y),
            color=c[2], fill=c[2]) +
  geom_area(aes(x=ifelse(x<=qnorm(0.05,0,1/sqrt(15),lower.tail=T), x, NA), y=y),
            color=c[4], fill=c[4]) +
  #lines
  geom_segment(linetype="dashed", x=0, xend=0, y=0, yend=1.7) +
  geom_vline(linetype="solid", xintercept=qnorm(0.05,0,1/sqrt(15),lower.tail=T)) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste(alpha,"=0.05=")))

p3 <- getnd(0, 1/sqrt(15)) %>% 
  ggplot() +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M",
                     limits=c(-3, 3), breaks=-2:2, 
                     expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 1.7), expand=expansion(0)) +
  # data
  geom_area(aes(x=x, y=y),
            color=c[2], fill=c[2]) +
  geom_area(aes(x=ifelse(x>=qnorm(0.025,0,1/sqrt(15),lower.tail=F), x, NA), y=y),
            color=c[4], fill=c[4]) +
  geom_area(aes(x=ifelse(x<=qnorm(0.025,0,1/sqrt(15),lower.tail=T), x, NA), y=y),
            color=c[4], fill=c[4]) +
  #lines
  geom_segment(linetype="dashed", x=0, xend=0, y=0, yend=1.7) +
  geom_vline(linetype="solid", xintercept=qnorm(0.025,0,1/sqrt(15),lower.tail=F)) +
  geom_vline(linetype="solid", xintercept=qnorm(0.025,0,1/sqrt(15),lower.tail=T)) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste(alpha,"=0.05=")))

p4 <- getnd(0, 1/sqrt(15)) %>% 
  mutate(y0 = y,
         y1 = y + 1*1.7,
         y2 = y + 2*1.7,
         x0 = x + 0.99,
         x1 = x + 0.66,
         x2 = x + 0.33) %>% 
  ggplot() +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M",
                     limits=c(-3, 3), breaks=-2:2, 
                     expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 2*1.7+1.7), expand=expansion(0)) +
  # data
  geom_ribbon(aes(x=x2, ymax=y2, ymin=2*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x2 >= qnorm(0.05,0,1/sqrt(15),lower.tail=F), x2, NA), ymax=y2, ymin=2*1.7),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               x=0.33, xend=0.33, y=0, yend=2*1.7+1.7) +
  geom_ribbon(aes(x=x1, ymax=y1, ymin=1*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x1 >= qnorm(0.05,0,1/sqrt(15),lower.tail=F), x1, NA), ymax=y1, ymin=1*1.7),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               x=0.66, xend=0.66, y=0, yend=1*1.7+1.7) +
  geom_ribbon(aes(x=x0, ymax=y0, ymin=0*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x0 >= qnorm(0.05,0,1/sqrt(15),lower.tail=F), x0, NA), ymax=y0, ymin=0*1.7),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               x=0.99, xend=0.99, y=0, yend=0*1.7+1.7) +
  # lines
  geom_vline(xintercept=qnorm(0.05,0,1/sqrt(15),lower.tail=F)) +
  geom_hline(yintercept=1:2 * 1.7) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=2*1.7+0.9, ymin=2*1.7+0.7) +
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=1*1.7+0.9, ymin=1*1.7+0.7) +
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=2*1.7+0.8, 
           label=expression(paste(1-beta,"="))) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=1*1.7+0.8, 
           label=expression(paste(1-beta,"="))) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste(1-beta,"=")))

p5 <- getnd(0, 1/sqrt(15)) %>% 
  mutate(y0 = y,
         y1 = y + 1*1.7,
         y2 = y + 2*1.7,
         x0 = x - 0.99,
         x1 = x - 0.66,
         x2 = x - 0.33) %>% 
  ggplot() +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M",
                     limits=c(-3, 3), breaks=-2:2, 
                     expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 2*1.7+1.7), expand=expansion(0)) +
  # data
  geom_ribbon(aes(x=x2, ymax=y2, ymin=2*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x2 <= qnorm(0.05,0,1/sqrt(15),lower.tail=T), x2, NA), ymax=y2, ymin=2*1.7),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               x=-0.33, xend=-0.33, y=0, yend=2*1.7+1.7) +
  geom_ribbon(aes(x=x1, ymax=y1, ymin=1*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x1 <= qnorm(0.05,0,1/sqrt(15),lower.tail=T), x1, NA), ymax=y1, ymin=1*1.7),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               x=-0.66, xend=-0.66, y=0, yend=1*1.7+1.7) +
  geom_ribbon(aes(x=x0, ymax=y0, ymin=0*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x0 <= qnorm(0.05,0,1/sqrt(15),lower.tail=T), x0, NA), ymax=y0, ymin=0*1.7),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               x=-0.99, xend=-0.99, y=0, yend=0*1.7+1.7) +
  # lines
  geom_vline(xintercept=qnorm(0.05,0,1/sqrt(15),lower.tail=T)) +
  geom_hline(yintercept=1:2 * 1.7) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=2*1.7+0.9, ymin=2*1.7+0.7) +
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=1*1.7+0.9, ymin=1*1.7+0.7) +
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=2*1.7+0.8, 
           label=expression(paste(1-beta,"="))) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=1*1.7+0.8, 
           label=expression(paste(1-beta,"="))) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste(1-beta,"=")))

p6 <- getnd(0, 1/sqrt(15)) %>% 
  mutate(y0 = y,
         y1 = y + 1*1.7,
         y2 = y + 2*1.7,
         x0 = x - 0.99,
         x1 = x + 0.66,
         x2 = x + 0.33) %>% 
  ggplot() +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M",
                     limits=c(-3, 3), breaks=-2:2, 
                     expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 2*1.7+1.7), expand=expansion(0)) +
  # data
  geom_ribbon(aes(x=x2, ymax=y2, ymin=2*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x2 >= qnorm(0.025,0,1/sqrt(15),lower.tail=F), x2, NA), ymax=y2, ymin=2*1.7),
              color=c[4], fill=c[4]) +
  geom_ribbon(aes(x=ifelse(x2 <= qnorm(0.025,0,1/sqrt(15),lower.tail=T), x2, NA), ymax=y2, ymin=2*1.7),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               x=0.33, xend=0.33, y=0, yend=2*1.7+1.7) +
  geom_ribbon(aes(x=x1, ymax=y1, ymin=1*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x1 >= qnorm(0.025,0,1/sqrt(15),lower.tail=F), x1, 100), ymax=y1, ymin=1*1.7),
              color=c[4], fill=c[4]) +
  geom_ribbon(aes(x=ifelse(x1 <= qnorm(0.025,0,1/sqrt(15),lower.tail=T), x1, 100), ymax=y1, ymin=1*1.7),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               x=0.66, xend=0.66, y=0, yend=1*1.7+1.7) +
  geom_ribbon(aes(x=x0, ymax=y0, ymin=0*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(x0 >= qnorm(0.025,0,1/sqrt(15),lower.tail=F), x0, 100), ymax=y0, ymin=0*1.7),
              color=c[4], fill=c[4]) +
  geom_ribbon(aes(x=ifelse(x0 <= qnorm(0.025,0,1/sqrt(15),lower.tail=T), x0, 100), ymax=y0, ymin=0*1.7),
              color=c[4], fill=c[4]) +
  geom_segment(linetype="dashed",
               x=-0.99, xend=-0.99, y=0, yend=0*1.7+1.7) +
  # lines
  geom_vline(xintercept=qnorm(c(0.025, 0.975),0,1/sqrt(15))) +
  geom_hline(yintercept=1:2 * 1.7) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=2*1.7+0.9, ymin=2*1.7+0.7) +
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=1*1.7+0.9, ymin=1*1.7+0.7) +
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=2*1.7+0.8, 
           label=expression(paste(1-beta,"="))) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=1*1.7+0.8, 
           label=expression(paste(1-beta,"="))) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste(1-beta,"=")))

p7 <- data.frame(mu0 = seq(-3, 3, 0.001)) %>% 
  mutate(y = pnorm(qnorm(0.95,0,1/sqrt(15)), mu0, 1/sqrt(15), lower.tail=F)) %>%
  ggplot(aes(mu0, y)) +
  geom_line(size=1, color=c[4]) +
  # axes
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3, 3), breaks=-2:2, 
                     expand=expansion(0)) +
  scale_y_continuous(name="Rejection Rate",
                     limits=c(0,1), breaks=seq(0,1,0.2)) +
  # theme
  theme_blog() + theme(
    aspect.ratio=0.5,
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    #axis.title.y = element_blank(),
    #axis.text.y = element_blank()
  ) +
  # annotations
  geom_vline(xintercept=c(0,0.33,0.66,0.99), linetype="dashed") +
  geom_hline(yintercept=0.05, linetype="dotted") +
  #geom_point(aes(x=0, y=0.05),
  #           shape=22, color=c[4], fill=c[4], size=2) +
  geom_point(aes(x=0.33, y=pnorm(qnorm(0.95,0,1/sqrt(15)), 0.33, 1/sqrt(15), lower.tail=F)),
             shape=22, color=c[4], fill=c[4], size=2) +
  geom_point(aes(x=0.66, y=pnorm(qnorm(0.95,0,1/sqrt(15)), 0.66, 1/sqrt(15), lower.tail=F)),
             shape=22, color=c[4], fill=c[4], size=2) +
  geom_point(aes(x=0.99, y=pnorm(qnorm(0.95,0,1/sqrt(15)), 0.99, 1/sqrt(15), lower.tail=F)),
             shape=22, color=c[4], fill=c[4], size=2) +
  annotate("text", label=expression(paste(alpha==0.05)),
           hjust=0, vjust=0, x=-2.9, y=0.06, size=3)

p8 <- data.frame(mu0 = seq(-3, 3, 0.001)) %>% 
  mutate(y = pnorm(qnorm(0.05,0,1/sqrt(15)), mu0, 1/sqrt(15), lower.tail=T)) %>%
  ggplot(aes(mu0, y)) +
  geom_line(size=1, color=c[4]) +
  # axes
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3, 3), breaks=-2:2, 
                     expand=expansion(0)) +
  scale_y_continuous(name="Rejection Rate",
                     limits=c(0,1), breaks=seq(0,1,0.2)) +
  # theme
  theme_blog() + theme(
    aspect.ratio=0.5,
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  # annotations
  geom_vline(xintercept=c(0,-0.33,-0.66,-0.99), linetype="dashed") +
  geom_hline(yintercept=0.05, linetype="dotted") +
  #geom_point(aes(x=0, y=0.05),
  #           shape=22, color=c[4], fill=c[4], size=2) +
  geom_point(aes(x=-0.33, y=pnorm(qnorm(0.05,0,1/sqrt(15)), -0.33, 1/sqrt(15), lower.tail=T)),
             shape=22, color=c[4], fill=c[4], size=2) +
  geom_point(aes(x=-0.66, y=pnorm(qnorm(0.05,0,1/sqrt(15)), -0.66, 1/sqrt(15), lower.tail=T)),
             shape=22, color=c[4], fill=c[4], size=2) +
  geom_point(aes(x=-0.99, y=pnorm(qnorm(0.05,0,1/sqrt(15)), -0.99, 1/sqrt(15), lower.tail=T)),
             shape=22, color=c[4], fill=c[4], size=2)  +
  annotate("text", label=expression(paste(alpha==0.05)),
           hjust=0, vjust=0, x=-2.9, y=0.06, size=3)

p9 <- data.frame(mu0 = seq(-3, 3, 0.001)) %>% 
  mutate(y = pnorm(qnorm(0.975,0,1/sqrt(15)), mu0, 1/sqrt(15), lower.tail=F) +
           pnorm(qnorm(0.025,0,1/sqrt(15)), mu0, 1/sqrt(15), lower.tail=T)) %>%
  ggplot(aes(mu0, y)) +
  geom_line(size=1, color=c[4]) +
  # axes
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3, 3), breaks=-2:2, 
                     expand=expansion(0)) +
  scale_y_continuous(name="Rejection Rate",
                     limits=c(0,1), breaks=seq(0,1,0.2)) +
  # theme
  theme_blog() + theme(
    aspect.ratio=0.5,
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  # annotations
  geom_vline(xintercept=c(0,0.33,0.66,-0.99), linetype="dashed") +
  geom_hline(yintercept=0.05, linetype="dotted") +
  #geom_point(aes(x=0, y=0.05),
  #           shape=22, color=c[4], fill=c[4], size=2) +
  geom_point(aes(x=0.33, y=
                   pnorm(qnorm(0.975,0,1/sqrt(15)), 0.33, 1/sqrt(15), lower.tail=F)+
                   pnorm(qnorm(0.025,0,1/sqrt(15)), 0.33, 1/sqrt(15), lower.tail=T)),
             shape=22, color=c[4], fill=c[4], size=2) +
  geom_point(aes(x=0.66, y=
                   pnorm(qnorm(0.975,0,1/sqrt(15)), 0.66, 1/sqrt(15), lower.tail=F)+
                   pnorm(qnorm(0.025,0,1/sqrt(15)), 0.66, 1/sqrt(15), lower.tail=T)),
             shape=22, color=c[4], fill=c[4], size=2) +
  geom_point(aes(x=-0.99, y=
                   pnorm(qnorm(0.975,0,1/sqrt(15)), -0.99, 1/sqrt(15), lower.tail=F)+
                   pnorm(qnorm(0.025,0,1/sqrt(15)), -0.99, 1/sqrt(15), lower.tail=T)),
             shape=22, color=c[4], fill=c[4], size=2) +
  annotate("text", label=expression(paste(alpha==0.05)),
           hjust=0, vjust=0, x=-2.9, y=0.06, size=3)

plot17 <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + plot_layout(design=layout)

ggsave(
  plot     = plot17,
  filename = "17_rejection_rate.svg",
  path     = plotdir,
  scale    = 0.6,
  height   = 8.5,
  width    = 10
)

# 18. Alpha Beta Inference --------------------------------------------------------------

layout <- "
ABC
DEF
"

p1 <- getnd(0,1/sqrt(15)) %>%
  mutate(xa = x,
         ya = y + 2*1.7,
         xb = x + 2*qnorm(0.95)/sqrt(15),
         yb = y + 1*1.7) %>% 
  ggplot() +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M",
                     limits=c(-3, 3), breaks=-2:2,
                     expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 2*1.7+1.7), expand=expansion(0)) +
  # data H0
  geom_ribbon(aes(x=xa, ymax=ya, ymin=2*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(xa >= qnorm(0.95)/sqrt(15), xa, NA), ymax=ya, ymin=2*1.7),
              color=c[4], fill=c[4]) +
  geom_hline(yintercept=2*1.7) +
  geom_segment(linetype="dashed",
               x=0, xend=0, y=2*1.7, yend=3*1.7) +
  # data H1
  geom_ribbon(aes(x=xb, ymax=yb, ymin=1*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(xb >= qnorm(0.95)/sqrt(15), xb, NA), ymax=yb, ymin=1*1.7),
              color=c[4], fill=c[4]) +
  geom_hline(yintercept=1*1.7) +
  geom_segment(linetype="dashed",
               x=2*qnorm(0.95)/sqrt(15), xend=2*qnorm(0.95)/sqrt(15), y=1.7, yend=2*1.7) +
  # line
  geom_vline(xintercept=qnorm(0.95)/sqrt(15)) +
  # annotations rates
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=2*1.7+0.9, ymin=2*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=2*1.7+0.8, 
           label=expression(paste(alpha,"=0.05="))) +
  annotate("rect", color=c[2], fill=c[2],
           xmax=3, xmin=2.8, ymax=1*1.7+0.9, ymin=1*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=1*1.7+0.8, 
           label=expression(paste(beta(mu[1]),"=",alpha,"="))) +
  # annotations text
  annotate("text", size=3, label=expression(paste(mu[0])),
           hjust=0.5, vjust=1, x=0, y=2*1.7-0.1) +
  annotate("text", size=3, label=expression(paste(mu[1])),
           hjust=0.5, vjust=1, x=2*qnorm(0.95)/sqrt(15), y=1*1.7-0.1) +
  annotate("text", size=2.5, label=expression(paste("icw ",mu>=mu[1])),
           hjust=1, vjust=0.5, x=qnorm(0.95)/sqrt(15)-0.2, y=1.7*0.5) +
  annotate("text", size=2.5, label=expression(paste("icw ",mu<=mu[0])),
           hjust=0, vjust=0.5, x=qnorm(0.95)/sqrt(15)+0.2, y=1.7*0.5) 

p2 <- getnd(0,1/sqrt(15)) %>%
  mutate(xa = x,
         ya = y + 2*1.7,
         xb = x - 2*qnorm(0.95)/sqrt(15),
         yb = y + 1*1.7) %>% 
  ggplot() +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M",
                     limits=c(-3, 3), breaks=-2:2,
                     expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 2*1.7+1.7), expand=expansion(0)) +
  # data H0
  geom_ribbon(aes(x=xa, ymax=ya, ymin=2*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(xa <= qnorm(0.05)/sqrt(15), xa, NA), ymax=ya, ymin=2*1.7),
              color=c[4], fill=c[4]) +
  geom_hline(yintercept=2*1.7) +
  geom_segment(linetype="dashed",
               x=0, xend=0, y=2*1.7, yend=3*1.7) +
  # data H1
  geom_ribbon(aes(x=xb, ymax=yb, ymin=1*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(xb <= qnorm(0.05)/sqrt(15), xb, NA), ymax=yb, ymin=1*1.7),
              color=c[4], fill=c[4]) +
  geom_hline(yintercept=1*1.7) +
  geom_segment(linetype="dashed",
               x=2*qnorm(0.05)/sqrt(15), xend=2*qnorm(0.05)/sqrt(15), y=1.7, yend=2*1.7) +
  # line
  geom_vline(xintercept=qnorm(0.05)/sqrt(15)) +
  # annotations rates
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=2*1.7+0.9, ymin=2*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=2*1.7+0.8, 
           label=expression(paste(alpha,"=0.05="))) +
  annotate("rect", color=c[2], fill=c[2],
           xmax=3, xmin=2.8, ymax=1*1.7+0.9, ymin=1*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=1*1.7+0.8, 
           label=expression(paste(beta(mu[1]),"=",alpha,"="))) +
  # annotations text
  annotate("text", size=3, label=expression(paste(mu[0])),
           hjust=0.5, vjust=1, x=0, y=2*1.7-0.1) +
  annotate("text", size=3, label=expression(paste(mu[1])),
           hjust=0.5, vjust=1, x=2*qnorm(0.05)/sqrt(15), y=1*1.7-0.1) +
  annotate("text", size=2.5, label=expression(paste("icw ",mu>=mu[0])),
           hjust=1, vjust=0.5, x=qnorm(0.05)/sqrt(15)-0.2, y=1.7*0.5) +
  annotate("text", size=2.5, label=expression(paste("icw ",mu<=mu[1])),
           hjust=0, vjust=0.5, x=qnorm(0.05)/sqrt(15)+0.2, y=1.7*0.5)

p3 <- getnd(0,1/sqrt(15)) %>%
  mutate(xa = x,
         ya = y + 2*1.7,
         xb1 = x + 2*qnorm(0.975)/sqrt(15),
         xb2 = x - 2*qnorm(0.975)/sqrt(15),
         yb = y + 1*1.7) %>% 
  ggplot() +
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M",
                     limits=c(-3, 3), breaks=-2:2,
                     expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 2*1.7+1.7), expand=expansion(0)) +
  # data H0
  geom_ribbon(aes(x=xa, ymax=ya, ymin=2*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(xa >= qnorm(0.975)/sqrt(15), xa, NA), ymax=ya, ymin=2*1.7),
              color=c[4], fill=c[4]) +
  geom_ribbon(aes(x=ifelse(xa <= qnorm(0.025)/sqrt(15), xa, NA), ymax=ya, ymin=2*1.7),
              color=c[4], fill=c[4]) +
  geom_hline(yintercept=2*1.7) +
  geom_segment(linetype="dashed",
               x=0, xend=0, y=2*1.7, yend=3*1.7) +
  # data H1
  geom_ribbon(aes(x=xb1, ymax=yb, ymin=1*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=xb2, ymax=yb, ymin=1*1.7),
              color=c[2], fill=c[2]) +
  geom_ribbon(aes(x=ifelse(xb1 >= qnorm(0.975)/sqrt(15), xb1, NA), ymax=yb, ymin=1*1.7),
              color=c[4], fill=c[4]) +
  geom_ribbon(aes(x=ifelse(xb2 <= qnorm(0.025)/sqrt(15), xb2, NA), ymax=yb, ymin=1*1.7),
              color=c[4], fill=c[4]) +
  geom_hline(yintercept=1*1.7) +
  geom_segment(linetype="dashed",
               x=2*qnorm(0.975)/sqrt(15), xend=2*qnorm(0.975)/sqrt(15), y=1.7, yend=2*1.7) +
  geom_segment(linetype="dashed",
               x=2*qnorm(0.025)/sqrt(15), xend=2*qnorm(0.025)/sqrt(15), y=1.7, yend=2*1.7) +
  # line
  geom_vline(xintercept=qnorm(c(0.025,0.975))/sqrt(15)) +
  # annotations rates
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=2*1.7+0.9, ymin=2*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=2*1.7+0.8, 
           label=expression(paste(alpha,"=0.05="))) +
  annotate("rect", color=c[2], fill=c[2],
           xmax=3, xmin=2.8, ymax=1*1.7+0.9, ymin=1*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=1*1.7+0.8, 
           label=expression(paste(beta(mu[1]),"=",alpha,"="))) +
  # annotations text mu0 mu1
  annotate("text", size=3, label=expression(paste(mu[0])),
           hjust=0.5, vjust=1, x=0, y=2*1.7-0.1) +
  annotate("text", size=3, label=expression(paste(mu[1])),
           hjust=0.5, vjust=1, x=2*qnorm(0.975)/sqrt(15), y=1*1.7-0.1) +
  annotate("text", size=3, label=expression(paste(-mu[1])),
           hjust=0.5, vjust=1, x=2*qnorm(0.025)/sqrt(15), y=1*1.7-0.1) +
  # annotation text zones
  annotate("text", size=2.5, label=expression(paste("icw ",mu==mu[0])),
           hjust=0, vjust=0.5, x=qnorm(0.975)/sqrt(15)+0.2, y=1.7*0.5) +
  annotate("text", size=2.5, label=expression(paste("icw ",mu==mu[0])),
           hjust=1, vjust=0.5, x=qnorm(0.025)/sqrt(15)-0.2, y=1.7*0.5) +
  annotate("text", size=2, label="icw",
           hjust=0.5, vjust=0.5, x=0, y=1.7*0.5+0.45) +
  annotate("text", size=2, label=expression(paste(mu>=mu[1])),
           hjust=0.5, vjust=0.5, x=0, y=1.7*0.5+0.15) +
  annotate("text", size=2, label="or",
           hjust=0.5, vjust=0.5, x=0, y=1.7*0.5-0.15) +
  annotate("text", size=2, label=expression(paste(mu<=-mu[1])),
           hjust=0.5, vjust=0.5, x=0, y=1.7*0.5-0.45)

p4 <- data.frame(mu0 = seq(-3, 3, 0.001)) %>% 
  mutate(y = pnorm(qnorm(0.95,0,1/sqrt(15)), mu0, 1/sqrt(15), lower.tail=F)) %>%
  ggplot(aes(mu0, y)) +
  geom_line(size=1, color=c[4]) +
  # axes
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3, 3), breaks=-2:2,
                     expand=expansion(0)) +
  scale_y_continuous(name="Rejection Rate",
                     limits=c(0,1), breaks=seq(0,1,0.2)) +
  # theme
  theme_blog() + theme(
    aspect.ratio=0.5,
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    #axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
  ) +
  # annotations
  geom_vline(linetype="dashed", 
             xintercept=c(
               0,
               2*qnorm(0.95, 0, 1/sqrt(15))
             )) +
  geom_hline(linetype="dotted",
             color="black",
             yintercept=c(
               0.05,
               0.95)) +
  annotate("text", label=expression(paste(alpha==0.05)),
           hjust=0, vjust=0, x=-2.9, y=0.06, size=3) +
  annotate("text", label=expression(paste(1-beta==0.95)),
           hjust=0, vjust=1, x=-2.9, y=0.94, size=3)

p5 <- data.frame(mu0 = seq(-3, 3, 0.001)) %>% 
  mutate(y = pnorm(qnorm(0.05,0,1/sqrt(15)), mu0, 1/sqrt(15), lower.tail=T)) %>%
  ggplot(aes(mu0, y)) +
  geom_line(size=1, color=c[4]) +
  # axes
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3, 3), breaks=-2:2, 
                     expand=expansion(0)) +
  scale_y_continuous(name="Rejection Rate",
                     limits=c(0,1), breaks=seq(0,1,0.2)) +
  # theme
  theme_blog() + theme(
    aspect.ratio=0.5,
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  # annotations
  geom_vline(linetype="dashed", 
             xintercept=c(
               0,
               2*qnorm(0.05, 0, 1/sqrt(15))
             )) +
  geom_hline(linetype="dotted",
             color="black",
             yintercept=c(
               0.05,
               0.95)) +
  annotate("text", label=expression(paste(alpha==0.05)),
           hjust=0, vjust=0, x=-2.9, y=0.06, size=3) +
  annotate("text", label=expression(paste(1-beta==0.95)),
           hjust=0, vjust=1, x=-2.9, y=0.94, size=3)

p6 <- data.frame(mu0 = seq(-3, 3, 0.001)) %>% 
  mutate(y = pnorm(qnorm(0.975,0,1/sqrt(15)), mu0, 1/sqrt(15), lower.tail=F) +
           pnorm(qnorm(0.025,0,1/sqrt(15)), mu0, 1/sqrt(15), lower.tail=T)) %>%
  ggplot(aes(mu0, y)) +
  geom_line(size=1, color=c[4]) +
  # axes
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3, 3), breaks=-2:2, 
                     expand=expansion(0)) +
  scale_y_continuous(name="Rejection Rate",
                     limits=c(0,1), breaks=seq(0,1,0.2)) +
  # theme
  theme_blog() + theme(
    aspect.ratio=0.5,
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  # annotations
  geom_vline(linetype="dashed", 
             xintercept=c(
               0,
               qnorm(0.975, 0, 1/sqrt(15)) + qnorm(0.95, 0, 1/sqrt(15)),
               qnorm(0.025, 0, 1/sqrt(15)) + qnorm(0.05, 0, 1/sqrt(15))
             )) +
  geom_hline(linetype="dotted",
             color="black",
             yintercept=c(
               0.05,
               0.95)) +
  annotate("text", label=expression(paste(alpha==0.05)),
           hjust=0, vjust=0, x=-2.9, y=0.06, size=3) +
  annotate("text", label=expression(paste(1-beta==0.95)),
           hjust=0, vjust=1, x=-2.9, y=0.94, size=3)


plot18 <- p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(design=layout)

ggsave(
  plot     = plot18,
  filename = "18_alpha_beta_inference.svg",
  path     = plotdir,
  scale    = 0.6,
  height   = 6.7,
  width    = 10
)

# 19. Rejection Rate by N ---------------------------------------------------------------


layout <- "
ABC
DEF
GHI
"

p1 <- getnd(0, 1/sqrt(5)) %>% 
  ggplot() + 
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M",
                     limits=c(-3, 3), breaks=-2:2,
                     expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 2.83), expand=expansion(0)) +
  # data
  geom_area(aes(x,y),
            color=c[1], fill=c[1]) +
  geom_area(aes(ifelse(x>=qnorm(0.975)/sqrt(5), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_area(aes(ifelse(x<=qnorm(0.025)/sqrt(5), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_vline(linetype="solid", xintercept=qnorm(0.975)/sqrt(5)) +
  geom_vline(linetype="solid", xintercept=qnorm(0.025)/sqrt(5)) +
  geom_vline(linetype="dashed", xintercept=0) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste(alpha,"=0.05="))) +
  ggtitle(expression(paste(N==5))) + theme(plot.title=element_text(hjust=0.5))

p2 <- getnd(0, 1/sqrt(15)) %>% 
  ggplot() + 
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M",
                     limits=c(-3, 3), breaks=-2:2,
                     expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 2.83), expand=expansion(0)) +
  # data
  geom_area(aes(x,y),
            color=c[2], fill=c[2]) +
  geom_area(aes(ifelse(x>=qnorm(0.975)/sqrt(15), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_area(aes(ifelse(x<=qnorm(0.025)/sqrt(15), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_vline(linetype="solid", xintercept=qnorm(0.975)/sqrt(15)) +
  geom_vline(linetype="solid", xintercept=qnorm(0.025)/sqrt(15)) +
  geom_vline(linetype="dashed", xintercept=0) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste(alpha,"=0.05="))) +
  ggtitle(expression(paste(N==15))) + theme(plot.title=element_text(hjust=0.5))

p3 <- getnd(0, 1/sqrt(50)) %>% 
  ggplot() + 
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M",
                     limits=c(-3, 3), breaks=-2:2,
                     expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 2.83), expand=expansion(0)) +
  # data
  geom_area(aes(x,y),
            color=c[3], fill=c[3]) +
  geom_area(aes(ifelse(x>=qnorm(0.975)/sqrt(50), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_area(aes(ifelse(x<=qnorm(0.025)/sqrt(50), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_vline(linetype="solid", xintercept=qnorm(0.975)/sqrt(50)) +
  geom_vline(linetype="solid", xintercept=qnorm(0.025)/sqrt(50)) +
  geom_vline(linetype="dashed", xintercept=0) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste(alpha,"=0.05="))) +
  ggtitle(expression(paste(N==50))) + theme(plot.title=element_text(hjust=0.5))

p4 <- getnd(0.65, 1/sqrt(5)) %>% 
  ggplot() + 
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M",
                     limits=c(-3, 3), breaks=-2:2,
                     expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 2.83), expand=expansion(0)) +
  # data
  geom_area(aes(x,y),
            color=c[1], fill=c[1]) +
  geom_area(aes(ifelse(x>=qnorm(0.975)/sqrt(5), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_area(aes(ifelse(x<=qnorm(0.025)/sqrt(5), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_vline(linetype="solid", xintercept=qnorm(0.975)/sqrt(5)) +
  geom_vline(linetype="solid", xintercept=qnorm(0.025)/sqrt(5)) +
  geom_vline(linetype="dashed", xintercept=0.65) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste(1-beta,"=")))

p5 <- getnd(0.65, 1/sqrt(15)) %>% 
  ggplot() + 
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M",
                     limits=c(-3, 3), breaks=-2:2,
                     expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 2.83), expand=expansion(0)) +
  # data
  geom_area(aes(x,y),
            color=c[2], fill=c[2]) +
  geom_area(aes(ifelse(x>=qnorm(0.975)/sqrt(15), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_area(aes(ifelse(x<=qnorm(0.025)/sqrt(15), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_vline(linetype="solid", xintercept=qnorm(0.975)/sqrt(15)) +
  geom_vline(linetype="solid", xintercept=qnorm(0.025)/sqrt(15)) +
  geom_vline(linetype="dashed", xintercept=0.65) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste(1-beta,"=")))

p6 <- getnd(0.65, 1/sqrt(50)) %>% 
  ggplot() + 
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M",
                     limits=c(-3, 3), breaks=-2:2,
                     expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 2.83), expand=expansion(0)) +
  # data
  geom_area(aes(x,y),
            color=c[3], fill=c[3]) +
  geom_area(aes(ifelse(x>=qnorm(0.975)/sqrt(50), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_area(aes(ifelse(x<=qnorm(0.025)/sqrt(50), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_vline(linetype="solid", xintercept=qnorm(0.975)/sqrt(50)) +
  geom_vline(linetype="solid", xintercept=qnorm(0.025)/sqrt(50)) +
  geom_vline(linetype="dashed", xintercept=0.65) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste(1-beta,"=")))

p7 <- data.frame(mu0 = seq(-3, 3, 0.001)) %>% 
  mutate(y = pnorm(qnorm(0.975,0,1/sqrt(5)), mu0, 1/sqrt(5), lower.tail=F) +
           pnorm(qnorm(0.025,0,1/sqrt(5)), mu0, 1/sqrt(5), lower.tail=T)) %>%
  ggplot(aes(mu0, y)) +
  geom_line(size=1, color=c[4]) +
  # axes
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3, 3), breaks=-2:2,
                     expand=expansion(0)) +
  scale_y_continuous(name="Rejection Rate",
                     limits=c(0,1), breaks=seq(0,1,0.2)) +
  # theme
  theme_blog() + theme(
    aspect.ratio=0.5,
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    #axis.title.y = element_blank(),
    #axis.text.y = element_blank()
  ) +
  # annotations
  geom_vline(xintercept=0, linetype="dashed") +
  geom_vline(xintercept=0.65, linetype="dashed") +
  geom_hline(yintercept=0.05, linetype="dotted") +
  #geom_point(aes(x=0, y=0.05),
  #           shape=22, color=c[4], fill=c[4], size=2) +
  geom_point(aes(x=0.65, y=
                   pnorm(qnorm(0.975,0,1/sqrt(5)), 0.65, 1/sqrt(5), lower.tail=F)+
                   pnorm(qnorm(0.025,0,1/sqrt(5)), 0.65, 1/sqrt(5), lower.tail=T)),
             shape=22, color=c[4], fill=c[4], size=2) +
  annotate("text", label=expression(paste(alpha==0.05)),
           hjust=0, vjust=0, x=-2.9, y=0.06, size=3)

p8 <- data.frame(mu0 = seq(-3, 3, 0.001)) %>% 
  mutate(y = pnorm(qnorm(0.975,0,1/sqrt(15)), mu0, 1/sqrt(15), lower.tail=F) +
           pnorm(qnorm(0.025,0,1/sqrt(15)), mu0, 1/sqrt(15), lower.tail=T)) %>%
  ggplot(aes(mu0, y)) +
  geom_line(size=1, color=c[4]) +
  # axes
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3, 3), breaks=-2:2, 
                     expand=expansion(0)) +
  scale_y_continuous(name="Rejection Rate",
                     limits=c(0,1), breaks=seq(0,1,0.2)) +
  # theme
  theme_blog() + theme(
    aspect.ratio=0.5,
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  # annotations
  geom_vline(xintercept=0, linetype="dashed") +
  geom_vline(xintercept=0.65, linetype="dashed") +
  geom_hline(yintercept=0.05, linetype="dotted") +
  #geom_point(aes(x=0, y=0.05),
  #           shape=22, color=c[4], fill=c[4], size=2) +
  geom_point(aes(x=0.65, y=
                   pnorm(qnorm(0.975,0,1/sqrt(15)), 0.65, 1/sqrt(15), lower.tail=F)+
                   pnorm(qnorm(0.025,0,1/sqrt(15)), 0.65, 1/sqrt(15), lower.tail=T)),
             shape=22, color=c[4], fill=c[4], size=2) +
  annotate("text", label=expression(paste(alpha==0.05)),
           hjust=0, vjust=0, x=-2.9, y=0.06, size=3)

p9 <- data.frame(mu0 = seq(-3, 3, 0.001)) %>% 
  mutate(y = pnorm(qnorm(0.975,0,1/sqrt(50)), mu0, 1/sqrt(50), lower.tail=F) +
           pnorm(qnorm(0.025,0,1/sqrt(50)), mu0, 1/sqrt(50), lower.tail=T)) %>%
  ggplot(aes(mu0, y)) +
  geom_line(size=1, color=c[4]) +
  # axes
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3, 3), breaks=-2:2, 
                     expand=expansion(0)) +
  scale_y_continuous(name="Rejection Rate",
                     limits=c(0,1), breaks=seq(0,1,0.2)) +
  # theme
  theme_blog() + theme(
    aspect.ratio=0.5,
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  # annotations
  geom_vline(xintercept=0, linetype="dashed") +
  geom_vline(xintercept=0.65, linetype="dashed") +
  geom_hline(yintercept=0.05, linetype="dotted") +
  #geom_point(aes(x=0, y=0.05),
  #           shape=22, color=c[4], fill=c[4], size=2) +
  geom_point(aes(x=0.65, y=
                   pnorm(qnorm(0.975,0,1/sqrt(50)), 0.65, 1/sqrt(50), lower.tail=F)+
                   pnorm(qnorm(0.025,0,1/sqrt(50)), 0.65, 1/sqrt(50), lower.tail=T)),
             shape=22, color=c[4], fill=c[4], size=2) +
  annotate("text", label=expression(paste(alpha==0.05)),
           hjust=0, vjust=0, x=-2.9, y=0.06, size=3)


plot19 <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + plot_layout(design=layout)

ggsave(
  plot     = plot19,
  filename = "19_rejection_rate_by_n.svg",
  path     = plotdir,
  scale    = 0.6,
  height   = 8.2,
  width    = 10
)

# 20. Rejection Rate By Alpha -----------------------------------------------------------

layout <- "
ABC
DEF
GHI
"

p3 <- getnd(0, 1/sqrt(15)) %>% 
  ggplot() + 
  # design 
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M",
                     limits=c(-3, 3), breaks=-2:2, 
                     expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 1.7), expand=expansion(0)) +
  # data
  geom_area(aes(x,y),
            color=c[2], fill=c[2]) +
  geom_area(aes(ifelse(x>=qnorm(0.900)/sqrt(15), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_area(aes(ifelse(x<=qnorm(0.100)/sqrt(15), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_vline(linetype="solid", xintercept=qnorm(0.900)/sqrt(15)) +
  geom_vline(linetype="solid", xintercept=qnorm(0.100)/sqrt(15)) +
  geom_vline(linetype="dashed", xintercept=0) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste(alpha,"=0.2="))) +
  ggtitle(expression(paste(alpha==0.20))) + theme(plot.title=element_text(hjust=0.5))

p2 <- getnd(0, 1/sqrt(15)) %>% 
  ggplot() + 
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M",
                     limits=c(-3, 3), breaks=-2:2, 
                     expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 1.7), expand=expansion(0)) +
  # data
  geom_area(aes(x,y),
            color=c[2], fill=c[2]) +
  geom_area(aes(ifelse(x>=qnorm(0.975)/sqrt(15), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_area(aes(ifelse(x<=qnorm(0.025)/sqrt(15), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_vline(linetype="solid", xintercept=qnorm(0.975)/sqrt(15)) +
  geom_vline(linetype="solid", xintercept=qnorm(0.025)/sqrt(15)) +
  geom_vline(linetype="dashed", xintercept=0) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste(alpha,"=0.05="))) +
  ggtitle(expression(paste(alpha==0.05))) + theme(plot.title=element_text(hjust=0.5))

p1 <- getnd(0, 1/sqrt(15)) %>% 
  ggplot() + 
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M",
                     limits=c(-3, 3), breaks=-2:2, 
                     expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 1.7), expand=expansion(0)) +
  # data
  geom_area(aes(x,y),
            color=c[2], fill=c[2]) +
  geom_area(aes(ifelse(x>=qnorm(0.995)/sqrt(15), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_area(aes(ifelse(x<=qnorm(0.005)/sqrt(15), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_vline(linetype="solid", xintercept=qnorm(0.995)/sqrt(15)) +
  geom_vline(linetype="solid", xintercept=qnorm(0.005)/sqrt(15)) +
  geom_vline(linetype="dashed", xintercept=0) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste(alpha,"=0.01="))) +
  ggtitle(expression(paste(alpha==0.01))) + theme(plot.title=element_text(hjust=0.5))

p6 <- getnd(0.65, 1/sqrt(15)) %>% 
  ggplot() + 
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M",
                     limits=c(-3, 3), breaks=-2:2,
                     expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 1.7), expand=expansion(0)) +
  # data
  geom_area(aes(x,y),
            color=c[2], fill=c[2]) +
  geom_area(aes(ifelse(x>=qnorm(0.900)/sqrt(15), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_area(aes(ifelse(x<=qnorm(0.100)/sqrt(15), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_vline(linetype="solid", xintercept=qnorm(0.900)/sqrt(15)) +
  geom_vline(linetype="solid", xintercept=qnorm(0.100)/sqrt(15)) +
  geom_vline(linetype="dashed", xintercept=0.65) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste(1-beta,"=")))

p5 <- getnd(0.65, 1/sqrt(15)) %>% 
  ggplot() + 
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M",
                     limits=c(-3, 3), breaks=-2:2, 
                     expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 1.7), expand=expansion(0)) +
  # data
  geom_area(aes(x,y),
            color=c[2], fill=c[2]) +
  geom_area(aes(ifelse(x>=qnorm(0.975)/sqrt(15), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_area(aes(ifelse(x<=qnorm(0.025)/sqrt(15), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_vline(linetype="solid", xintercept=qnorm(0.975)/sqrt(15)) +
  geom_vline(linetype="solid", xintercept=qnorm(0.025)/sqrt(15)) +
  geom_vline(linetype="dashed", xintercept=0.65) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste(1-beta,"=")))

p4 <- getnd(0.65, 1/sqrt(15)) %>% 
  ggplot() + 
  # design
  coord_fixed(ratio=1.25) +
  theme_blog() + theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank()
  ) +
  # axes
  scale_x_continuous(name="M",
                     limits=c(-3, 3), breaks=-2:2,
                     expand=expansion(0)) +
  scale_y_continuous(limits=c(0, 1.7), expand=expansion(0)) +
  # data
  geom_area(aes(x,y),
            color=c[2], fill=c[2]) +
  geom_area(aes(ifelse(x>=qnorm(0.995)/sqrt(15), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_area(aes(ifelse(x<=qnorm(0.005)/sqrt(15), x, 1111), y),
            color=c[4], fill=c[4]) +
  geom_vline(linetype="solid", xintercept=qnorm(0.995)/sqrt(15)) +
  geom_vline(linetype="solid", xintercept=qnorm(0.005)/sqrt(15)) +
  geom_vline(linetype="dashed", xintercept=0.65) +
  # annotations
  annotate("rect", color=c[4], fill=c[4],
           xmax=3, xmin=2.8, ymax=0*1.7+0.9, ymin=0*1.7+0.7) +
  annotate("text", size=3,
           hjust=1, vjust=0.5,
           x=2.8, y=0*1.7+0.8, 
           label=expression(paste(1-beta,"=")))

p9 <- data.frame(mu0 = seq(-3, 3, 0.001)) %>% 
  mutate(y = pnorm(qnorm(0.900,0,1/sqrt(15)), mu0, 1/sqrt(15), lower.tail=F) +
           pnorm(qnorm(0.100,0,1/sqrt(15)), mu0, 1/sqrt(15), lower.tail=T)) %>%
  ggplot(aes(mu0, y)) +
  geom_line(size=1, color=c[4]) +
  # axes
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3, 3), breaks=-2:2, 
                     expand=expansion(0)) +
  scale_y_continuous(name="Rejection Rate",
                     limits=c(0,1), breaks=seq(0,1,0.2)) +
  # theme
  theme_blog() + theme(
    aspect.ratio=0.5,
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  # annotations
  geom_vline(xintercept=0, linetype="dashed") +
  geom_vline(xintercept=0.65, linetype="dashed") +
  geom_hline(yintercept=0.20, linetype="dotted") +
  #geom_point(aes(x=0, y=0.05),
  #           shape=22, color=c[4], fill=c[4], size=2) +
  geom_point(aes(x=0.65, y=
                   pnorm(qnorm(0.900,0,1/sqrt(15)), 0.65, 1/sqrt(15), lower.tail=F)+
                   pnorm(qnorm(0.100,0,1/sqrt(15)), 0.65, 1/sqrt(15), lower.tail=T)),
             shape=22, color=c[4], fill=c[4], size=2) +
  annotate("text", label=expression(paste(alpha==0.20)),
           hjust=0, vjust=0, x=-2.9, y=0.21, size=3) 

p8 <- data.frame(mu0 = seq(-3, 3, 0.001)) %>% 
  mutate(y = pnorm(qnorm(0.975,0,1/sqrt(15)), mu0, 1/sqrt(15), lower.tail=F) +
           pnorm(qnorm(0.025,0,1/sqrt(15)), mu0, 1/sqrt(15), lower.tail=T)) %>%
  ggplot(aes(mu0, y)) +
  geom_line(size=1, color=c[4]) +
  # axes
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3, 3), breaks=-2:2, 
                     expand=expansion(0)) +
  scale_y_continuous(name="Rejection Rate",
                     limits=c(0,1), breaks=seq(0,1,0.2)) +
  # theme
  theme_blog() + theme(
    aspect.ratio=0.5,
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  # annotations
  geom_vline(xintercept=0, linetype="dashed") +
  geom_vline(xintercept=0.65, linetype="dashed") +
  geom_hline(yintercept=0.05, linetype="dotted") +
  #geom_point(aes(x=0, y=0.05),
  #           shape=22, color=c[4], fill=c[4], size=2) +
  geom_point(aes(x=0.65, y=
                   pnorm(qnorm(0.975,0,1/sqrt(15)), 0.65, 1/sqrt(15), lower.tail=F)+
                   pnorm(qnorm(0.025,0,1/sqrt(15)), 0.65, 1/sqrt(15), lower.tail=T)),
             shape=22, color=c[4], fill=c[4], size=2) +
  annotate("text", label=expression(paste(alpha==0.05)),
           hjust=0, vjust=0, x=-2.9, y=0.06, size=3)

p7 <- data.frame(mu0 = seq(-3, 3, 0.001)) %>% 
  mutate(y = pnorm(qnorm(0.995,0,1/sqrt(15)), mu0, 1/sqrt(15), lower.tail=F) +
           pnorm(qnorm(0.005,0,1/sqrt(15)), mu0, 1/sqrt(15), lower.tail=T)) %>%
  ggplot(aes(mu0, y)) +
  geom_line(size=1, color=c[4]) +
  # axes
  scale_x_continuous(name=expression(paste("Distance from ", mu[0])),
                     limits=c(-3, 3), breaks=-2:2,
                     expand=expansion(0)) +
  scale_y_continuous(name="Rejection Rate",
                     limits=c(0,1), breaks=seq(0,1,0.2)) +
  # theme
  theme_blog() + theme(
    aspect.ratio=0.5,
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    #axis.title.y = element_blank(),
    #axis.text.y = element_blank()
  ) +
  # annotations
  geom_vline(xintercept=0, linetype="dashed") +
  geom_vline(xintercept=0.65, linetype="dashed") +
  geom_hline(yintercept=0.01, linetype="dotted") +
  #geom_point(aes(x=0, y=0.05),
  #           shape=22, color=c[4], fill=c[4], size=2) +
  geom_point(aes(x=0.65, y=
                   pnorm(qnorm(0.995,0,1/sqrt(15)), 0.65, 1/sqrt(15), lower.tail=F)+
                   pnorm(qnorm(0.005,0,1/sqrt(15)), 0.65, 1/sqrt(15), lower.tail=T)),
             shape=22, color=c[4], fill=c[4], size=2) +
  annotate("text", label=expression(paste(alpha==0.01)),
           hjust=0, vjust=0, x=-2.9, y=0.02, size=3) 


plot20 <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + plot_layout(design=layout)

ggsave(
  plot     = plot20,
  filename = "20_rejection_rate_by_alpha.svg",
  path     = plotdir,
  scale    = 0.6,
  height   = 7,
  width    = 10
)

# 21. Rejection Rate Heatmap ------------------------------------------------------------

plot21 <- expand.grid(
  n = seq(1, 100, 1),
  alpha = 0.05,
  mu = seq(-3, 3, 0.01)
) %>% 
  
  rowwise() %>%
  # critical values
  mutate(left  = qnorm(alpha/2, 0, 1/sqrt(n), lower.tail=T),
         right = qnorm(alpha/2, 0, 1/sqrt(n), lower.tail=F)) %>% 
  # rejection rate
  mutate(rej   = pnorm(left, mu, 1/sqrt(n), lower.tail=T) +
           pnorm(right, mu, 1/sqrt(n), lower.tail=F)) %>% 
  # values: rejection rate = alpha 
  mutate(eq_left  = qnorm(alpha, left, 1/sqrt(n), lower.tail=T),
         eq_right = qnorm(alpha, right, 1/sqrt(n), lower.tail=F)) %>% 
  
  ggplot() +
  
  # design
  theme_blog() + theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_text(angle=0)
  ) +
  # axes
  coord_fixed(xlim  = c(-2,2),
              ylim  = c(1,100),
              ratio = 0.03) +
  scale_x_continuous(name   = expression(paste("Distance from ", mu[0])),
                     breaks = -1:1,
                     expand = expansion(0)) +
  scale_y_continuous(name   = "N",
                     breaks = c(1,seq(0,100,20)),
                     expand = expansion(0)) +
  # data
  geom_raster(aes(x=mu, y=n, fill=rej)) +
  scale_fill_viridis_c(name = "Rejection\nRate",
                       limits = c(0.05, 1),
                       breaks = c(0.05, 1),
                       labels = expression(alpha,1),
                       expand = expansion(0),
                       option = "magma") +
  geom_vline(linetype="dashed",
             color="white",
             xintercept=0) +
  geom_line(linetype="dashed",
            color="black",
            aes(x=eq_right, y=n)) +
  geom_line(linetype="dashed",
            color="black",
            aes(x=eq_left, y=n)) +
  ggtitle(expression(alpha==0.05)) +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(legend.key.height=unit(1.2,"lines")) 

ggsave(
  plot     = plot21,
  filename = "21_rejection_rate_heatmap.svg",
  path     = plotdir,
  scale    = 0.6,
  height   = 4,
  width    = 10
)

# 22. Rejection Rate Heatmap By Alpha ---------------------------------------------------

plot22 <- expand.grid(
  n = seq(1, 100, 1),
  alpha = c(0.2, 0.05, 0.01),
  mu = seq(-3, 3, 0.01)
) %>% 
  
  rowwise() %>%
  # critical values
  mutate(left  = qnorm(alpha/2, 0, 1/sqrt(n), lower.tail=T),
         right = qnorm(alpha/2, 0, 1/sqrt(n), lower.tail=F)) %>% 
  # rejection rate
  mutate(rej   = pnorm(left, mu, 1/sqrt(n), lower.tail=T) +
           pnorm(right, mu, 1/sqrt(n), lower.tail=F)) %>% 
  # values: rejection rate = alpha 
  mutate(eq_left  = qnorm(alpha, left, 1/sqrt(n), lower.tail=T),
         eq_right = qnorm(alpha, right, 1/sqrt(n), lower.tail=F)) %>% 
  
  ggplot() +
  
  # design
  theme_blog() + theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_text(angle=0)
  ) +
  # axes
  coord_fixed(xlim  = c(-2,2),
              ylim  = c(1,100),
              ratio = 0.03) +
  scale_x_continuous(name   = expression(paste("Distance from ", mu[0])),
                     breaks = -1:1,
                     expand = expansion(0)) +
  scale_y_continuous(name   = "N",
                     breaks = c(1,seq(0,100,20)),
                     expand = expansion(0)) +
  # data
  facet_wrap(. ~ alpha, ncol=3, labeller=label_bquote(alpha == .(alpha))) +
  geom_raster(aes(x=mu, y=n, fill=rej)) +
  scale_fill_viridis_c(name = "Rejection\nRate",
                       limits = c(0.01, 1),
                       breaks = c(0.01, 1),
                       expand = expansion(0),
                       option = "magma") +
  geom_vline(linetype="dashed",
             color="white",
             xintercept=0) +
  theme(legend.key.height=unit(0.75,"lines"))

ggsave(
  plot     = plot22,
  filename = "22_rejection_rate_heatmap_by_alpha.svg",
  path     = plotdir,
  scale    = 0.6,
  height   = 3.5,
  width    = 10
)

# 23. Rejection Rate Heatmap By Sigma ---------------------------------------------------

plot23 <- expand.grid(
  n = seq(1, 100, 1),
  alpha = 0.05,
  sigma = c(0.5, 1, 1.5),
  mu = seq(-3, 3, 0.01)
) %>% 
  
  rowwise() %>%
  # critical values
  mutate(left  = qnorm(alpha/2, 0, sigma/sqrt(n), lower.tail=T),
         right = qnorm(alpha/2, 0, sigma/sqrt(n), lower.tail=F)) %>% 
  # rejection rate
  mutate(rej   = pnorm(left, mu, sigma/sqrt(n), lower.tail=T) +
           pnorm(right, mu, sigma/sqrt(n), lower.tail=F)) %>% 
  # values: rejection rate = alpha 
  mutate(eq_left  = qnorm(alpha, left, sigma/sqrt(n), lower.tail=T),
         eq_right = qnorm(alpha, right, sigma/sqrt(n), lower.tail=F)) %>% 
  
  ggplot() +
  
  # design
  theme_blog() + theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_text(angle=0)
  ) +
  # axes
  coord_fixed(xlim  = c(-2,2),
              ylim  = c(1,100),
              ratio = 0.03) +
  scale_x_continuous(name   = expression(paste("Distance from ", mu[0])),
                     breaks = -1:1,
                     expand = expansion(0)) +
  scale_y_continuous(name   = "N",
                     breaks = c(1,seq(0,100,20)),
                     expand = expansion(0)) +
  # data
  facet_wrap(. ~ sigma, ncol=3, labeller=label_bquote(sigma == .(sigma))) +
  geom_raster(aes(x=mu, y=n, fill=rej)) +
  scale_fill_viridis_c(name = "Rejection\nRate",
                       limits = c(0.05, 1),
                       breaks = c(0.05, 1),
                       labels = expression(alpha,1),
                       expand = expansion(0),
                       option = "magma") +
  geom_vline(linetype="dashed",
             color="white",
             xintercept=0) +
  theme(legend.key.height=unit(0.75,"lines")) +
  ggtitle(expression(alpha==0.05)) +
  theme(plot.title=element_text(hjust=0.5, margin=margin(b=10)))

ggsave(
  plot     = plot23,
  filename = "23_rejection_rate_heatmap_by_sigma.svg",
  path     = plotdir,
  scale    = 0.6,
  height   = 4,
  width    = 10
)

# 24. Rejection Rate Heatmap By Sigma Standardized --------------------------------------

plot24 <- expand.grid(
  n = seq(1, 100, 1),
  alpha = 0.05,
  sigma = c(0.5, 1, 1.5),
  z = seq(-2, 2, 0.01)
) %>% 
  
  rowwise() %>%
  mutate(mu = sigma*z) %>% 
  # critical values
  mutate(left  = qnorm(alpha/2, 0, sigma/sqrt(n), lower.tail=T),
         right = qnorm(alpha/2, 0, sigma/sqrt(n), lower.tail=F)) %>% 
  # rejection rate
  mutate(rej   = pnorm(left, mu, sigma/sqrt(n), lower.tail=T) +
           pnorm(right, mu, sigma/sqrt(n), lower.tail=F)) %>% 
  # values: rejection rate = alpha 
  mutate(eq_left  = qnorm(alpha, left, sigma/sqrt(n), lower.tail=T),
         eq_right = qnorm(alpha, right, sigma/sqrt(n), lower.tail=F)) %>% 
  
  ggplot() +
  
  # design
  theme_blog() + theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_text(angle=0)
  ) +
  # axes
  coord_fixed(xlim  = c(-2,2),
              ylim  = c(1,100),
              ratio = 0.03) +
  scale_x_continuous(name   = expression(paste("Distance from ", mu[0])),
                     breaks = -1:1,
                     labels = expression(-1~sigma, 0, +1~sigma),
                     expand = expansion(0)) +
  scale_y_continuous(name   = "N",
                     breaks = c(1,seq(0,100,20)),
                     expand = expansion(0)) +
  # data
  facet_wrap(. ~ sigma, ncol=3, labeller=label_bquote(sigma == .(sigma))) +
  geom_raster(aes(x=z, y=n, fill=rej)) +
  scale_fill_viridis_c(name = "Rejection\nRate",
                       limits = c(0.05, 1),
                       breaks = c(0.05, 1),
                       labels = expression(alpha,1),
                       expand = expansion(0),
                       option = "magma") +
  geom_vline(linetype="dashed",
             color="white",
             xintercept=0) +
  theme(legend.key.height=unit(0.75,"lines")) +
  ggtitle(expression(alpha==0.05)) +
  theme(plot.title=element_text(hjust=0.5, margin=margin(b=10)))

ggsave(
  plot     = plot24,
  filename = "24_rejection_rate_heatmap_by_sigma_standardized.svg",
  path     = plotdir,
  scale    = 0.6,
  height   = 4,
  width    = 10
)
