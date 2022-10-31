library(here)
library(ggplot2)

source(here("content", "blog", "plotsetup.R"))

savedir  <- here("static")
filename <- "logo.png"

x <- seq(-5,5,.01)
y <- dcauchy(x)

data.frame(x=x, y=y) |> 
  ggplot(aes(x=x, y=y)) +
  theme_blog() +
  ax(xl=c(-5,5), yl=c(0, max(y))) +
  geom_area(color="black", fill=color$grayd)

ggsave(
  filename = filename,
  path     = savedir,
  scale    = .6,
  width    = 3,
  height   = 1
)
