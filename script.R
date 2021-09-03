##---- LIBRARY

library(dplyr)
library(purrr)
library(ggforce)
library(Rcpp)
library(tidyverse)

##---- LISSAJOUS FUNCTION

lj <- function(a = 1, b = 1, A = 1, B = 1, d = 200, delta = 0.01*pi) {
  result <- data.frame(theta = seq(0, 10*pi, delta)) %>% 
    mutate(x = A*sin(a*theta + d), y = B*sin(b*theta))
  result
}

##---- DATA FRAME

set.seed(1111)
n=100

df <- 1:n %>% map_df(~lj(a = runif(1, 0, 10), A = runif(1, 0, 1)), .id = "id")


##---- PLOT

p <- ggplot() +
  geom_path(aes(x, y), df, size = 0.5, lineend = "round") +
  facet_wrap(~id, nrow = sqrt(n)) +
  coord_equal() +
  theme_blankcanvas(margin_cm = 0.75)

# save the plot
ggsave("lj006.png", p, width = 20, height = 20, units = "cm", dpi = 300)

