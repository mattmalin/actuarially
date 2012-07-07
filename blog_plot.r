library(ggplot2)
values <- data.frame(value = c(25 - rlnorm(800, 1, 2), 10 + rnorm(1200, 1, 8), rlnorm(900, 2, 1.9)), type = c(rep(1, 800), rep(2, 1200), rep(3, 900)))
ggplot(values, aes(x = value, y = ..count.., fill = factor(type))) + 
  geom_density() + 
  xlim(c(-10, 35)) + 
  opts(legend.position = "off")
