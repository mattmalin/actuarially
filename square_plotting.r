
values <- as.list(looped_possible[[1]])
sum(values[c("a", "b", "c")])
sum(values[c("u", "t", "r")])

d=data.frame(x1=c(1,3,1,5,4), x2=c(2,4,3,6,6), y1=c(1,1,4,1,3), y2=c(2,2,5,3,5), t=c('a','a','a','b','b'), r=c(1,2,3,4,5))
ggplot() + 
scale_x_continuous(name="x") + 
scale_y_continuous(name="y") +
geom_rect(data=d, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t), color="black", alpha=0.5) +
geom_text(data=d, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r), size=4) +
opts(title="geom_rect", plot.title=theme_text(size=40, vjust=1.5))


library(ggplot2)

squares <- data.frame(
  x1 = c(0),
  y1 = c(0),
  x2 = c(values$u),
  y2 = c(values$u),
  square = c("u"))

  
  
square_positions <- list()

square_positions[["u"]] <- data.frame(
  x1 = 0,
  y1 = 0,
  x2 = values$u,
  y2 = values$u,
  square = "u")

square_positions[["t"]] <- data.frame(
  x1 = values$u,
  y1 = 0,
  x2 = with(values, u + t),
  y2 = with(values, t),
  square = "t")

square_positions[["r"]] <- data.frame(
  x1 = with(values, u + t),
  y1 = 0,
  x2 = with(values, u + t + r),
  y2 = with(values, r),
  square = "r")

square_positions[["r"]] <- data.frame(
  x1 = with(values, u + t),
  y1 = 0,
  x2 = with(values, u + t + r),
  y2 = with(values, r),
  square = "r")

square_positions_df <- do.call("rbind", square_positions)

ggplot(square_positions_df) +  
  scale_x_continuous(name="x") + 
  scale_y_continuous(name="y") +
  geom_rect(aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = square)) +
  geom_text(aes(x = x1 + (x2 - x1) / 2, y = y1 + (y2 - y1) / 2, label = square))
