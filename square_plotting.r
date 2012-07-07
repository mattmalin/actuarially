library(ggplot2)
values <- as.list(possible_solutions[[1]])
 
square_positions <- list()

square_positions[["u"]] <- with(values, data.frame(
  x1 = 0,
  y1 = 0,
  x2 = u,
  y2 = u,
  square = "u"))

square_positions[["t"]] <- with(values, data.frame(
  x1 = u,
  y1 = 0,
  x2 = u + t,
  y2 = t,
  square = "t"))
  
square_positions[["r"]] <- with(values, data.frame(
  x1 = u + t,
  y1 = 0,
  x2 = u + t + r,
  y2 = r,
  square = "r"))

square_positions[["k"]] <- with(values, data.frame(
  x1 = 0,
  y1 = u,
  x2 = k,
  y2 = u + k,
  square = "k"))
  
square_positions[["s"]] <- with(values, data.frame(
  x1 = k,
  y1 = u,
  x2 = k + s,
  y2 = u + s,
  square = "s"))
  
square_positions[["l"]] <- with(values, data.frame(
  x1 = k,
  y1 = u + s,
  x2 = k + l,
  y2 = u + s + l,
  square = "l"))

square_positions[["q"]] <- with(values, data.frame(
  x1 = k + l,
  y1 = t,
  x2 = k + l + q,
  y2 = t + q,
  square = "q"))

square_positions[["m"]] <- with(values, data.frame(
  x1 = k + l,
  y1 = t + q,
  x2 = k + l + m,
  y2 = t + q + m,
  square = "m"))

square_positions[["o"]] <- with(values, data.frame(
  x1 = k + l + m,
  y1 = t + q,
  x2 = k + l + m + o,
  y2 = t + q + o,
  square = "o"))

square_positions[["n"]] <- with(values, data.frame(
  x1 = k + l + m,
  y1 = t + q + o,
  x2 = k + l + m + n,
  y2 = t + q + o + n,
  square = "n"))

square_positions[["p"]] <- with(values, data.frame(
  x1 = k + l + q,
  y1 = r,
  x2 = k + l + q + p,
  y2 = r + p,
  square = "p"))

square_positions[["j"]] <- with(values, data.frame(
  x1 = k + l + q + p,
  y1 = r,
  x2 = k + l + q + p + j,
  y2 = r + j,
  square = "j"))

square_positions[["i"]] <- with(values, data.frame(
  x1 = a + f + g,
  y1 = r + p,
  x2 = a + f + g + i,
  y2 = r + i,
  square = "i"))

square_positions[["a"]] <- with(values, data.frame(
  x1 = 0,
  y1 = k + u,
  x2 = a,
  y2 = k + u + a,
  square = "a"))

square_positions[["b"]] <- with(values, data.frame(
  x1 = a,
  y1 = t + q + m + f,
  x2 = a + b,
  y2 = t + q + m + f + b,
  square = "b"))

square_positions[["c"]] <- with(values, data.frame(
  x1 = a + b,
  y1 = r + j + e,
  x2 = a + b + c,
  y2 = r + j + e + c,
  square = "c"))

square_positions[["d"]] <- with(values, data.frame(
  x1 = a + b,
  y1 = r + j + h,
  x2 = a + b + d,
  y2 = r + j + h + d,
  square = "d"))

square_positions[["e"]] <- with(values, data.frame(
  x1 = a + b + d,
  y1 = r + j,
  x2 = a + b + d + e,
  y2 = r + j + e,
  square = "e"))

square_positions[["f"]] <- with(values, data.frame(
  x1 = a,
  y1 = t + l,
  x2 = a + f,
  y2 = t + l + f,
  square = "f"))

square_positions[["g"]] <- with(values, data.frame(
  x1 = a + f,
  y1 = t + q + o,
  x2 = a + f + g,
  y2 = t + q + o + g,
  square = "g"))

square_positions[["h"]] <- with(values, data.frame(
  x1 = a + f + g,
  y1 = r + p + i,
  x2 = a + f + g + h,
  y2 = r + p + i + h,
  square = "h"))

square_positions[["i"]] <- with(values, data.frame(
  x1 = a + f + g,
  y1 = r + p,
  x2 = a + f + g + i,
  y2 = r + p + i,
  square = "i"))

square_positions_df <- do.call("rbind", square_positions)

set.seed(1)
ggplot(square_positions_df) +  
  scale_x_continuous(name="x") + 
  scale_y_continuous(name="y") +
  geom_rect(aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = factor(square, levels = sample(levels(square)))), colour = "black") +
  geom_text(aes(x = x1 + (x2 - x1) / 2, y = y1 + (y2 - y1) / 2, label = square)) +
  opts(legend.position = "none", title = "21 squares")

####
