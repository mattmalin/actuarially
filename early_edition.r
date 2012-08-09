library(XML)
library(ggplot2)

ep_ratings <- readHTMLTable(
  readLines("http://www.imdb.com/title/tt0115163/epdate"), 
  which = 1, 
  stringsAsFactors = FALSE)

ep_ratings <- ep_ratings[c("#", "Episode", "UserRating")]
  
ep_ratings <- within(ep_ratings, ep_number <- 1:{nrow(ep_ratings)})

ggplot(ep_ratings, aes(x = as.numeric(UserRating), y = `#`)) + geom_point() + scale_x_continuous(limits = c(0, 10))