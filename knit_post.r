library(knitr)
library(markdown)

knit2html("actuary_april_2012.Rmd", "actuary_april_2012.html")
browseURL("actuary_april_2012.html")

knit("smartphone_split.Rmd", "smartphone_split.Md")
markdownToHTML("smartphone_split.Md", "smartphone_split.html", fragment.only = TRUE)

knit2html("smartphone_split.r", "smartphone_split.html")
browseURL("smartphone_split.html")


