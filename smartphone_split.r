17 + 14 + 11 +9 +34 + 9 + 2.9 +0.1 +0.2 +0.5 + 0.3 +  0.5 + 0.9 + 0.6
(51.8/51)*(17 + 14 + 11 + 9) + 34.3 + 8.1 + 2.9 +0.1 +0.2 +0.5 + 0.3 + 0.5 + 0.9 + 0.6

# from http://blog.nielsen.com/nielsenwire/?p=32494

# BAD VISUALISATION!

phone_manufacturer_split <- data.frame(
  operating_system = c(
    rep("Android OS", 4), 
	"Apple iPhone", 
	"RIM Blackberry", 
	rep("Windows Mobile", 3), 
	rep("Windows 7", 3),
	"Symbian",
	"Palm/WebOS"),
  manufacturer = c(
    "Samsung", "HTC", "Motorola", "Other",
	"Apple",
	"RIM Blackberry",
	"HTC", "Palm", "Other",
	"Samsung", "Nokia", "HTC",
	"Nokia",
	"Palm"),
  share = c(
    0.17, 0.14, 0.11, 0.09,
	0.34,
	0.09,
	0.029, 0.001, 0.002,
	0.005, 0.003, 0.005,
	0.009, 0.006
  )
)
# Some checks:
sum(phone_manufacturer_split$share) # should be 1
# [1] 1


combined <- aggregate(share ~ operating_system , phone_manufacturer_split, sum)
names(combined)[which(names(combined) == "share")] <- "os_share"
combined <- within(combined, cumsum <- cumsum(os_share))
combined <- within(combined, os_midpoint <- cumsum - 0.5 * os_share)

phone_manufacturer_split <- merge(
  phone_manufacturer_split, 
  combined[c("operating_system", "os_share", "os_midpoint")], 
  by = "operating_system")

phone_manufacturer_split <- within(
  phone_manufacturer_split,
  percentage_of_os_split <- share / os_share)

# the actually GOOD plot: 
ggplot(phone_manufacturer_split, aes(x = factor(operating_system, levels = combined$operating_system[order(combined$os_share, decreasing = TRUE)]), y = share, fill = manufacturer)) + geom_bar()


ggplot(phone_manufacturer_split, aes(x = os_midpoint, y = share, fill = manufacturer)) + 
  geom_bar() +
  scale_x_continuous(breaks = combined$os_midpoint, labels = combined$operating_system)

library(ggplot2)

the_plot <- ggplot(phone_manufacturer_split, aes(x = os_midpoint, y = percentage_of_os_split, fill = manufacturer)) 
the_plot <- the_plot + geom_bar(aes(width = os_share), stat = "identity", position = "stack")
the_plot <- the_plot + geom_text(aes( x = os_midpoint, y = percentage_of_os_split, label = manufacturer), position = "stack")
the_plot




#####

ggplot(phone_manufacturer_split, aes(x = os_midpoint, y = percentage_of_os_split, 

the_plot + scale_x_discrete(breaks = combined$os_midpoint, )

the_plot <- the_plot + geom_tile(aes(width = os_share))

ggplot(phone_manufacturer_split, aes(x = os_share, y = share, fill = factor(manufacturer))) + geom_tile()
ggplot(phone_manufacturer_split, aes(x = os_share, y = share, fill = factor(manufacturer), width = os_share)) + geom_tile(position = "fill")

x.cell.boundary <- c(0, 4, 6, 8, 10, 14) 
example <- data.frame( 
  x = rep(c(2, 5, 7, 9, 12), 2), 
  y = factor(rep(c(1,2), each=5)), 
  z = rep(1:5, each=2), 
  w = rep(diff(x.cell.boundary), 2) 
) 

qplot(x, y, fill=factor(z), data=example, geom="tile", width=w) 