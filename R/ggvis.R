## ---- setup_ggvis
library(ggvis)
library(dplyr)

## ---- load_diamond_ggvis
data(diamonds, package='ggplot2')
diamonds = diamonds[sample(NROW(diamonds), size=1000),]
head(diamonds)

## ---- qplot_ggvis
#these are equivalent:
#ggvis(x=~carat, y=~price, data=diamonds)
#ggvis(~carat, ~price, data=diamonds)
diamonds %>% ggvis(~carat, ~price)

## ---- qplot_colour_ggvis
diamonds %>% ggvis(~carat, ~price, fill=~clarity)

## ---- qplot_overplotting_ggvis
diamonds %>% ggvis(~carat, ~price, fill=~clarity) %>% layer_points(opacity:=1/2)

## ---- qplot_log_ggvis
diamonds %>% ggvis(~log(carat), ~log(price), fill=~clarity)

## ---- hw_load_ggvis
cat("
   height weight health 
1  0.6008 0.3355  1.280 
2  0.9440 0.6890  1.208 
3  0.6150 0.6980  1.036 
4  1.2340 0.7617  1.395 
5  0.7870 0.8910  0.912 
6  0.9150 0.9330  1.175 
7  1.0490 0.9430  1.237 
8  1.1840 1.0060  1.048 
9  0.7370 1.0200  1.003 
10 1.0770 1.2150  0.943 
11 1.1280 1.2230  0.912 
12 1.5000 1.2360  1.311 
13 1.5310 1.3530  1.411 
14 1.1500 1.3770  0.603 
15 1.9340 2.0734  1.073 ", 
file='height_weight.dat')

hw <- read.table('height_weight.dat', header=T)

head(hw)

## ---- hw_plot_ggvis
hw %>% ggvis(~weight, ~health, size=~height, fill:="steelblue")

## ---- hw_lm_ggvis
hw %>% ggvis(~weight, ~health) %>% layer_points() %>% layer_model_predictions(model='lm', se=TRUE)


## ---- hw_regression_ggvis
fit <- lm(health~weight, data=hw)
hii <- hatvalues(fit) #leverages
res <- fit$res #residuals
data.frame(hw, leverage=hii, residual=res) %>%
  ggvis(~weight, ~health) %>%
  layer_points(size=~leverage, fill=~abs(residual)) %>%
  layer_model_predictions(model='lm') %>%
  # This is needed because ggvis does not automatically prevent legends from overplotting
  # This will be fixed in future releases
  add_legend("size", properties = legend_props(legend = list(y = 100)))


## ---- diamonds_plot_ggvis
p <- ggvis(diamonds, x=~carat, y=~price, fill=~cut)
p <- p %>% layer_points()
p #render plot

## ---- diamonds_hist_ggvis
p <- ggvis(diamonds, x=~carat)
p <- p %>% layer_histograms(fill:="steelblue", binwidth=.25)
p

## ---- diamonds_stat_ggvis
diamonds %>% ggvis(~carat) %>% layer_histograms(binwidth=.25, fill:="steelblue")

## ---- hw_aes_ggvis
p2 <- hw

p2 <- hw %>% ggvis() %>% add_props(x=~height, y=~health)
p2 #render

p2 <- p2 %>% add_props(x=~weight, y=~health) # change mapping
p2

#add another aesthetic (colour)
p2 %>% layer_points(fill=~height)

#there is not currently a way to remove a mapping

#instead of mapping aesthetics to a variable, we can set them to a constant
p2 %>% layer_points(fill:="darkblue") #set col to darkblue

#incorrectly mapping an aesthetic to a scalar is not possible in ggvis.

## ---- arrests_bar_ggvis
data(package='effects', 'Arrests')

head(Arrests)

dat <- data.frame(count=c(1L,2L), colour=c("Black", "White"), Percent_Released = c(0.74, 0.85))

# basic bar graph
dat %>% ggvis(x=~count, y=~Percent_Released) %>% layer_bars()

# Fill different fill colors.
dat %>%
  ggvis(x=~count, y=~Percent_Released, fill:=~colour) %>%
  group_by(colour)  %>%
  layer_bars()

# ggvis has a black outline by default

# Removing the legend broken right now, see issue #229
dat %>% group_by(colour) %>%
  ggvis(x=~count, y=~Percent_Released, fill=~colour) %>%
  layer_bars() %>%
  hide_legend("fill")


## ---- arrests_histogram_ggvis
# Overlaid histograms
Arrests %>%
  group_by(released) %>%
  ggvis(x= ~checks, fill= ~released) %>%
  layer_histograms(opacity:=1/2, stack=FALSE)

## ---- rcookbook_ggvis
# Basic histogram from the vector "rating". Each bin is .5 wide.

df <- data.frame(cond = factor( rep(c("A","B"), each=200) ), rating = c(rnorm(200),rnorm(200, mean=.5)))
###simulate data from mixed dist. 0.5N(0,1) + 0.5N(0.5,1)

df %>% ggvis(x=~rating) %>% layer_histograms(binwidth=.5)

# Draw with black outline, white fill
df %>% ggvis(x=~rating) %>% layer_histograms(binwidth=.5, fill:="white")

# Density curve
df %>% ggvis(x=~rating) %>% layer_densities()

# It is not currently possible to have a density and histogram on the same plot using ggvis
