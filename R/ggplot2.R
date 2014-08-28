## ---- setup_ggplot2
library(ggplot2)

## ---- load_diamond_ggplot2
diamonds = diamonds[sample(NROW(diamonds), size=1000),]
head(diamonds)

## ---- qplot_ggplot2
#these are equivalent:
#qplot(diamonds$carat, diamonds$price, data=diamonds)
#qplot(x=carat, y=price, data=diamonds)
qplot(carat, price, data=diamonds)

## ---- qplot_colour_ggplot2
qplot(carat, price, data=diamonds, colour=clarity)

## ---- qplot_overplotting_ggplot2
qplot(carat, price, data=diamonds, colour=clarity, alpha=I(1/2))

## ---- qplot_log_ggplot2
qplot(log(carat), log(price), data=diamonds, colour=clarity)

## ---- hw_load_ggplot2
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

## ---- hw_plot_ggplot2
qplot(x=weight, y=health, data=hw, size=height, colour=I("steelblue"))

## ---- hw_lm_ggplot2
qplot(x=weight, y=health, data=hw) + geom_smooth(method=lm)

## ---- hw_regression_ggplot2
fit <- lm(health~weight, data=hw)
hii <- hatvalues(fit) #leverages
res <- fit$res #residuals

qplot(x=weight, y=health, data=hw, size=hii, colour=abs(res)) +
  geom_abline(intercept=fit$coeff[1], slope=fit$coeff[2]) #regression line

## ---- diamonds_plot_ggplot2
p <- ggplot(data=diamonds, aes(x=carat,y=price,colour=cut)) #init. plot, specifying data and aes
p <- p + layer(geom="point") # add a layer with points geom
p #render plot


## ---- diamonds_hist_ggplot2
p <- ggplot(diamonds, aes(x=carat)) #init
p <- p + layer(
  geom="bar",
  geom_params=list(fill="steelblue"),
  stat="bin",
  stat_params=list(binwidth=.25)
)
p #render the plot

## ---- diamonds_stat_ggplot2
#specify the geom, using default stat
ggplot(diamonds, aes(x=carat)) + geom_histogram(binwidth=.25, fill="steelblue")

#specify the stat, using default geom
ggplot(diamonds, aes(x=carat)) + stat_bin(binwidth=.25, fill="steelblue") + geom_density()


## ---- hw_aes_ggplot2
p2 <- ggplot(data=hw) #initialize

p2 <- p2 + aes(x=height, y=health) #specify a mapping
p2 + geom_point() #render

p2 <- p2 + aes(x=weight, y=health) #change mapping 
p2 + geom_point() 

#add another aesthetic (colour)
p2 + geom_point(aes(colour=height)) 

#we can also remove aesthetics
p2 + geom_point(aes(colour=NULL)) 

#instead of mapping aesthetics to a variable, we can set them to a constant
p2 + geom_point(colour="darkblue") #set col to darkblue

#This is different from an aesthetic mapping:
p2 + geom_point(aes(colour="darkblue"))  #create a new variable called "darkblue", and map it to color

## ---- arrests_bar_ggplot2
data(package='effects', 'Arrests')

head(Arrests)

dat <- data.frame(colour= factor(c("Black", "White"), levels=c("Black","White")), Percent_Released = c(0.74, 0.85))

# basic bar graph
ggplot(dat, aes(x=colour, y=Percent_Released)) + geom_bar(stat="identity")

# Fill different fill colors.
ggplot(dat, aes(x=colour, y=Percent_Released)) + geom_bar(aes(fill=colour), stat="identity")

# Add a black outline
ggplot(dat, aes(x=colour, y=Percent_Released, fill=colour)) + geom_bar(colour="black", stat="identity")

# Removing the legend
ggplot(dat, aes(x=colour, y=Percent_Released, fill=colour)) +
 geom_bar(colour="black", stat="identity") +
 guides(fill=FALSE)


## ---- arrests_histogram_ggplot2
# Overlaid histograms
ggplot(Arrests, aes(x=checks, fill=released)) + geom_histogram(binwidth=1, alpha=.5, position="identity")
# conclusions from this plot: if you have more checks, you are much less likely to be released


## ---- arrests_boxplot_ggplot2
#specify the theme
p <- ggplot(data=Arrests) + theme(plot.title=element_text(lineheight=.8, face="bold"))

p + geom_boxplot(mapping=aes(x=colour, y=unclass(checks))) + ggtitle("Prior Police Checks by Race") 

p + geom_boxplot(mapping=aes(x=released, y=unclass(checks))) + ggtitle("Prior Police Checks by Released (Yes/No)")

# faceting
p + facet_wrap(~released) +
  geom_boxplot(mapping=aes(x=colour, y=unclass(checks), color=colour)) +
  ggtitle("Prior Police Checks by Race and Released (Yes/No)")

## ---- rcookbook_ggplot2
# Basic histogram from the vector "rating". Each bin is .5 wide.

df <- data.frame(cond = factor( rep(c("A","B"), each=200) ), rating = c(rnorm(200),rnorm(200, mean=.5)))
###simulate data from mixed dist. 0.5N(0,1) + 0.5N(0.5,1)

ggplot(df, aes(x=rating)) + geom_histogram(binwidth=.5)

# Draw with black outline, white fill
ggplot(df, aes(x=rating)) + geom_histogram(binwidth=.5, colour="black", fill="white")

# Density curve
ggplot(df, aes(x=rating)) + geom_density()

# Histogram overlaid with kernel density curve
ggplot(df, aes(x=rating)) + geom_histogram(aes(y=..density..),      
# Histogram with density instead of count on y-axis
binwidth=.5,
colour="black", fill="white") +
geom_density(alpha=.2, fill="pink")  # Overlay with transparent density plot
