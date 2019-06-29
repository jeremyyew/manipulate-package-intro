# Preview ---------------------------------------------------------------------
# Remember to set your working directory to the current folder with the data. 

# Example
install.packages("manipulate")
library(manipulate)
library(ggplot2)

# Supply a single plotting expression, and any number of input controls that are assigned to variables, as parameters to the manipulate function.  
manipulate(
  plot(cars, xlim = c(x.min, x.max), 
       type = plot_type, 
       axes = axes, 
       ann = label), 
  # Refer to variables defined below. Order of parameters does not matter. 
  x.min = slider(0, 15),
  x.max = slider(15, 30, initial = 25),
  plot_type = picker("p", "l", "b"),
  axes = checkbox(TRUE, "Axes"),
  label = checkbox(TRUE, "Labels")
)

# Example 1: Exploring relationships in the attitude dataset -----------------------------------------------------------------------

# Load the installed packages required to complete the task.
# Note that aes_string() is used instead of aes().
# This is to allow input of a string.
manipulate(
  ggplot(attitude, aes_string(x=x.variable,y="rating"))+
    geom_point()+geom_smooth(method = method),
  # Create 2 picker options:
  # 1. x.variable, which contains the names of all the categories
  # that can be plotted on the x axis.
  # 2. method, which containes the different methods of smoothing
  # that can be used to smooth the data.
  # x.variable and method can be altered using the pickers.
  x.variable=picker("rating","complaints","privileges",
                    "learning","raises","critical","advance"),
  method=picker("lm", "glm", "gam", "loess", "rlm")
)

# Example 2: Visualizing distributions in the mpg dataset --------------------------------------------------------------------------
# Begin manipulate function.
manipulate(
  ggplot(data = mpg, aes_string(x, y))+  #prepare ggplot using mpg     dataset. we use x and y as variables that we can manipulate later.                                    
    geom_boxplot(fill = "white", colour = "#3366FF")+  #plot boxplots # adjust colour and fill to make them more visible.
    theme(axis.text.x = element_text(angle = 90,  #rotate x labels to be perpendicular to the x-axis.
                                     vjust = 0.5,
                                     hjust = 1)),
  x = picker(as.list(names(mpg))),    #insert slider for x-variable. we use as.list because we are selecting the variables of the mpg dataset. 
  y = picker(as.list(names(mpg)))     #insert slider for y-variable.
)

# Example 3: Browsing the ASEAN Tourism dataset -----------------------------------------------------------------------

# Prepare the data. 
asean_tourism <- read.csv("asean_tourism.csv",
                          stringsAsFactors = FALSE)
names(asean_tourism)[1] <- "country"


manipulate(
  ggplot(asean_tourism, aes_string("country", paste("X", year, sep=""),
                                   fill="country")) +
    geom_bar(stat="identity") + 
    theme(legend.position="none",
          axis.text.x = element_text(angle = 90, 
                                     vjust = 0.5,
                                     hjust = 1)),
  year = picker(as.list(c(2004:2015)))
)


# Example 4: Inserting multiple lines of code into manipulate -----------------------------------------------------------------------
# With curly braces 

manipulate(
  {x <- faithful[, 2] 
  bins <- seq(min(x), max(x), length.out = numBins + 1)
  hist(x, breaks = bins, col = 'darkgray', border = 'white')},
  
  numBins = slider(min = 1, max = 50, initial = 30, label = "Number of bins:")
)


# With a function

f <- function(numBins) {
  x <- faithful[, 2] 
  bins <- seq(min(x), max(x), length.out = numBins + 1)
  hist(x, breaks = bins, col = 'darkgray', border = 'white')
}

manipulate(
  f(numBins),
  numBins = slider(min = 1, max = 50, initial = 30, label = "Number of bins:")
)

# Example 5: Generating graphics options quickly --------------------------------------------------------------------------
library(RColorBrewer)

# Data and ggplot2 idea from: 
# https://4dpiecharts.com/2011/08/17/user2011-easy-interactive-ggplots-talk/

chromium <- read.csv("chromium.csv", stringsAsFactors = FALSE)
nickel <- read.csv("nickel.csv", stringsAsFactors = FALSE)

ggplot(chromium, aes(air, bm, colour = welding.type)) +
  geom_point() +
  geom_smooth(aes(group = ""), method = "lm")

ggplot(nickel, aes(air, bm, colour = welding.type)) +
  geom_point() +
  geom_smooth(aes(group = ""), method = "lm")

# Code that throws errors because picker() defaults to the first choice,
# unless initial = "" is specified. It's always best to choose an option
# you now will work.

manipulate({
  p <- ggplot(data, aes(air, bm, colour = welding.type)) + geom_point()
  p + geom_smooth(aes(group = ""), method = "lm", colour = lm) +
    scale_x_continuous(trans = xScale) +
    scale_y_continuous(trans = yScale) +
    scale_colour_manual(values = brewer.pal(4, scater))
},
# Pick appropriate transformation
yScale = picker("asn", "atanh", "boxcox",
                "exp", "identity", "log",
                "log10", "log1p", "log2", "logit",
                "probability", "probit", "reciprocal",
                "reverse", "sqrt",
                label = "Y Scale Transformation"),
xScale = picker("asn", "atanh", "boxcox",
                "exp", "identity", "log",
                "log10", "log1p", "log2", "logit",
                "probability", "probit", "reciprocal",
                "reverse", "sqrt",
                label = "X Scale Transformation"),
# Switch between datasets
data = picker("Chromium" = chromium, "Nickel" = nickel, label = "Datasets"),
# Pick appropriate point colour (searchable)
scater = picker(as.list(rownames(brewer.pal.info)),
                label = "Palette"),
# Pick appropriate regresion colour (searchable)
lm = picker(as.list(colours()),
            label = "Regression")
)

# Use manipualte to pick optimal representations before finalizing plot

manipulate({
  p <- ggplot(data, aes(air, bm, colour = welding.type)) + geom_point()
  p + geom_smooth(aes(group = ""), method = "lm", colour = lm) +
    scale_x_continuous(trans = xScale) +
    scale_y_continuous(trans = yScale) +
    scale_colour_manual(values = brewer.pal(4, scater))
},
# Pick appropriate transformation
yScale = picker("asn", "atanh", "boxcox",
                "exp", "identity", "log",
                "log10", "log1p", "log2", "logit",
                "probability", "probit", "reciprocal",
                "reverse", "sqrt", initial = "identity",
                label = "Y Scale Transformation"),
xScale = picker("asn", "atanh", "boxcox",
                "exp", "identity", "log",
                "log10", "log1p", "log2", "logit",
                "probability", "probit", "reciprocal",
                "reverse", "sqrt", initial = "identity",
                label = "X Scale Transformation"),
# Switch between datasets
data = picker("Chromium" = chromium, "Nickel" = nickel, label = "Datasets"),
# Pick appropriate point colour (searchable)
scater = picker(as.list(rownames(brewer.pal.info)),
                label = "Palette"),
# Pick appropriate regresion colour (searchable)
lm = picker(as.list(colours()),
            label = "Regression")
)
