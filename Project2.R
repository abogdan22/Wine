#Project 2: Wine
#Stat 6021
#Summer 2021
#Alice Bogdan, Tyler Entner, Carly Kelly, Christian Prosser

#necessary libraries
library(MASS) 
library(lawstat) #for levene.test()
library(multcomp) #for pairise, glht
library(tidyverse)
library(ggplot2) #EDA
library(dplyr)
library(gridExtra)
library(faraway)
library(ROCR)
library(nnet) #multinomial regression model

#data
red <- read.csv("wineQualityReds.csv", header = TRUE, sep = ',')
white <- read.csv("wineQualityWhites.csv", header = TRUE, sep = ',')

#combined dataset for red and white wine. Color = 1 for red wine, 0 for white wine
combined <- read.csv("combined_wine.csv", header = TRUE, sep = ',')
#nrow(filter(combined, color == 1))

################################################
#Exploratory Data Analysis
###############################################
#average wine quality
mean(red$quality) #5.63
mean(white$quality) #5.88
mean(combined$quality) #5.82

#min and maxs of wine quality
min(red$quality) #3
max(red$quality) #8
min(white$quality) #3
max(white$quality) #9

#distribution of wine predictors
#bar plot quality
#ggplot(data = combined) + geom_bar(mapping = aes(x = quality))+ggtitle("Bar Plot: Quality")
#bar plot color
ggplot(data = combined) + geom_bar(mapping = aes(x = color))+ggtitle("Bar Plot: Color")

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#create a df specifically for histogram matrix (removes color)
drops <- c("color")
combined_hist <- combined[ , !(names(combined) %in% drops)]

#for look for histogram matrix
myplots <- list()  # new empty list
for(i in 1:ncol(combined_hist)){
  col <- names(combined_hist)[i]
  ggp <- ggplot(combined_hist, aes_string(x = col)) +
    geom_histogram(bins = 30, fill = "cornflowerblue", color = "black") 
  myplots[[i]] <- ggp  # add each plot into plot list
}

#histogram matrix
multiplot(plotlist = myplots, cols = 4)

################################################
#Collapse Quality to be Binomial
###############################################
#are the categorical predictors numerical?
is.numeric(combined$quality)

#create copy of combined df
combined_collapse <-data.frame(combined)

#have R treat quality as categorical variables
combined_collapse$quality <- factor(combined_collapse$quality)
#collapse Quality to be Good and Not Good
contrasts(combined_collapse$quality) #reference class is 3
levels(combined_collapse$quality)

#match with levels(combined_collapse$quality) order
new.quality <- c("Not Good", "Not Good","Not Good","Good","Good","Good","Good") 

#add new binary variable to dataframe
combined_collapse$quality_binary <- factor(new.quality[combined_collapse$quality])
contrasts(combined_collapse$quality_binary) #Good is reference class

#relevel quality_binary reference class
combined_collapse$quality_binary <- relevel(combined_collapse$quality_binary, ref = "Not Good")
contrasts(combined_collapse$quality_binary) #Now Not Good is reference class

#create a df specifically removing original quality predictor
drops2 <- c("quality")
combined_collapse <- combined_collapse[ , !(names(combined_collapse) %in% drops2)]

################################################
#Model
###############################################
#fit intercept only and full model
lognull <- glm(quality_binary ~ 1, family = "binomial", data = combined_collapse)
logfull <- glm(quality_binary ~ ., family = "binomial", data = combined_collapse)

#starting and ending model have to be specified so the algorithm knows where to start and how far to go in the search procedure
#to carry out procedures, type
step(lognull, scope=list(lower=lognull, upper=logfull), direction="forward") #forward selection
step(logfull, scope=list(lower=lognull, upper=logfull), direction="backward") #backward elimination
step(lognull, scope=list(lower=lognull, upper=logfull), direction="both") #stepwise regression

#fitted logistic regression model (using forward selection)
for_result <- glm(quality_binary ~ alcohol + volatile.acidity + sulphates + residual.sugar + color + density + free.sulfur.dioxide + total.sulfur.dioxide + pH + fixed.acidity + citric.acid, family = "binomial", data = combined_collapse)
summary(for_result)
vif(for_result)

#drop density
for_red <- glm(quality_binary ~ alcohol + volatile.acidity + sulphates + residual.sugar + color + free.sulfur.dioxide + total.sulfur.dioxide + pH + fixed.acidity + citric.acid, family = "binomial", data = combined_collapse)


#is the model useful?
1-pchisq(for_result$null.deviance-for_result$deviance,11) #11 predictors
#small pvalue. Therefore reject the null (all coefficients are 0). At least one of the predictors is nonzero. Correspondings to individual pvalues of z tests. 

