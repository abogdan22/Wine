#University of Virginia (MSDS)
#Stat 6021
#Summer 2021
#Project 2: Wine
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
library(caret)
library(corrplot)

#data
red <- read.csv("wineQualityReds.csv", header = TRUE, sep = ',')
white <- read.csv("wineQualityWhites.csv", header = TRUE, sep = ',')

#combined dataset for red and white wine. Color = 1 for red wine, 0 for white wine
combined <- read.csv("combined_wine.csv", header = TRUE, sep = ',')
#nrow(filter(combined, color == 1))

#drop X from red
drops_Xr <- c("X")
red <- red[ , !(names(red) %in% drops_Xr)]
#drop X from white
drops_Xw <- c("X")
white <- white[ , !(names(white) %in% drops_Xw)]

################################################
#Preprocess (Standardization)
###############################################
###Standardization did not work for me
#preprocess data
#summary(red)
#preproc_r <- preProcess(red[,c("pH","alcohol")], method = c("center","scale"))

#perform normalization
#norm_r <- predict(preproc_r, red[,c("pH","alcohol")])

#data after normalization
#summary(red)

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

################################################
#Scatterplot Matrix
###############################################

#scatterplot matrix
#pairs(red[,c(1:6,12)], pch = 19, lower.panel = NULL)
#pairs(red[,7:12], pch = 19, lower.panel = NULL)

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

#for loop for histogram matrix
myplots <- list()  # new empty list
for(i in 1:ncol(combined_hist)){
  col <- names(combined_hist)[i]
  ggp <- ggplot(combined_hist, aes_string(x = col)) +
    geom_histogram(bins = 30, fill = "cornflowerblue", color = "black") 
  myplots[[i]] <- ggp  # add each plot into plot list
}

multiplot(plotlist = myplots, cols = 4)

################################################
#Correlation Plots
###############################################
#red
corr_r <- cor(red[,])
corrplot(corr_r, method="circle") #high correlation btw citric.acid&fixed.acidity, density&fixed.acidity, pH&fixed acidity, total.sulfur.dioxide&free.sulfur.dioxide, alcohol&density
#white
corr_w <- cor(white[,])
corrplot(corr_w, method="circle") #high correlation of density&residual.sugar, alcohol&density
#combined
corr <- cor(combined[,])
corrplot(corr, method="circle") #color highly correlated to other predictors (possible interaction)

################################################
#Collapse Quality to be Binomial
###############################################
#are the categorical predictors numerical?
is.numeric(combined$quality)

#set variable as 0 and 1 (quality >=6 labeled as 1, quality <6 labeled as 0)
quality_bin <- as.integer(combined$quality > 5)
quality_bin_r <- as.integer(red$quality > 5)
quality_bin_w <- as.integer(white$quality > 5)

#create copy of combined df
combined_collapse <-data.frame(combined)
red_collapse <-data.frame(red)
white_collapse <-data.frame(white)

#add variable to respective models
combined_collapse$quality_bin <- quality_bin
red_collapse$quality_bin_r <- quality_bin_r
white_collapse$quality_bin_w <- quality_bin_w

#drop qualtiy from collapsed models
#combined
drops2 <- c("quality")
combined_collapse <- combined_collapse[ , !(names(combined_collapse) %in% drops2)]
red_collapse <- red_collapse[ , !(names(red_collapse) %in% drops2)]
white_collapse <- white_collapse[ , !(names(white_collapse) %in% drops2)]

#have R treat quality as categorical variable
combined_collapse$quality_bin <- factor(combined_collapse$quality_bin)
red_collapse$quality_bin_r <- factor(red_collapse$quality_bin_r)
white_collapse$quality_bin_w <- factor(white_collapse$quality_bin_w)

#reference classes for each dataset
contrasts(combined_collapse$quality_bin)
contrasts(red_collapse$quality_bin_r)
contrasts(white_collapse$quality_bin_w)

'''
#alternatively....
#have R treat quality as categorical variables
combined_collapse$quality <- factor(combined_collapse$quality)
red_collapse$quality <- factor(red_collapse$quality)
white_collapse$quality <- factor(white_collapse$quality)
#collapse Quality to be Good and Not Good
#combined
contrasts(combined_collapse$quality) #reference class is 3
levels(combined_collapse$quality)
#red
contrasts(red_collapse$quality) #reference class is 3
levels(red_collapse$quality)
#white
contrasts(white_collapse$quality) #reference class is 3
levels(white_collapse$quality)


#match with levels(combined_collapse$quality) order
#combined
new.quality <- c("Not Good", "Not Good","Not Good","Good","Good","Good","Good") 
#red
new.quality_r <- c("Not Good", "Not Good","Not Good","Good","Good","Good") 
#white
new.quality_w <- c("Not Good", "Not Good","Not Good","Good","Good","Good","Good") 

#add new binary variable to dataframe
#combined
combined_collapse$quality_binary <- factor(new.quality[combined_collapse$quality])
contrasts(combined_collapse$quality_binary) #Good is reference class
#red
red_collapse$quality_binary <- factor(new.quality_r[red_collapse$quality])
contrasts(red_collapse$quality_binary) #Good is reference class
#white
white_collapse$quality_binary <- factor(new.quality_w[white_collapse$quality])
contrasts(white_collapse$quality_binary) #Good is reference class

#relevel quality_binary reference class
#combined
combined_collapse$quality_binary <- relevel(combined_collapse$quality_binary, ref = "Not Good")
contrasts(combined_collapse$quality_binary) #Now Not Good is reference class
#red
red_collapse$quality_binary <- relevel(red_collapse$quality_binary, ref = "Not Good")
contrasts(red_collapse$quality_binary) #Now Not Good is reference class
#white
white_collapse$quality_binary <- relevel(white_collapse$quality_binary, ref = "Not Good")
contrasts(white_collapse$quality_binary) #Now Not Good is reference class

#create a df specifically removing original quality predictor
#combined
drops2 <- c("quality")
combined_collapse <- combined_collapse[ , !(names(combined_collapse) %in% drops2)]
#red
drops_r <- c("quality")
red_collapse <- red_collapse[ , !(names(red_collapse) %in% drops_r)]
#white
drops_w <- c("quality")
white_collapse <- white_collapse[ , !(names(white_collapse) %in% drops_w)]
'''

################################################
#Model (red)
###############################################
##set the random number generator so same results can be reproduced
set.seed(1)

#split data into train and test sets
sample_r <- sample.int(nrow(red_collapse), floor(.50*nrow(red_collapse)), replace = F)
train_r <- red_collapse[sample_r, ]
test_r <- red_collapse[-sample_r, ]

#fit intercept only and full model
lognull_r <- glm(quality_bin_r ~ 1, family = "binomial", data = train_r)
logfull_r <- glm(quality_bin_r ~ ., family = "binomial", data = train_r)

#starting and ending model have to be specified so the algorithm knows where to start and how far to go in the search procedure
#to carry out procedures, type
step(lognull_r, scope=list(lower=lognull_r, upper=logfull_r), direction="forward") #forward selection
#(5) alcohol, volatile.acidity, sulphates, total.sulfur.dioxide. free.sulfur.dioxide
###set.seed(111): (7) alcohol, volatile.acidity, total.sulfur.dioxide, sulphates, chlorides, residual.sugar, free.sulfur.dioxide
step(logfull_r, scope=list(lower=lognull_r, upper=logfull_r), direction="backward") #backward elimination
#(7) alcohol, volatile.acidity, sulphates, total.sulfur.dioxide. free.sulfur.dioxide, fixed.acidity, citric.acid 
###set.seed(111):(9) volatile.acidity, citric.acid, residual.sugar, chlorides, free.sulfur.dioxide, total.sulfur.dioxide, pH, sulphates, alcohol
step(lognull_r, scope=list(lower=lognull_r, upper=logfull_r), direction="both") #stepwise regression
#(5)alcohol, volatile.acidity, sulphates, total.sulfur.dioxide. free.sulfur.dioxide
###set.seed(111): (7) alcohol, volatile.acidity, total.sulfur.dioxide, sulphates, chlorides, residual.sugar, free.sulfur.dioxide

#forward and stepwise has 5 predictors
#backward and stepwise have 7 predictors (no pH)

#fit model with 7 predictors (using backward selection method)
#result_red7 <- glm(quality_bin_r ~ alcohol+volatile.acidity+sulphates+total.sulfur.dioxide+free.sulfur.dioxide+fixed.acidity+citric.acid, family = "binomial", data = train_r)
#summary(result_red7) #free.sulfur.dioxide insignificant. 
#vif(result_red7) #min vif = 6.88 (sulphates), max vif = 18.80 (total.sulfur.dioxide)

#model based on simulation choice
result_red_base <- glm(quality_bin_r ~ alcohol + volatile.acidity + total.sulfur.dioxide + sulphates + chlorides + free.sulfur.dioxide, family = "binomial", data = train_r)
summary(result_red_base)
vif(result_red_base)

#is the model useful
1-pchisq(result_red_base$null.deviance-result_red_base$deviance,6)
#pvalue = 0 (very small). Reject null. At least one predictor is nonzero. Yes, model is useful.

#red wine correlation plot
corr_r <- cor(train_r[,])
corrplot(corr_r, method="circle") #high correlation btw citric.acid&fixed.acidity, density&fixed.acidity, pH&fixed acidity, total.sulfur.dioxide&free.sulfur.dioxide, alcohol&density

#remove chlorides because insignificant in base model
result_red10 <- glm(quality_bin_r ~ alcohol + volatile.acidity + sulphates + total.sulfur.dioxide + free.sulfur.dioxide, family = "binomial", data = train_r)
summary(result_red10)
vif(result_red10)

#remove chlorides then remove total.sulfur.dioxide to minimize multicollinearity
#result_red9 <- glm(quality_bin_r ~ alcohol + volatile.acidity + sulphates + free.sulfur.dioxide, family = "binomial", data = train_r)
#summary(result_red9)
#vif(result_red9)

#remove free.sulfur.dioxide because insignificant (removes chlorides, total.sulfur.dioxide, and free.sulfur.dioxide from base model)
#result_red11 <- glm(quality_bin_r ~ alcohol + volatile.acidity + sulphates, family = "binomial", data = train_r)
#summary(result_red11)
#vif(result_red11)

############################## FINAL RED MODEL #########################
#instead of remove total.sulfur.dioxide to minimize multicollinearity, swap for free.sulfur.dioxide
result_red_final <- glm(quality_bin_r ~ alcohol + volatile.acidity + sulphates + total.sulfur.dioxide, family = "binomial", data = train_r)
summary(result_red_final)
vif(result_red_final)

#is the model useful
1-pchisq(result_red_final$null.deviance-result_red_final$deviance,4)
#pvalue = 0 (very small). Reject null. At least one predictor is nonzero. Yes, model is useful.

#can we remove coefficients from base model?
1-pchisq(result_red_final$deviance-result_red_base$deviance,2)
#pvalue is greater than 0.05. Fail to reject the null. We can drop chlorides and free.sulfur.dioxide from base model.
########################################################################

#fit model based on set.seed(111)
#result_red <- glm(quality_bin_r ~ alcohol + volatile.acidity + total.sulfur.dioxide + sulphates + chlorides + residual.sugar + free.sulfur.dioxide, family = "binomial", data = train_r)
#summary(result_red)
#vif(result_red)

#is the model useful
#1-pchisq(result_red$null.deviance-result_red$deviance,7)
#small pvalue. Reject null. Yes, useful model. 

#fit model without free.sulfur.dioxide
#result_red6 <- glm(quality_bin_r ~ alcohol+volatile.acidity+sulphates+total.sulfur.dioxide+fixed.acidity+citric.acid, family = "binomial", data = train_r)
#summary stats
#summary(result_red6)
#multicollinearity
#vif(result_red6) #min vif = 6.82 (total.sulfur.dioxide), max vif = 18.48 (citric.acid)

#fit model based on forward selection. 
#result_red5 <- glm(quality_bin_r ~ alcohol+volatile.acidity+sulphates+total.sulfur.dioxide+free.sulfur.dioxide , family = "binomial", data = train_r)
#summary(result_red5)
#multicollinearity
#vif(result_red5) #min vif = 6.52 (sulphates) and max vif = 13.83 (total.sulfur.dioxide)

#null: fixed.acidity and citric.acid are 0
#alt: at least one in null is nonzero
#1-pchisq(result_red5$deviance-result_red7$deviance,2)
#p-value is less than 0.05. Reject null. Model with 7 predictors better than model with 5 predicotrs. 

#remove residual sugar
#result_red3 <- glm(quality_bin_r ~ alcohol + volatile.acidity + total.sulfur.dioxide + sulphates + chlorides + free.sulfur.dioxide, family = "binomial", data = train_r)
#summary(result_red3)
#multicollinearity
#vif(result_red3)

#remove residual sugar and chlorides
#result_red4 <- glm(quality_bin_r ~ alcohol + volatile.acidity + total.sulfur.dioxide + sulphates + free.sulfur.dioxide, family = "binomial", data = train_r)
#summary(result_red4)
#multicollinearity
#vif(result_red4)

#remove residual sugar and total.sulfur.dioxide
#result_red5 <- glm(quality_bin_r ~ alcohol + volatile.acidity + sulphates + free.sulfur.dioxide + chlorides, family = "binomial", data = train_r)
#summary(result_red5)
#multicollinearity
#vif(result_red5)

#remove residual sugar, chlorides, and free.sulfur.dioxde
#result_red13 <- glm(quality_bin_r ~ alcohol + volatile.acidity + sulphates + chlorides, family = "binomial", data = train_r)
#summary(result_red13)
#vif(result_red13)

#remove residual sugar, total.sulfur.dioxide, and chlorides
#result_red2 <- glm(quality_bin_r ~ alcohol + volatile.acidity + sulphates + free.sulfur.dioxide, family = "binomial", data = train_r)
#summary(result_red2)
#multicollinearity
#vif(result_red2)

#remove residual sugar, total.sulfur.dioxide, chlorides and free sulphur
#result_red1 <- glm(quality_bin_r ~ alcohol + volatile.acidity + sulphates, family = "binomial", data = train_r)
#summary(result_red1)
#multicollinearity
#vif(result_red1)

#null: free.sulfur dioxide and residual sugar are 0
#alt: at least one is nonzero
#1-pchisq(result_red2$deviance-result_red$deviance,3)
#p-value is less than 0.05 therefore reject the null. 
#therefore, reject the null. 
#our data supports the claim that at least one bp is nonzero and significant in determining heart disease

################################################
#Model Eval (red)
###############################################
#breakdown of observations for test set (number of good and bad wine from test set for red)
as.data.frame(table(test_r$quality_bin_r))

##predicted survival rate for testing data based on training data (calculates predictive probabilities of survival of test data set)
preds_r<-predict(result_red_final, newdata = test_r, type="response") #specify type = "response" for probability

##produce the numbers associated with classification table
rates_r<-prediction(preds_r, test_r$quality_bin_r)

##store the true positive and false postive rates
roc_result_r<-performance(rates_r,measure="tpr", x.measure="fpr") #sets up ROC curve (TPR- True Positive Rate, FPR- False Positive Rate)

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result_r, main="ROC Curve for Wine Quality (Red-Binary)") #produces ROC curve
lines(x = c(0,1), y = c(0,1), col="red") 

##compute the AUC
auc_r <- performance(rates_r, measure = "auc")
auc_r@y.values #0.8394181

##confusion matrix. Actual values in the rows, predicted classification in cols
con_r_0.5 <- table(test_r$quality_bin_r, preds_r>0.5) 
con_r_0.4 <- table(test_r$quality_bin_r, preds_r>0.4) 

#confusion matrix values (threshold 0.5)
(TN_r <- con_r_0.5[1][1])
(FN_r <- con_r_0.5[2][1])
(FP_r <- con_r_0.5[3][1])
(TP_r <- con_r_0.5[4][1])

#confusion matrix values (threshold 0.4)
(TN_r4 <- con_r_0.4[1][1])
(FN_r4 <- con_r_0.4[2][1])
(FP_r4 <- con_r_0.4[3][1])
(TP_r4 <- con_r_0.4[4][1])

#con_r_0.5
#con_r_0.7 <- table(test_r$quality_bin_r, preds_r>0.7) 
#con_r_0.7

#confusion matrix stats
#(error_rate_r <-  (FP_r+FN_r)/nrow(test_r))
#(FP_rate <-  FP_r/(TN_r+FP_r)) 
#(FN_rate <- FN_r/(FN_r+TP_r)) 
#(sen_r <- TP_r/(FN_r+TP_r)) 
#(spec_r <- TN_r/(TN_r+FP_r)) 

#confusion matrix stats
#(error_rate_r <-  (FP_r+FN_r)/nrow(test_r))
#(FP_rate <-  FP_r/(TN_r+FP_r)) #
#(FN_rate <- FN_r/(FN_r+TP_r)) #
#(sen_r <- TP_r/(FN_r+TP_r)) #
#(spec_r <- TN_r/(TN_r+FP_r)) #

###confusion matrix notes
##                  | Model classifies as "No" (y^=0)  | Model classifies as "Yes" (y^=1)
## True "No" (y=0)  |  True Neg (TN)                   |  False Pos (FP)
## True "Yes" (y=1) |  False Neg (FN)                  |  True Pos (TP)

################################################
#Model (white)
###############################################
##set the random number generator so same results can be reproduced
set.seed(1)

#split data into train and test
sample_w <- sample.int(nrow(white_collapse), floor(.50*nrow(white_collapse)), replace = F)
train_w <- white_collapse[sample_w, ]
test_w <- white_collapse[-sample_w, ] #- (minus sign) for obs not in sample

#fit intercept only and full model for white wine
lognull_w <- glm(quality_bin_w ~ 1, family = "binomial", data = train_w)
logfull_w <- glm(quality_bin_w ~ ., family = "binomial", data = train_w)

#model selection (forward, backward, and stepwise)
step(lognull_w, scope=list(lower=lognull_w, upper=logfull_w), direction="forward") #forward selection
#(8) alcohol, volatile.acidity, residual.sugar, fixed.acidity, sulphates, free.sulfur.dioxide, density, pH
step(logfull_w, scope=list(lower=lognull_w, upper=logfull_w), direction="backward") #backward elimination
#(7) alcohol, volatile.acidity, residual.sugar, sulphates, free.sulfur.dioxide, density, pH
step(lognull_w, scope=list(lower=lognull_w, upper=logfull_w), direction="both") #stepwise regression
#(7) alcohol, volatile.acidity, residual.sugar, sulphates, free.sulfur.dioxide, density, pH

#forward has 8 predictors
#backward and stepwise have 7 predictors (no fixed.acidity)

#fit log model based on repeated sample pick (base model)
result_white_base <- glm(quality_bin_w ~ alcohol + volatile.acidity + residual.sugar + sulphates +  free.sulfur.dioxide + density + pH, family = "binomial", data = train_w)
summary(result_white_base)
vif(result_white_base) 

#correlation plot for white wine dataset (train_w)
corr_w <- cor(train_w[,])
corrplot(corr_w, method="circle") #high correlation of density&residual.sugar, alcohol&density

############################# FINAL MODEL FOR WHITE ################################### 
#remove density
result_white_final <- glm(quality_bin_w ~ alcohol + volatile.acidity + residual.sugar + sulphates +  free.sulfur.dioxide + pH, family = "binomial", data = train_w)
summary(result_white_final)
vif(result_white_final) 

#is the model useful
1-pchisq(result_white_final$null.deviance - result_white_final$deviance,6)
#pvalue = 0 (very small). Reject null. At least one predictor is nonzero. Yes, model is useful.

#can we remove coefficients from base model?
1-pchisq(result_white_final$deviance - result_white_base$deviance,1)
#pvalue less 0.05. Fail to reject null. Delta G-squared says we can't remove density
#######################################################################################

#remove alcohol
#result_white8 <- glm(quality_bin_w ~ residual.sugar + volatile.acidity + sulphates +  free.sulfur.dioxide + density + pH, family = "binomial", data = train_w)
#summary(result_white8)
#vif(result_white8) 

#remove alcohol and free.sulfur.dioxide
#result_white1 <- glm(quality_bin_w ~ residual.sugar + volatile.acidity + sulphates + density + pH, family = "binomial", data = train_w)
#summary(result_white1)
#vif(result_white1) 

#remove sugar
#result_white6 <- glm(quality_bin_w ~ alcohol + volatile.acidity + sulphates +  free.sulfur.dioxide + density + pH, family = "binomial", data = train_w)
#summary(result_white6)
#vif(result_white6) 

#remove sugar and pH
#result_white7 <- glm(quality_bin_w ~ alcohol + volatile.acidity + sulphates +  free.sulfur.dioxide + density, family = "binomial", data = train_w)
#summary(result_white7)
#vif(result_white7) 

#remove sugar, pH, and density
#result_white9 <- glm(quality_bin_w ~ alcohol + volatile.acidity + sulphates + free.sulfur.dioxide, family = "binomial", data = train_w)
#summary(result_white9)
#vif(result_white9) 

################################################################
#remove sugar, pH, and alcohol
#result_white10 <- glm(quality_bin_w ~ volatile.acidity + sulphates +  free.sulfur.dioxide + density, family = "binomial", data = train_w)
#summary(result_white10)
#vif(result_white10) 

#is the model useful
#1-pchisq(result_white10$null.deviance-result_white_base$deviance,4)
#pvalue = 0 (very small). Reject null. At least one predictor is nonzero. Yes, model is useful.

#can we remove coefficients from base model?
#1-pchisq(result_white10$deviance-result_white_base$deviance,3)
#pvalue less 0.05. Fail to reject null. Delta G-squared says we can't remove 3 predictors
################################################################

#remove alcohol and residual sugar
#result_white2 <- glm(quality_bin_w ~ volatile.acidity + sulphates +  free.sulfur.dioxide + density + pH, family = "binomial", data = train_w)
#summary(result_white2)
#vif(result_white2) 

#null: alcohol and residual sugar are 0
#alt: at least one is nonzero
#1-pchisq(result_white2$deviance-result_white$deviance,2)
#p-value is less than 0.05 therefore reject the null. Cannot remove alcohol and residual sugar. 

#remove density and alcohol
#result_white4 <- glm(quality_bin_w ~ volatile.acidity + residual.sugar + sulphates +  free.sulfur.dioxide + pH, family = "binomial", data = train_w)
#summary(result_white4)
#vif(result_white4) 

#1-pchisq(result_white4$deviance-result_white$deviance,2)
#p-value is less than 0.05 therefore reject the null. Cannot remove density and alcohol

#remove density, alcohol, and free.sulfur.dioxide
#result_white5 <- glm(quality_bin_w ~ volatile.acidity + residual.sugar + sulphates + pH, family = "binomial", data = train_w)
#summary(result_white5)
#vif(result_white5) 

#1-pchisq(result_white5$deviance-result_white$deviance,3)
#p-value is less than 0.05 therefore reject the null. Cannot remove density, alcohol, and free.sulfur.dioxide

################################################
#Model Eval (white)
###############################################
#breakdown of observations for test set 
as.data.frame(table(test_w$quality_bin_w))
#number of observations in test set
nrow(test_w)

##predicted survival rate for testing data based on training data (calculates predictive probabilities of survival of test data set)
preds_w<-predict(result_white_final, newdata = test_w, type="response") #specify type = "response" for probability

##produce the numbers associated with classification table
rates_w<-prediction(preds_w, test_w$quality_bin_w)

##store the true positive and false postive rates
roc_result_w<-performance(rates_w,measure="tpr", x.measure="fpr") #sets up ROC curve (TPR- True Positive Rate, FPR- False Positive Rate)

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result_w, main="ROC Curve for Wine Quality (White-Binary)") #produces ROC curve
lines(x = c(0,1), y = c(0,1), col="red") #produces diagonal line to compare ROC curve with

##compute the AUC
auc_w <- performance(rates_w, measure = "auc")
auc_w@y.values 

##confusion matrix. Actual values in the rows, predicted classification in cols
con_w_0.5 <- table(test_w$quality_bin_w, preds_w>0.5)
con_w_0.4 <- table(test_w$quality_bin_w, preds_w>0.4) 

#confusion matrix values (threshold 0.5)
(TN_w <- con_w_0.5[1][1])
(FN_w <- con_w_0.5[2][1])
(FP_w <- con_w_0.5[3][1])
(TP_w <- con_w_0.5[4][1])

#confusion matrix values (threshold 0.4)
(TN_w4 <- con_w_0.4[1][1])
(FN_w4 <- con_w_0.4[2][1])
(FP_w4 <- con_w_0.4[3][1])
(TP_w4 <- con_w_0.4[4][1])

#confusion matrix stats
#(error_rate_w <-  (FP_w+FN_w)/nrow(test_w)) 
#(FP_rate_w <-  FP_w/(TN_w+FP_w))
#(FN_rate_w <- FN_w/(FN_w+TP_w))
#(sen_w <- TP_w/(FN_w+TP_w))
#(spec_w <- TN_w/(TN_w+FP_w))

################################################
#Model (combined). DOES NOT HAVE GOOD RESULTS
###############################################
#set seed
set.seed(1)

##choose the observations to be in the training (split data in half)
#generate vector half the length of the data frame. Floor in case odd number of obs (rounds down). Replace = F bc sample without replacement. 
sample <- sample.int(nrow(combined_collapse), floor(0.5*nrow(combined_collapse)), replace = F)
train <- combined_collapse[sample, ]
test <- combined_collapse[-sample, ] #- (minus sign) for obs not in sample

#fit intercept only and full model
lognull <- glm(quality_bin ~ 1, family = "binomial", data = train)
logfull <- glm(quality_bin ~ ., family = "binomial", data = train)

#starting and ending model have to be specified so the algorithm knows where to start and how far to go in the search procedure
#to carry out procedures, type
step(lognull, scope=list(lower=lognull, upper=logfull), direction="forward") #forward selection
#(10) alcohol, volatile.acidity, sulphates, residual.sugar, color, density, total.sulfur.dioxide, free.sulfur.dioxide, pH, fixed.acidity
step(logfull, scope=list(lower=lognull, upper=logfull), direction="backward") #backward elimination
#(10) fixed.acidity, volatile.acidity, residual.sugar, free.sulfur.dioxide, total.sulfur.dioxide, density, pH, sulphates, alcohol, color
step(lognull, scope=list(lower=lognull, upper=logfull), direction="both") #stepwise regression
#(10) alcohol, volatile.acidity, sulphates, residual.sugar, color, density, total.sulfur.dioxide, free.sulfur.dioxide, pH, fixed.acidity

#fitted logistic regression model (using forward selection)
for_result <- glm(quality_bin ~ alcohol + volatile.acidity + sulphates + residual.sugar + color + density + free.sulfur.dioxide + total.sulfur.dioxide + pH + fixed.acidity, family = "binomial", data = train)
summary(for_result)
vif(for_result)

#is the model useful?
1-pchisq(for_result$null.deviance-for_result$deviance,11) #11 predictors
#small pvalue. Therefore reject the null (all coefficients are 0). At least one of the predictors is nonzero. Correspondings to individual pvalues of z tests. 

#remove fixed acidity
for_result1 <- glm(quality_bin ~ alcohol + volatile.acidity + sulphates + residual.sugar + color + density + free.sulfur.dioxide + total.sulfur.dioxide + pH, family = "binomial", data = train)
summary(for_result1)
vif(for_result1)

#no fixed acidity or pH
for_result2 <- glm(quality_bin ~ alcohol + volatile.acidity + sulphates + residual.sugar + color + density + free.sulfur.dioxide + total.sulfur.dioxide, family = "binomial", data = train)
summary(for_result2)
vif(for_result2)

##predicted survival rate for testing data based on training data (calculates predictive probabilities of survival of test data set)
preds<-predict(for_result3, newdata = test, type="response") #specify type = "response" for probability

##produce the numbers associated with classification table
rates<-prediction(preds, test$quality_bin)

##store the true positive and false postive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr") #sets up ROC curve (TPR- True Positive Rate, FPR- False Positive Rate)

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for Wine Quality (Combined-Binary)") #produces ROC curve
lines(x = c(0,1), y = c(0,1), col="red") #produces diagonal line to compare ROC curve with

##compute the AUC
auc <- performance(rates, measure = "auc")
auc@y.values 

##confusion matrix. Actual values in the rows, predicted classification in cols
con_c_0.5 <-table(test$quality_bin, preds>0.5) #test$Survied rows of true values, preds > 0.5 for column values

#drop density
#for_reduce <- glm(quality_binary ~ alcohol + volatile.acidity + sulphates + residual.sugar + color + free.sulfur.dioxide + total.sulfur.dioxide + pH + fixed.acidity + citric.acid, family = "binomial", data = combined_collapse)
#summary(for_reduce)
#vif(for_reduce)
#1-pchisq(for_reduce$deviance-for_result$deviance,1)
#reject the null. Cannot drop density

#drop density, fixed.acidity, pH
#for_reduce <- glm(quality_binary ~ alcohol + volatile.acidity + sulphates + residual.sugar + color + free.sulfur.dioxide + total.sulfur.dioxide + citric.acid, family = "binomial", data = combined_collapse)
#summary(for_reduce)
#vif(for_reduce)
#1-pchisq(for_reduce$deviance-for_result$deviance,1)
#reject the null. At least one of the predictors (density, fixed.acidity, pH) is nonzero

################################################
#Evaluate Model (combined model. Results are not good)
###############################################
#set seed
set.seed(1)

#split data into train and test sets
sample <- sample.int(nrow(combined_collapse), floor(0.5*nrow(combined_collapse)), replace = F)
train <- combined_collapse[sample, ]
test <- combined_collapse[-sample, ] #- (minus sign) for obs not in sample

##use training data to fit logistic regression model with fare and gender as predictors
for_result_train <- glm(quality_bin ~ alcohol + volatile.acidity + sulphates + residual.sugar + color + density + free.sulfur.dioxide + total.sulfur.dioxide + pH + fixed.acidity + citric.acid, family = "binomial", data = train)

##predicted survival rate for testing data based on training data (calculates predictive probabilities of survival of test data set)
preds<-predict(for_result_train, newdata = test, type="response") #specify type = "response" for probability

##produce the numbers associated with classification table
rates<-prediction(preds, test$quality_bin)

##store the true positive and false postive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr") #sets up ROC curve (TPR- True Positive Rate, FPR- False Positive Rate)

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for Wine Quality (Combined-Binary)") #produces ROC curve
lines(x = c(0,1), y = c(0,1), col="red") #produces diagonal line to compare ROC curve with

##compute the AUC
auc <- performance(rates, measure = "auc")
auc@y.values

##confusion matrix. Actual values in the rows, predicted classification in cols
con_c_0.5 <-table(test$quality_bin, preds>0.5) #test$Survied rows of true values, preds > 0.5 for column values

con_c_0.7 <- table(test$quality_bin, preds>0.7) #raising threshold makes it harder for individual to be classified as survived

#confusion matrix values
TN_c <- con_c_0.5[1][1]
FN_c <- con_c_0.5[2][1]
FP_c <- con_c_0.5[3][1]
TP_c <- con_c_0.5[4][1]

#overall error rate for white
(error_rate_c <-  (FP_c+FN_c)/nrow(test)) 
(FP_rate_c <-  FP_c/(TN_c+FP_c)) 
(FN_rate_c <- FN_c/(FN_c+TP_c)) 
(sen_c <- TP_c/(FN_c+TP_c)) 
(spec_c <- TN_c/(TN_c+FP_c)) 

#confusion matrix values
TN_c_2 <- con_c_0.7[1][1]
FN_c_2 <- con_c_0.7[2][1]
FP_c_2 <- con_c_0.7[3][1]
TP_c_2 <- con_c_0.7[4][1]

#overall error rate for white
#(error_rate_c_2 <-  (FP_c_2+FN_c_2)/nrow(test)) 
#(FP_rate_c_2 <-  FP_c_2/(TN_c_2+FP_c_2)) 
#(FN_rate_c_2 <- FN_c_2/(FN_c_2+TP_c_2))
#(sen_c_2 <- TP_c_2/(FN_c_2+TP_c_2))
#(spec_c_2 <- TN_c_2/(TN_c_2+FP_c_2)) 

#######################################################
#Outliers (red)
#######################################################
#are there any obs that are outlying in the response variable?
#n_r <- length(red_collapse$quality_binary) #number of rows in dataset (specifically col Fertility)
#p_r <- 10 #number of parameters: 3 predictors and 1 intercept

##critical value using Bonferroni procedure
#qt(1-0.05/(2*n_r), n_r-p_r-1) #4.176071
#any data pt larger than 4.176071 will be an outlier

##residuals
#res<-result_red$residuals 

##studentized residuals
#student.res<-rstandard(result_red) 

##externally studentized residuals
#ext.student.res<-rstudent(result_red) 

#few ways to outlier
#numerical check: sort externally studentized residuals
#sort(ext.student.res) #none larger than 4.176071

#visual check: create plot of ext. studentized residuals
#plot(ext.student.res,main="Externally Studentized Residuals", ylim=c(-10,10))
#abline(h=qt(1-0.05/(2*n_r), n_r-p_r-1), col="red") #critical value
#abline(h=-qt(1-0.05/(2*n_r), n_r-p_r-1), col="red") #critical value
#no residuals outside of critical values

#line. If abs val > crit value
#ext.student.res[abs(ext.student.res)>qt(1-0.05/(2*n_r), n_r-p_r-1)] #none
