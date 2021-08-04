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
library(caret)
library(corrplot)

#data
red <- read.csv("wineQualityReds.csv", header = TRUE, sep = ',')
white <- read.csv("wineQualityWhites.csv", header = TRUE, sep = ',')

#combined dataset for red and white wine. Color = 1 for red wine, 0 for white wine
combined <- read.csv("combined_wine.csv", header = TRUE, sep = ',')
#nrow(filter(combined, color == 1))

#drop X from red
#red
drops_Xr <- c("X")
red <- red[ , !(names(red) %in% drops_Xr)]
#white
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

#histogram matrix
multiplot(plotlist = myplots, cols = 4)

#correlation plot
#red
corr_r <- cor(red[,])
corrplot(corr_r, method="circle") #high correlation btw citric.acid&fixed.acidity, density&fixed.acidity, pH&fixed acidity, total.sulfur.dioxide&free.sulfur.dioxide, alcohol&density
#white
corr_w <- cor(white[,])
corrplot(corr_w, method="circle") #high correlation of density&residual.sugar, alcohol&density

################################################
#Collapse Quality to be Binomial
###############################################
#are the categorical predictors numerical?
is.numeric(combined$quality)

#set variable as 0 and 1
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

combined_collapse$quality_bin <- factor(combined_collapse$quality_bin)
red_collapse$quality_bin_r <- factor(red_collapse$quality_bin_r)
white_collapse$quality_bin_w <- factor(white_collapse$quality_bin_w)

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
#fit intercept only and full model
lognull_r <- glm(quality_bin_r ~ 1, family = "binomial", data = red_collapse)
logfull_r <- glm(quality_bin_r ~ ., family = "binomial", data = red_collapse)

#starting and ending model have to be specified so the algorithm knows where to start and how far to go in the search procedure
#to carry out procedures, type
step(lognull_r, scope=list(lower=lognull_r, upper=logfull_r), direction="forward") #forward selection
#alcohol, volatile.acidity, total.sulfur,dioxide, sulphates, chlorides, free.sulfur.dioxide, pH, citric.acid, fixed.acidity
step(logfull_r, scope=list(lower=lognull_r, upper=logfull_r), direction="backward") #backward elimination
#alcohol, volatile.acidity, total.sulfur.dioxide, sulphates, chlorides, free.sulfur.dioxide, citric.acid, fixed.acidity
step(lognull_r, scope=list(lower=lognull_r, upper=logfull_r), direction="both") #stepwise regression
#alcohol, volatile.acidity, total.sulfur.dioxide, sulphates, chlorides, free.sulfur.dioxide, citric.acid, fixed.acidity

#forward has 10 predictors
#backward and stepwise have 9 predictors (no pH)

#full_red <- glm(quality_binary ~ ., family = "binomial", data = red_collapse)
#summary(full_red)
#red_11 <- glm(quality_binary ~ fixed.acidity+volatile.acidity+citric.acid, family = "binomial", data = red_collapse)

#fit model with 9 predictors (using backward selection method)
result_red <- glm(quality_bin_r ~ alcohol + volatile.acidity + total.sulfur.dioxide + sulphates + chlorides +  free.sulfur.dioxide + fixed.acidity + citric.acid, family = "binomial", data = red_collapse)
#summary stats
summary(result_red)
#multicollinearity
vif(result_red)

#######################################################
#Outliers (red)
#######################################################
#are there any obs that are outlying in the response variable?
n_r <- length(red_collapse$quality_binary) #number of rows in dataset (specifically col Fertility)
p_r <- 10 #number of parameters: 3 predictors and 1 intercept

##critical value using Bonferroni procedure
qt(1-0.05/(2*n_r), n_r-p_r-1) #4.176071
#any data pt larger than 4.176071 will be an outlier

##residuals
res<-result_red$residuals 

##studentized residuals
student.res<-rstandard(result_red) 

##externally studentized residuals
ext.student.res<-rstudent(result_red) 

#few ways to outlier
#numerical check: sort externally studentized residuals
sort(ext.student.res) #none larger than 4.176071

#visual check: create plot of ext. studentized residuals
plot(ext.student.res,main="Externally Studentized Residuals", ylim=c(-10,10))
abline(h=qt(1-0.05/(2*n_r), n_r-p_r-1), col="red") #critical value
abline(h=-qt(1-0.05/(2*n_r), n_r-p_r-1), col="red") #critical value
#no residuals outside of critical values

#line. If abs val > crit value
ext.student.res[abs(ext.student.res)>qt(1-0.05/(2*n_r), n_r-p_r-1)] #none

################################################
#Model Eval (red)
###############################################

##set the random number generator so same results can be reproduced
set.seed(111)

##choose the observations to be in the training (split data in half)
#generate vector half the length of the data frame. Floor in case odd number of obs (rounds down). Replace = F bc sample without replacement. 
sample_r <- sample.int(nrow(red_collapse), floor(.50*nrow(red_collapse)), replace = F)
train_r <- red_collapse[sample_r, ]
test_r <- red_collapse[-sample_r, ] #- (minus sign) for obs not in sample

#preprocess data
#summary(train_r)
#preproc_r_train <- preProcess(train_r[,c("pH","alcohol")], method = c("center","scale"))

#perform normalization
#norm_r_train <- predict(preproc_r, train_r[,c("pH","alcohol")])
#summary(train_r)

##use training data to fit logistic regression model with fare and gender as predictors
result_red_train <- glm(quality_bin_r ~ alcohol + volatile.acidity + total.sulfur.dioxide + sulphates + chlorides +  free.sulfur.dioxide + fixed.acidity + citric.acid, family = "binomial", data = train_r)

##predicted survival rate for testing data based on training data (calculates predictive probabilities of survival of test data set)
preds_r<-predict(result_red_train, newdata = test_r, type="response") #specify type = "response" for probability

##produce the numbers associated with classification table
rates_r<-prediction(preds_r, test_r$quality_bin_r)

##store the true positive and false postive rates
roc_result_r<-performance(rates_r,measure="tpr", x.measure="fpr") #sets up ROC curve (TPR- True Positive Rate, FPR- False Positive Rate)

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result_r, main="ROC Curve for Wine Quality (Red-Binary)") #produces ROC curve
lines(x = c(0,1), y = c(0,1), col="red") #produces diagonal line to compare ROC curve with

##compute the AUC
auc_r <- performance(rates_r, measure = "auc")
auc_r@y.values #0.8271047

##confusion matrix. Actual values in the rows, predicted classification in cols
con_r_0.5 <- table(test_r$quality_bin_r, preds_r>0.5) #test$Survied rows of true values, preds > 0.5 for column values

con_r_0.7 <- table(test_r$quality_bin_r, preds_r>0.7) #raising threshold makes it harder for individual to be classified as survived

###confusion matrix notes
##                  | Model classifies as "No" (y^=0)  | Model classifies as "Yes" (y^=1)
## True "No" (y=0)  |  True Neg (TN)                   |  False Pos (FP)
## True "Yes" (y=1) |  False Neg (FN)                  |  True Pos (TP)

#confusion matrix values
TN_r <- con_r_0.5[1][1]
FN_r <- con_r_0.5[2][1]
FP_r <- con_r_0.5[3][1]
TP_r <- con_r_0.5[4][1]

#confusion matrix stats
error_rate_r <-  (FP_r+FN_r)/nrow(test_r) #0.23875
FP_rate <-  FP_r/(TN_r+FP_r) #0.2744063
FN_rate <- FN_r/(FN_r+TP_r) #0.2066508
sen_r <- TP_r/(FN_r+TP_r) #0.7933492
spec_r <- TN_r/(TN_r+FP_r) #0.7255937

################################################
#Model (white)
###############################################
#fit intercept only and full model for white wine
lognull_w <- glm(quality_bin_w ~ 1, family = "binomial", data = white_collapse)
logfull_w <- glm(quality_bin_w ~ ., family = "binomial", data = white_collapse)

#starting and ending model have to be specified so the algorithm knows where to start and how far to go in the search procedure
#to carry out procedures, type
step(lognull_w, scope=list(lower=lognull_w, upper=logfull_w), direction="forward") #forward selection
#alcohol, volatile.acidity, residual.sugar, fixed.acidity, sulphates, free.sulfur.dioxide, density, pH
step(logfull_w, scope=list(lower=lognull_w, upper=logfull_w), direction="backward") #backward elimination
#alcohol, volatile.acidity, residual.sugar, sulphates, free.sulfur.dioxide, density, pH
step(lognull_w, scope=list(lower=lognull_w, upper=logfull_w), direction="both") #stepwise regression
#alcohol, volatile.acidity, residual.sugar, sulphates, free.sulfur.dioxide, density, pH

#forward has 8 predictors
#backward and stepwise have 7 predictors (no fixed.acidity)

#fit log model with 9 predictors (backwards selection choice)
result_white <- glm(quality_bin_w ~ alcohol + volatile.acidity + residual.sugar + sulphates +  free.sulfur.dioxide + density + pH, family = "binomial", data = white_collapse)
#summary stats
summary(result_white)
#multicollinearity
vif(result_white) #density, residual.sugar

################################################
#Model Eval (white)
###############################################

##set the random number generator so same results can be reproduced
set.seed(111)

##choose the observations to be in the training (split data in half)
#generate vector half the length of the data frame. Floor in case odd number of obs (rounds down). Replace = F bc sample without replacement. 
sample_w <- sample.int(nrow(white_collapse), floor(.50*nrow(white_collapse)), replace = F)
train_w <- white_collapse[sample_w, ]
test_w <- white_collapse[-sample_w, ] #- (minus sign) for obs not in sample

##use training data to fit logistic regression model with fare and gender as predictors
result_white_train <- glm(quality_bin_w ~ alcohol + volatile.acidity + residual.sugar + sulphates +  free.sulfur.dioxide + density + pH, family = "binomial", data = train_w)

##predicted survival rate for testing data based on training data (calculates predictive probabilities of survival of test data set)
preds_w<-predict(result_white_train, newdata = test_w, type="response") #specify type = "response" for probability

##produce the numbers associated with classification table
rates_w<-prediction(preds_w, test_w$quality_bin_w)

##store the true positive and false postive rates
roc_result_w<-performance(rates_w,measure="tpr", x.measure="fpr") #sets up ROC curve (TPR- True Positive Rate, FPR- False Positive Rate)

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result_w, main="ROC Curve for Wine Quality (White-Binary)") #produces ROC curve
lines(x = c(0,1), y = c(0,1), col="red") #produces diagonal line to compare ROC curve with

##compute the AUC
auc_w <- performance(rates_w, measure = "auc")
auc_w@y.values #0.7958

##confusion matrix. Actual values in the rows, predicted classification in cols
con_w_0.5 <- table(test_w$quality_bin_w, preds_w>0.5) #test$Survied rows of true values, preds > 0.5 for column values

con_w_0.7 <- table(test_w$quality_bin_w, preds_w>0.7) #raising threshold makes it harder for individual to be classified as survived

#confusion matrix values (threshold 0.5)
TN_w <- con_w_0.5[1][1]
FN_w <- con_w_0.5[2][1]
FP_w <- con_w_0.5[3][1]
TP_w <- con_w_0.5[4][1]

#confusion matrix stats
error_rate_w <-  (FP_w+FN_w)/nrow(test_w) #0.2441813
FP_rate_w <-  FP_w/(TN_w+FP_w) #0.5082592
FN_rate_w <- FN_w/(FN_w+TP_w) #0.1191336
sen_w <- TP_w/(FN_w+TP_w) #0.8808664
spec_w <- TN_w/(TN_w+FP_w) #0.4917408

#confusion matrix values (threshold 0.7)
TN_w_2 <- con_w_0.7[1][1]
FN_w_2 <- con_w_0.7[2][1]
FP_w_2 <- con_w_0.7[3][1]
TP_w_2 <- con_w_0.7[4][1]

#confusion matrix stats
(error_rate_w_2 <-  (FP_w_2+FN_w_2)/nrow(test_w)) #0.3033891
(FP_rate_w_2 <-  FP_w_2/(TN_w_2+FP_w_2)) #0.2465057
(FN_rate_w_2 <- FN_w_2/(FN_w_2+TP_w_2)) #0.3303249
(sen_w_2 <- TP_w_2/(FN_w_2+TP_w_2)) #0.6696751
(spec_w_2 <- TN_w_2/(TN_w_2+FP_w_2)) # 0.7534943

################################################
#Model (combined)
###############################################
#fit intercept only and full model
lognull <- glm(quality_bin ~ 1, family = "binomial", data = combined_collapse)
logfull <- glm(quality_bin ~ ., family = "binomial", data = combined_collapse)

#starting and ending model have to be specified so the algorithm knows where to start and how far to go in the search procedure
#to carry out procedures, type
step(lognull, scope=list(lower=lognull, upper=logfull), direction="forward") #forward selection
step(logfull, scope=list(lower=lognull, upper=logfull), direction="backward") #backward elimination
step(lognull, scope=list(lower=lognull, upper=logfull), direction="both") #stepwise regression

#fitted logistic regression model (using forward selection)
for_result <- glm(quality_bin ~ alcohol + volatile.acidity + sulphates + residual.sugar + color + density + free.sulfur.dioxide + total.sulfur.dioxide + pH + fixed.acidity + citric.acid, family = "binomial", data = combined_collapse)
summary(for_result)
vif(for_result)

#is the model useful?
1-pchisq(for_result$null.deviance-for_result$deviance,11) #11 predictors
#small pvalue. Therefore reject the null (all coefficients are 0). At least one of the predictors is nonzero. Correspondings to individual pvalues of z tests. 

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
##set the random number generator so same results can be reproduced
set.seed(111)

##choose the observations to be in the training (split data in half)
#generate vector half the length of the data frame. Floor in case odd number of obs (rounds down). Replace = F bc sample without replacement. 
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
auc@y.values #0.8045277

##confusion matrix. Actual values in the rows, predicted classification in cols
con_c_0.5 <-table(test$quality_bin, preds>0.5) #test$Survied rows of true values, preds > 0.5 for column values

con_c_0.7 <- table(test$quality_bin, preds>0.7) #raising threshold makes it harder for individual to be classified as survived

#confusion matrix values
TN_c <- con_c_0.5[1][1]
FN_c <- con_c_0.5[2][1]
FP_c <- con_c_0.5[3][1]
TP_c <- con_c_0.5[4][1]

#overall error rate for white
(error_rate_c <-  (FP_c+FN_c)/nrow(test)) #0.2573099
(FP_rate_c <-  FP_c/(TN_c+FP_c)) #0.4333613
(FN_rate_c <- FN_c/(FN_c+TP_c)) #0.1551556
(sen_c <- TP_c/(FN_c+TP_c)) #0.8448444
(spec_c <- TN_c/(TN_c+FP_c)) # 0.5666387

#confusion matrix values
TN_c_2 <- con_c_0.7[1][1]
FN_c_2 <- con_c_0.7[2][1]
FP_c_2 <- con_c_0.7[3][1]
TP_c_2 <- con_c_0.7[4][1]

#overall error rate for white
(error_rate_c_2 <-  (FP_c_2+FN_c_2)/nrow(test)) #0.3007079
(FP_rate_c_2 <-  FP_c_2/(TN_c_2+FP_c_2)) #0.1835708
(FN_rate_c_2 <- FN_c_2/(FN_c_2+TP_c_2)) #0.368677
(sen_c_2 <- TP_c_2/(FN_c_2+TP_c_2)) #0.631323
(spec_c_2 <- TN_c_2/(TN_c_2+FP_c_2)) #0.8164292
