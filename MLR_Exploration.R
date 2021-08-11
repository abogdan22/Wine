#Reading in Data Files
library(ggplot2)
install.packages("performance")
library(performance)
white <- read.csv('wineQualityWhites.csv', header=TRUE)
red <- read.csv('wineQualityReds.csv', header=TRUE)
white
red
################################################################################
#Data Exploration
################################################################################
par(mfrow=c(1,2))
hist(white$quality, labels=TRUE)
hist(red$quality)

boxplot(white$quality, main="Quality of White Wine")
boxplot(red$quality, main="Quality of Red Wine")


# Basic scatter plot
ggplot(data = white) + 
  geom_point(mapping = aes(x=total.sulfur.dioxide, y=quality, color = free.sulfur.dioxide))

counts <- table(dataSet$cut)
bp<-barplot(counts, main = "Cut distribution", xlab="Types of cuts", ylab = "Number of Cuts")   
text(bp, 0, round(counts, 1),cex=0.8,pos=3)

#(e.g. there are many more normal wines than excellent or poor ones). 
#What is the ranking system?
#For logistic regression, maybe 6 would be a good cut off
################################################################################
#Looking at modeling only reds
################################################################################
red.full <- lm(quality~., data=red)
summary(red.full)

rednull <- lm(quality~1, data=red)
##model with all predictors
redfull <- lm(quality~., data=red)
step(redfull, scope=list(lower=rednull, upper=redfull), direction="backward")
#coefficients for red only with backwards: volatile.acidity, chlorides, free.sulfer.dioxide, total.sulfer.dioxide
#pH, sulphates, alcohol
red.backwards <- lm(quality~volatile.acidity+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol, data=red)
vif(red.backwards)
summary(red.backwards)
################################################################################
#Checking assumptions for backwards red model
################################################################################
plot(red.backwards$fitted.values,red.backwards$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")
#To me this looks pretty bad, like zebra stripes
library(MASS)
boxcox(red.backwards, seq(0,3,0.2))
#1 falls in acceptable range so no y-transformation is necessary
red.backwards.logall <- lm(quality~log(volatile.acidity)+log(chlorides)+log(free.sulfur.dioxide)+total.sulfur.dioxide+log(pH)+sulphates+log(alcohol), data=red)
acf(red.backwards.logall$residuals, main="ACF of Residuals")
#We fail this test badly, again.
##Normal probability or QQ plot of residuals
qqnorm(red.backwards$residuals)
qqline(red.backwards$residuals, col="red")
check_collinearity(white.backwards)
################################################################################
#Combining Data Sets
################################################################################
all_wine <- read.csv('combined_wine.csv',header=TRUE)
attach(all_wine)
################################################################################
#Looking at correlations between variables
################################################################################
preds<-cbind(quality, fixed.acidity, volatile.acidity, citric.acid, residual.sugar, chlorides, free.sulfur.dioxide, total.sulfur.dioxide, density, pH, sulphates, alcohol, color)
cor(preds)
#Large correlations between density & alcohol, free and total sulfer dioxide
full <- lm(quality~., data=all_wine)
partial1 <- lm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+total.sulfur.dioxide+pH+sulphates+alcohol+color)

anova(partial1, full)
#since p < 0.05 reject null hypothesis and the full model is preferable

library(faraway)
vif(full)

partial2 <- lm(quality~fixed.acidity+residual.sugar+total.sulfur.dioxide+density+color)

anova(partial2, full)
#since p < 0.05 reject null hypothesis and the full model is preferable

summary(full)
# t-test says we can drop citric acid
partial3 <- lm(quality~fixed.acidity+volatile.acidity+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol+color)
summary(partial3)
vif(partial3)
#density & residual.sugar has by far the highest VIF so try dropping that first
#anova said not to do this, then tried dropping things related closely to density
#anova said not to do this also
partial4 <- lm(quality~+volatile.acidity++chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+color)

anova(partial4, partial3)
#reject the null hypothesis, keep them in the model...

#Trying backwards selection:
regnull <- lm(quality~1, data=all_wine)
##model with all predictors
regfull <- lm(quality~fixed.acidity+volatile.acidity+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol+color, data=all_wine)
#
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")


partial5 <- lm(quality ~ fixed.acidity + volatile.acidity + residual.sugar + 
                  chlorides + free.sulfur.dioxide + total.sulfur.dioxide + 
                  density + pH + sulphates + alcohol + color)
#came up with the exact same as the start
#So lets try forwards

step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
partial6 <- lm(quality ~ alcohol + volatile.acidity + sulphates + 
  residual.sugar + color + density + free.sulfur.dioxide + 
  total.sulfur.dioxide + chlorides + pH + fixed.acidity)
vif(partial6)
#SAME HIGH VIF SCORES AGAIN
partial7 <- lm(quality ~ alcohol + volatile.acidity + sulphates + 
                 residual.sugar + free.sulfur.dioxide + 
                 total.sulfur.dioxide + chlorides + pH + fixed.acidity)
vif(partial7)
summary(partial7)
anova(partial7, partial3)
########################################
#Claims that we should not drop, but multicollinearity is present if we don't.
########################################
#Checking if transformations needed
plot(partial7$fitted.values,partial7$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")

library(MASS)
boxcox(partial7, seq(0,1,0.2))
quality.8 <- quality**.8
partial7 <- lm(quality.8 ~ alcohol + volatile.acidity + sulphates + 
                 residual.sugar + free.sulfur.dioxide + 
                 total.sulfur.dioxide + chlorides + pH + fixed.acidity)
boxcox(partial7, seq(0,2,0.2))

##ACF plot of residuals
acf(partial7$residuals, main="ACF of Residuals")

##Normal probability or QQ plot of residuals
qqnorm(partial7$residuals)
qqline(partial7$residuals, col="red")
################################################################################
#Looking at modeling only white wine
################################################################################
white.full <- lm(quality~., data=white)
summary(white.full)

whitenull <- lm(quality~1, data=white)
##model with all predictors
whitefull <- lm(quality~., data=white)
step(whitefull, scope=list(lower=whitenull, upper=whitefull), direction="backward")
#coefficients to keep according to backward: fixed.acidity, volatile.acidity, residual.sugar, free.sulfer.dioxide
#density, pH, sulphates, alcohol
white.backwards<- lm(quality~fixed.acidity+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+alcohol, data=white)
vif(white.backwards)
#sugar and density have very high VIFs
#dropping sugar
white.backwards.no.sugar<-lm(quality~fixed.acidity+volatile.acidity+free.sulfur.dioxide+density+pH+sulphates+alcohol, data=white)
white.backwards.no.density<-lm(quality~fixed.acidity+volatile.acidity+residual.sugar+free.sulfur.dioxide+pH+sulphates+alcohol, data=white)
white.backwards.no.high.vif <- lm(quality~fixed.acidity+volatile.acidity+free.sulfur.dioxide+pH+sulphates+alcohol, data=white)
#comparing backwards to dropped vif
anova(white.backwards.no.high.vif, white.backwards)
#reject null hypothesis which means use full model
#comparing backwards with only dropping sugar
anova(white.backwards.no.sugar, white.backwards)
#reject null hypothesis which means use full model
#comparing backwards with no density
anova(white.backwards.no.density, white.backwards)
#reject null hypothesis which means use full model.
################################################################################
#WHY IS VIF TELLING ME THERE'S MULTICOLLINEARITY BUT ANOVA NOT ALLOWING ME TO DROP
#PREDICTORS THAT COULD HELP THIS PROBLEM

