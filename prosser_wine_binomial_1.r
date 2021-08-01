library(dplyr)
library(glue)
library(lambda.r)
library(MASS)
library(pryr)
library(tidyverse)

# function to write formula for the specified list of predictors
build_formula(xs) %as%
    glue("quality_bit ~ {paste(xs, collapse = ' + ')}")
   
# function to remove specified variables from vector
remove_var(xs, remove) %as%
    xs[Vectorize(f(x, all(remove != x)))(xs)]
    

setwd("...")
df <- read.csv("combined_wine.csv")

# create binary response variable for quality
df$quality_bit <- as.integer(df$quality > 5)

# remove responses from predictors
pred_full <- remove_var(names(df), c(
      "quality"
    , "quality_bit"
))

# build glm linear model
df_lm_full <- glm(build_formula(pred_full)
    , data = df
    , family = "binomial"
)
# equivelent to...
df_lm_full <- glm(quality_bit ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol + color
    , data = df
    , family = "binomial"
)
summary(df_lm_full)
