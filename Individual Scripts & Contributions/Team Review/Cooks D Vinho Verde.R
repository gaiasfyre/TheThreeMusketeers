# Remove Outliers Using Cook's Distance

## Import Packages
install.packages('ISLR')
install.packages('tibble')
install.packages ('dplyr')
library(ISLR)
library(tibble)
library(dplyr)

## Load Data
wine <-WineQT

## Model
model <- lm(fixed.acidity ~ ., data = wine)
summary(model)

## Plot
par(mfrow = c(2, 2))
plot(model)

## Cook's Distance
cooksD <- cooks.distance(model)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

names_of_influential <- names(influential)

outliers <- wine[names_of_influential,]

wine_without_outliers <- wine %>% anti_join(outliers)

## Model 2
model2 <- lm(fixed.acidity ~ ., data = wine_without_outliers)

summary(model2)