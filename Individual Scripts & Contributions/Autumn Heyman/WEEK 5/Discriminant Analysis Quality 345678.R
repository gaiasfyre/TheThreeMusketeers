## Load Libraries
library(MASS)
library(ggplot2)

## Load in Data
wine<-WineQt.csv

## View Structure of the Data Set
str(wine)


### Recode Quality into 3 Groups
wine$qualityR <- NA

wine$qualityR [wine$quality==3] <- 0
wine$qualityR [wine$quality==4] <- 0
wine$qualityR [wine$quality==5] <- 1
wine$qualityR [wine$quality==6] <- 1
wine$qualityR [wine$quality==7] <- 2
wine$qualityR [wine$quality==8] <- 2

head(wine)

### Get Rid of Unnecessary Columns
wine_wrangled <- wine[, c(1,5,8,9,12)]
head(wine_wrangled)

##Scale the data:scale each predictor value
wine_wrangled[1:4] <- scale(wine_wrangled[1:4])

##Find mean of each predictor variable
apply(wine_wrangled[1:4], 2, mean)

###Find standard deviation of each predictor variable
apply(wine_wrangled[1:4], 2, sd) 

##Create Training and Test Samples
###Make this example reproducible
set.seed(1)

###Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(wine_wrangled), replace=TRUE, prob=c(0.7,0.3))
train <- wine_wrangled[sample, ]
test <- wine_wrangled[!sample, ]

##Fit LDA Model
LDAmodel <- lda(quality~., data=wine_wrangled)

#view model output
LDAmodel

#Prior probabilities of group: These represent the proportions of each Quality Group in the training set. 
## 3.4% of all observations in the training set were in the quality group 0 (quality rating 3-4).
## 82.7% of all observations in the training set were in the quality group 1 (quality rating 5-6).
## 13.9% of all observations in the training set were in the quality group 0 (quality rating 3-4).


#Group means: These display the mean values for each predictor variable for each species.
## Density
## pH
## Fixed Acidity
## Chlorides

#Coefficients of linear discriminants: These display the linear combination of 
#predictor variables that are used to form the decision rule of the LDA model. 

## LD1: density: -1.34, pH: 0.27, fixed.acidity: 1.50, chlorides: -0.12
## LD2: density: 0.38, pH: -1.41, fixed.acidity: -0.94, chlorides: -0.58

#Use LDA model to make predictions on test data
predicted <- predict(LDAmodel, test)

names(predicted)


#View predicted class for first six observations in test set
predicted$class

#View predicted posterior for first six observations in test set
predicted$posterior

#View linear discriminants for first six observations in test set
predicted$x

#Find accuracy of the model
mean(predicted$class==test$qualityR)
##The model accurately predicted the quality group for 99.7% of the observations.

#Visualize the results
##Define data to plot
lda_plot <- cbind(train, predict(LDAmodel)$x)

##Create plot
ggplot(lda_plot, aes(LD1,LD2))+
  geom_point(aes(color==qualityR))
predicted <- predict(LDAmodel, test)
mean(predicted$class==test$qualityR)

