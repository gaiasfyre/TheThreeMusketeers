#import dataset
wine = read.csv('Data/WineQT.csv')

#load packages
library(tidyverse)

#view dataset
head(wine)

#drop ID column
wine1 = subset(wine, select = -c( 0,13))

#create box plots to visually identify variables with outliers

#all box plots
boxplot(wine1)
ggplot(stack(wine1), aes(x = ind, y = values))+
  geom_boxplot(fill='rosybrown', color="darkred") +
  coord_flip()

#box plots by variable
bp.fixed.acidity <- ggplot(wine, aes(x = "", y = fixed.acidity)) + geom_boxplot(fill='rosybrown', color="darkred") + coord_flip() + xlab("")
bp.fixed.acidity
# numerous outliers present (44)
# view outliers ($out)
boxplot.stats(wine$fixed.acidity)

bp.volatile.acidity <- ggplot(wine, aes(x = "", y = volatile.acidity)) + geom_boxplot() + xlab("")
bp.volatile.acidity
# numerous outliers present (14)
# view outliers ($out)
boxplot.stats(wine$volatile.acidity)


bp.citric.acid <- ggplot(wine, aes(x = "", y = citric.acid)) + geom_boxplot() + xlab("")
bp.citric.acid
# one outlier present
# view outliers ($out)
boxplot.stats(wine$citric.acid)

bp.sugar <- ggplot(wine, aes(x = "", y = residual.sugar)) + geom_boxplot() + xlab("")
bp.sugar
# numerous outliers present (110)
# view outliers ($out)
boxplot.stats(wine$residual.sugar)

bp.chlorides <- ggplot(wine, aes(x = "", y = chlorides)) + geom_boxplot() + xlab("")
bp.chlorides
# numerous outliers present (77)
# view outliers ($out)
boxplot.stats(wine$chlorides)

bp.free.sulfur <- ggplot(wine, aes(x = "", y = free.sulfur.dioxide)) + geom_boxplot() + xlab("")
bp.free.sulfur
# numerous outliers present (18)
# view outliers ($out)
boxplot.stats(wine$free.sulfur.dioxide)

bp.total.sulfur <- ggplot(wine, aes(x = "", y = total.sulfur.dioxide)) + geom_boxplot() + xlab("")
bp.total.sulfur
# numerous outliers present (40)
# view outliers ($out)
boxplot.stats(wine$total.sulfur.dioxide)

bp.density <- ggplot(wine, aes(x = "", y = density)) + geom_boxplot() + xlab("")
bp.density
# numerous outliers present (36)
# view outliers ($out)
boxplot.stats(wine$density)

bp.ph <- ggplot(wine, aes(x = "", y = pH)) + geom_boxplot() + xlab("")
bp.ph
# numerous outliers present (20)
# view outliers ($out)
boxplot.stats(wine$pH)

bp.sulphates <- ggplot(wine, aes(x = "", y = sulphates)) + geom_boxplot() + xlab("")
bp.sulphates
# numerous outliers present (43)
# view outliers ($out)
boxplot.stats(wine$sulphates)

bp.alcohol <- ggplot(wine, aes(x = "", y = alcohol)) + geom_boxplot() + xlab("")
bp.alcohol
# a few outliers present (12)
# view outliers ($out)
boxplot.stats(wine$alcohol)

# create scatter / qq "normal probability" plots (for assessment of normality)
qq.fixed.acidity <- ggplot(wine, aes(sample = fixed.acidity)) + geom_qq() 
qq.fixed.acidity
#

qq.volatile.acidity <- ggplot(wine, aes(sample = volatile.acidity)) + geom_qq()
qq.volatile.acidity
#

qq.citric.acid <- ggplot(wine, aes(sample = citric.acid)) + geom_qq()
qq.citric.acid
#

qq.sugar <- ggplot(wine, aes(sample = residual.sugar)) + geom_qq()
qq.sugar
#

qq.chlorides <- ggplot(wine, aes(sample = chlorides)) + geom_qq()
qq.chlorides
#

qq.free.sulfer <- ggplot(wine, aes(sample = free.sulfur.dioxide)) + geom_qq()
qq.free.sulfer
#

qq.total.sulfer <- ggplot(wine, aes(sample = total.sulfur.dioxide)) + geom_qq()
qq.total.sulfer
#

qq.density <- ggplot(wine, aes(sample = density)) + geom_qq()
qq.density
#

qq.ph <- ggplot(wine, aes(sample = pH)) + geom_qq()
qq.ph
#

qq.sulphates <- ggplot(wine, aes(sample = sulphates)) + geom_qq()
qq.sulphates
#

qq.alcohol <- ggplot(wine, aes(sample = alcohol)) + geom_qq()
qq.alcohol
#