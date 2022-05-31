install.packages("tidyverse")

library(tidyverse)

wine = read.csv("Data/WineQT.csv")

head(wine)

#Renaming columns by replacing spaces with dots
names(wine) <- str_replace_all(names(wine), c(" "="."))

#Dropping Id column
wine = subset(wine, select = -c(Id))

