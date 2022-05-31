# Recoding 'Residual Sugars' from Continuous to Categorical

## Classes of Still Red Wine
###  Up to 4 g/l---      Dry (Sec)
###  4 g/l – 12 g/l---   Medium dry (Demi-sec) 
###  12 g/l – 45 g/l---  Medium (Medium sweet)
###  More than 45 g/l--- Sweet

## Recoding to:
### Dry -> 0
### Medium dry -> 1
### Medium -> 2
### Sweet -> 3

# Load in Packages
library ("dyplr")

# Load in Data

head(wine)

# Create Residual Sugars Recoded Column
wine$residual.sugarR <- NA
head(wine)

# Recode Residual Sugars
wine$residual.sugarR[wine$residual.sugar >45.00] <- 3
wine$residual.sugarR[wine$residual.sugar >12.00] <- 2
wine$residual.sugarR[wine$residual.sugar >4.01] <- 1
wine$residual.sugarR[wine$residual.sugar <4.00] <- 0





