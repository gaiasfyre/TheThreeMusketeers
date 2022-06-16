---
title: "Wine Quality Prediction Project"
authors: "Autumn Heyman, Erin Weaver, Georgia Miller"
output: html_document
---
# File Setup Tasks
## Import Libraries


```r
library("tidyverse")
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
```

```
## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
## ✓ readr   2.0.2     ✓ forcats 0.5.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library("caret")
```

```
## Loading required package: lattice
```

```
## 
## Attaching package: 'caret'
```

```
## The following object is masked from 'package:purrr':
## 
##     lift
```

```r
library("lmtest")
```

```
## Loading required package: zoo
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```r
library("magrittr")
```

```
## 
## Attaching package: 'magrittr'
```

```
## The following object is masked from 'package:purrr':
## 
##     set_names
```

```
## The following object is masked from 'package:tidyr':
## 
##     extract
```

```r
library("dplyr")
library("tidyr")
library("popbio")
```

```
## 
## Attaching package: 'popbio'
```

```
## The following object is masked from 'package:caret':
## 
##     sensitivity
```

```r
library("e1071")
library("PerformanceAnalytics")
```

```
## Loading required package: xts
```

```
## 
## Attaching package: 'xts'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     first, last
```

```
## 
## Attaching package: 'PerformanceAnalytics'
```

```
## The following objects are masked from 'package:e1071':
## 
##     kurtosis, skewness
```

```
## The following object is masked from 'package:graphics':
## 
##     legend
```

```r
library("corrplot")
```

```
## corrplot 0.91 loaded
```

```r
library("corpcor")
library("MASS")
```

```
## 
## Attaching package: 'MASS'
```

```
## The following object is masked from 'package:dplyr':
## 
##     select
```

```r
library("brant")
library("car")
```

```
## Loading required package: carData
```

```
## 
## Attaching package: 'car'
```

```
## The following object is masked from 'package:dplyr':
## 
##     recode
```

```
## The following object is masked from 'package:purrr':
## 
##     some
```

```r
library("effects")
```

```
## Use the command
##     lattice::trellis.par.set(effectsTheme())
##   to customize lattice options for effects plots.
## See ?effectsTheme for details.
```

## Set Working Directory
*** can be manually overridden by selecting source file location from the session menu **


```r
setwd("/Users/erinweaver/Documents/GitHub/TheThreeMusketeers")
```

## Import Dataset


```r
wineqt <- read.csv("/Users/erinweaver/Documents/GitHub/TheThreeMusketeers/Data/WineQT.csv")
head(wineqt)
```

```
##   fixed.acidity volatile.acidity citric.acid residual.sugar chlorides
## 1           7.4             0.70        0.00            1.9     0.076
## 2           7.8             0.88        0.00            2.6     0.098
## 3           7.8             0.76        0.04            2.3     0.092
## 4          11.2             0.28        0.56            1.9     0.075
## 5           7.4             0.70        0.00            1.9     0.076
## 6           7.4             0.66        0.00            1.8     0.075
##   free.sulfur.dioxide total.sulfur.dioxide density   pH sulphates alcohol
## 1                  11                   34  0.9978 3.51      0.56     9.4
## 2                  25                   67  0.9968 3.20      0.68     9.8
## 3                  15                   54  0.9970 3.26      0.65     9.8
## 4                  17                   60  0.9980 3.16      0.58     9.8
## 5                  11                   34  0.9978 3.51      0.56     9.4
## 6                  13                   40  0.9978 3.51      0.56     9.4
##   quality Id
## 1       5  0
## 2       5  1
## 3       5  2
## 4       6  3
## 5       5  4
## 6       5  5
```

# Data Wrangling 

## Drop Id Column 


```r
wine_subset <- wineqt[, c(1,2,3,4,5,6,7,8,9,10,11,12)]
```

## Recode into three groups


```r
wine_subset$qualityR <- NA
wine_subset$qualityR [wine_subset$quality==3] <- 0
wine_subset$qualityR [wine_subset$quality==4] <- 0
wine_subset$qualityR [wine_subset$quality==5] <- 1
wine_subset$qualityR [wine_subset$quality==6] <- 1
wine_subset$qualityR [wine_subset$quality==7] <- 2
wine_subset$qualityR [wine_subset$quality==8] <- 2
head(wine_subset)
```

```
##   fixed.acidity volatile.acidity citric.acid residual.sugar chlorides
## 1           7.4             0.70        0.00            1.9     0.076
## 2           7.8             0.88        0.00            2.6     0.098
## 3           7.8             0.76        0.04            2.3     0.092
## 4          11.2             0.28        0.56            1.9     0.075
## 5           7.4             0.70        0.00            1.9     0.076
## 6           7.4             0.66        0.00            1.8     0.075
##   free.sulfur.dioxide total.sulfur.dioxide density   pH sulphates alcohol
## 1                  11                   34  0.9978 3.51      0.56     9.4
## 2                  25                   67  0.9968 3.20      0.68     9.8
## 3                  15                   54  0.9970 3.26      0.65     9.8
## 4                  17                   60  0.9980 3.16      0.58     9.8
## 5                  11                   34  0.9978 3.51      0.56     9.4
## 6                  13                   40  0.9978 3.51      0.56     9.4
##   quality qualityR
## 1       5        1
## 2       5        1
## 3       5        1
## 4       6        1
## 5       5        1
## 6       5        1
```
## Drop Quality


```r
wine_wrangled <- wine_subset[, c(1,2,3,4,5,6,7,8,9,10,11,13)]
head(wine_wrangled)
```

```
##   fixed.acidity volatile.acidity citric.acid residual.sugar chlorides
## 1           7.4             0.70        0.00            1.9     0.076
## 2           7.8             0.88        0.00            2.6     0.098
## 3           7.8             0.76        0.04            2.3     0.092
## 4          11.2             0.28        0.56            1.9     0.075
## 5           7.4             0.70        0.00            1.9     0.076
## 6           7.4             0.66        0.00            1.8     0.075
##   free.sulfur.dioxide total.sulfur.dioxide density   pH sulphates alcohol
## 1                  11                   34  0.9978 3.51      0.56     9.4
## 2                  25                   67  0.9968 3.20      0.68     9.8
## 3                  15                   54  0.9970 3.26      0.65     9.8
## 4                  17                   60  0.9980 3.16      0.58     9.8
## 5                  11                   34  0.9978 3.51      0.56     9.4
## 6                  13                   40  0.9978 3.51      0.56     9.4
##   qualityR
## 1        1
## 2        1
## 3        1
## 4        1
## 5        1
## 6        1
```

## Check All Data Types 


```r
str(wine_wrangled)
```

```
## 'data.frame':	1143 obs. of  12 variables:
##  $ fixed.acidity       : num  7.4 7.8 7.8 11.2 7.4 7.4 7.9 7.3 7.8 6.7 ...
##  $ volatile.acidity    : num  0.7 0.88 0.76 0.28 0.7 0.66 0.6 0.65 0.58 0.58 ...
##  $ citric.acid         : num  0 0 0.04 0.56 0 0 0.06 0 0.02 0.08 ...
##  $ residual.sugar      : num  1.9 2.6 2.3 1.9 1.9 1.8 1.6 1.2 2 1.8 ...
##  $ chlorides           : num  0.076 0.098 0.092 0.075 0.076 0.075 0.069 0.065 0.073 0.097 ...
##  $ free.sulfur.dioxide : num  11 25 15 17 11 13 15 15 9 15 ...
##  $ total.sulfur.dioxide: num  34 67 54 60 34 40 59 21 18 65 ...
##  $ density             : num  0.998 0.997 0.997 0.998 0.998 ...
##  $ pH                  : num  3.51 3.2 3.26 3.16 3.51 3.51 3.3 3.39 3.36 3.28 ...
##  $ sulphates           : num  0.56 0.68 0.65 0.58 0.56 0.56 0.46 0.47 0.57 0.54 ...
##  $ alcohol             : num  9.4 9.8 9.8 9.8 9.4 9.4 9.4 10 9.5 9.2 ...
##  $ qualityR            : num  1 1 1 1 1 1 1 2 2 1 ...
```


# Exploratory Analysis

### Summarizing the dataset

```r
summary(wine_wrangled)
```

```
##  fixed.acidity    volatile.acidity  citric.acid     residual.sugar  
##  Min.   : 4.600   Min.   :0.1200   Min.   :0.0000   Min.   : 0.900  
##  1st Qu.: 7.100   1st Qu.:0.3925   1st Qu.:0.0900   1st Qu.: 1.900  
##  Median : 7.900   Median :0.5200   Median :0.2500   Median : 2.200  
##  Mean   : 8.311   Mean   :0.5313   Mean   :0.2684   Mean   : 2.532  
##  3rd Qu.: 9.100   3rd Qu.:0.6400   3rd Qu.:0.4200   3rd Qu.: 2.600  
##  Max.   :15.900   Max.   :1.5800   Max.   :1.0000   Max.   :15.500  
##    chlorides       free.sulfur.dioxide total.sulfur.dioxide    density      
##  Min.   :0.01200   Min.   : 1.00       Min.   :  6.00       Min.   :0.9901  
##  1st Qu.:0.07000   1st Qu.: 7.00       1st Qu.: 21.00       1st Qu.:0.9956  
##  Median :0.07900   Median :13.00       Median : 37.00       Median :0.9967  
##  Mean   :0.08693   Mean   :15.62       Mean   : 45.91       Mean   :0.9967  
##  3rd Qu.:0.09000   3rd Qu.:21.00       3rd Qu.: 61.00       3rd Qu.:0.9978  
##  Max.   :0.61100   Max.   :68.00       Max.   :289.00       Max.   :1.0037  
##        pH          sulphates         alcohol         qualityR    
##  Min.   :2.740   Min.   :0.3300   Min.   : 8.40   Min.   :0.000  
##  1st Qu.:3.205   1st Qu.:0.5500   1st Qu.: 9.50   1st Qu.:1.000  
##  Median :3.310   Median :0.6200   Median :10.20   Median :1.000  
##  Mean   :3.311   Mean   :0.6577   Mean   :10.44   Mean   :1.105  
##  3rd Qu.:3.400   3rd Qu.:0.7300   3rd Qu.:11.10   3rd Qu.:1.000  
##  Max.   :4.010   Max.   :2.0000   Max.   :14.90   Max.   :2.000
```

********* STOP SIGN ******** :)

# Assumption Testing:

## Assumption 1: The Dependent Variable is Ordered
### Results of Test: Pass.

## Assumption 2: One or more of the independent variables are either continuous, categorical, or ordinal
### Results of Test: Pass.

## Assumption 3: Absence of Multicollinearity

### Correlation Matrix

```r
wine_wrangled$qualityR <- as.numeric(wine_wrangled$qualityR)
corr_matrix <- cor(wine_wrangled)
corr_matrix
```

```
##                      fixed.acidity volatile.acidity citric.acid residual.sugar
## fixed.acidity           1.00000000     -0.250728322  0.67315725    0.171830535
## volatile.acidity       -0.25072832      1.000000000 -0.54418694   -0.005751097
## citric.acid             0.67315725     -0.544186937  1.00000000    0.175814854
## residual.sugar          0.17183054     -0.005751097  0.17581485    1.000000000
## chlorides               0.10788857      0.056336259  0.24531249    0.070863112
## free.sulfur.dioxide    -0.16483079     -0.001962479 -0.05758910    0.165338797
## total.sulfur.dioxide   -0.11062837      0.077747722  0.03687111    0.190790035
## density                 0.68150088      0.016511520  0.37524326    0.380146952
## pH                     -0.68516260      0.221491518 -0.54633914   -0.116958936
## sulphates               0.17459183     -0.276078597  0.33123176    0.017474504
## alcohol                -0.07505485     -0.203909273  0.10625034    0.058420606
## qualityR                0.12543453     -0.355576085  0.25696751    0.051997327
##                        chlorides free.sulfur.dioxide total.sulfur.dioxide
## fixed.acidity         0.10788857        -0.164830793          -0.11062837
## volatile.acidity      0.05633626        -0.001962479           0.07774772
## citric.acid           0.24531249        -0.057589104           0.03687111
## residual.sugar        0.07086311         0.165338797           0.19079003
## chlorides             1.00000000         0.015280458           0.04816316
## free.sulfur.dioxide   0.01528046         1.000000000           0.66109287
## total.sulfur.dioxide  0.04816316         0.661092872           1.00000000
## density               0.20890071        -0.054150318           0.05017483
## pH                   -0.27775907         0.072803706          -0.05912572
## sulphates             0.37478389         0.034445122           0.02689368
## alcohol              -0.22991709        -0.047094832          -0.18816480
## qualityR             -0.10634301        -0.033256243          -0.07728294
##                          density          pH   sulphates     alcohol
## fixed.acidity         0.68150088 -0.68516260  0.17459183 -0.07505485
## volatile.acidity      0.01651152  0.22149152 -0.27607860 -0.20390927
## citric.acid           0.37524326 -0.54633914  0.33123176  0.10625034
## residual.sugar        0.38014695 -0.11695894  0.01747450  0.05842061
## chlorides             0.20890071 -0.27775907  0.37478389 -0.22991709
## free.sulfur.dioxide  -0.05415032  0.07280371  0.03444512 -0.04709483
## total.sulfur.dioxide  0.05017483 -0.05912572  0.02689368 -0.18816480
## density               1.00000000 -0.35277462  0.14313929 -0.49472690
## pH                   -0.35277462  1.00000000 -0.18549903  0.22532220
## sulphates             0.14313929 -0.18549903  1.00000000  0.09442113
## alcohol              -0.49472690  0.22532220  0.09442113  1.00000000
## qualityR             -0.13191392 -0.10392646  0.19532723  0.36792683
##                         qualityR
## fixed.acidity         0.12543453
## volatile.acidity     -0.35557608
## citric.acid           0.25696751
## residual.sugar        0.05199733
## chlorides            -0.10634301
## free.sulfur.dioxide  -0.03325624
## total.sulfur.dioxide -0.07728294
## density              -0.13191392
## pH                   -0.10392646
## sulphates             0.19532723
## alcohol               0.36792683
## qualityR              1.00000000
```

### Correlation Plots


```r
corrplot(corr_matrix, type="upper", order="hclust", p.mat = corr_matrix, sig.level = 0.01, insig="blank")
```

<img src="Ordinal-345678_files/figure-html/unnamed-chunk-10-1.png" width="672" />


```r
corrplot(corr_matrix, method = "number", is.corr = FALSE)
```

<img src="Ordinal-345678_files/figure-html/unnamed-chunk-11-1.png" width="672" />

### Pearson's Test


```r
chart.Correlation(wine_wrangled, histogram=FALSE, method="pearson")
```

<img src="Ordinal-345678_files/figure-html/unnamed-chunk-12-1.png" width="672" />
### Results of Test: Fail, assuming the cut off is 0.6 for correlation, Pass if the cut off is 0.7 for correlation.

The Variable Pairs with the highest correlation are as follows:
* pH & fixed.acidity (-0.69)
* density & fixed.acidity (0.68)
* citric.acid & fixed.acidity (0.67)
* total sulfur.dioxide & free.sulfur.dioxide (0.66)
--> should we drop fixed.acidity & free.sulfur.dioxide?


# Creating a Base Model  

## Define the Order of the Dependent Variable

```r
wine_wrangled$qualityR = factor(wine_wrangled$qualityR, levels = c("0", "1", "2"), ordered = TRUE) 
str(wine_wrangled)
```

```
## 'data.frame':	1143 obs. of  12 variables:
##  $ fixed.acidity       : num  7.4 7.8 7.8 11.2 7.4 7.4 7.9 7.3 7.8 6.7 ...
##  $ volatile.acidity    : num  0.7 0.88 0.76 0.28 0.7 0.66 0.6 0.65 0.58 0.58 ...
##  $ citric.acid         : num  0 0 0.04 0.56 0 0 0.06 0 0.02 0.08 ...
##  $ residual.sugar      : num  1.9 2.6 2.3 1.9 1.9 1.8 1.6 1.2 2 1.8 ...
##  $ chlorides           : num  0.076 0.098 0.092 0.075 0.076 0.075 0.069 0.065 0.073 0.097 ...
##  $ free.sulfur.dioxide : num  11 25 15 17 11 13 15 15 9 15 ...
##  $ total.sulfur.dioxide: num  34 67 54 60 34 40 59 21 18 65 ...
##  $ density             : num  0.998 0.997 0.997 0.998 0.998 ...
##  $ pH                  : num  3.51 3.2 3.26 3.16 3.51 3.51 3.3 3.39 3.36 3.28 ...
##  $ sulphates           : num  0.56 0.68 0.65 0.58 0.56 0.56 0.46 0.47 0.57 0.54 ...
##  $ alcohol             : num  9.4 9.8 9.8 9.8 9.4 9.4 9.4 10 9.5 9.2 ...
##  $ qualityR            : Ord.factor w/ 3 levels "0"<"1"<"2": 2 2 2 2 2 2 2 3 3 2 ...
```

## Dividing data into training and test set  *** REVIEW THIS SECTION ***
*** how to determine appropriate sample size
*** what is seed and how to determine appropriate value

### Random sampling 

```r
samplesize = 0.60*nrow(wine_wrangled)
set.seed(100)
index = sample(seq_len(nrow(wine_wrangled)), size = samplesize)
```

### Creating training and test set 

```r
datatrain = wine_wrangled[index,]
datatest = wine_wrangled[-index,]
```

## Create the Ordinal Logistic Regression Model

```r
model= polr(qualityR ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data = wine_wrangled, Hess = TRUE)

summary(model)
```

```
## Call:
## polr(formula = qualityR ~ fixed.acidity + volatile.acidity + 
##     citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + 
##     total.sulfur.dioxide + density + pH + sulphates + alcohol, 
##     data = wine_wrangled, Hess = TRUE)
## 
## Coefficients:
##                           Value Std. Error  t value
## fixed.acidity         -0.330008   0.082411  -4.0044
## volatile.acidity      -4.405687   0.661679  -6.6583
## citric.acid            0.845356   0.790894   1.0689
## residual.sugar        -0.060996   0.068254  -0.8937
## chlorides             -8.934343   2.368258  -3.7725
## free.sulfur.dioxide    0.001840   0.011773   0.1563
## total.sulfur.dioxide  -0.004261   0.003804  -1.1202
## density              273.446919   1.637315 167.0094
## pH                    -4.146601   0.863993  -4.7993
## sulphates              1.777010   0.544503   3.2635
## alcohol                1.110960   0.094846  11.7133
## 
## Intercepts:
##     Value    Std. Error t value 
## 0|1 261.2794   1.6791   155.6049
## 1|2 268.2894   1.6758   160.0944
## 
## Residual Deviance: 947.3073 
## AIC: 973.3073
```

## Significance Table of coefficients and intercepts


```r
summary_table <- coef(summary(model))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table
```

```
##                              Value  Std. Error     t value p value
## fixed.acidity         -0.330007685 0.082411311  -4.0043980   0.000
## volatile.acidity      -4.405687281 0.661679176  -6.6583436   0.000
## citric.acid            0.845355599 0.790893893   1.0688610   0.285
## residual.sugar        -0.060996277 0.068253771  -0.8936690   0.371
## chlorides             -8.934343441 2.368257602  -3.7725387   0.000
## free.sulfur.dioxide    0.001840378 0.011772759   0.1563251   0.876
## total.sulfur.dioxide  -0.004261068 0.003803822  -1.1202068   0.263
## density              273.446919191 1.637314577 167.0093964   0.000
## pH                    -4.146600904 0.863993383  -4.7993434   0.000
## sulphates              1.777009838 0.544502584   3.2635471   0.001
## alcohol                1.110960010 0.094846094  11.7132922   0.000
## 0|1                  261.279402659 1.679120772 155.6048898   0.000
## 1|2                  268.289427620 1.675819874 160.0944301   0.000
```

ordinaloutput <- as.data.frame(summary_table)
ordinaloutput

library(tibble)
ordinaloutput <- tibble::rownames_to_column(ordinaloutput, "Variable")

write.csv(ordinaloutput,"/Users/erinweaver/Documents/GitHub/TheThreeMusketeers/Data/ordinaloutput.csv", row.names = FALSE)


## Explanation of Results:


## Assumption 4: Proportional Odds  - run after model is generated


```r
brant(model)
```

```
## ---------------------------------------------------- 
## Test for		X2	df	probability 
## ---------------------------------------------------- 
## Omnibus			36.22	11	0
## fixed.acidity		3.31	1	0.07
## volatile.acidity	2.28	1	0.13
## citric.acid		1.76	1	0.18
## residual.sugar		6.72	1	0.01
## chlorides		1.41	1	0.23
## free.sulfur.dioxide	0.28	1	0.6
## total.sulfur.dioxide	6.71	1	0.01
## density			4.18	1	0.04
## pH			5.18	1	0.02
## sulphates		8.87	1	0
## alcohol			0.02	1	0.89
## ---------------------------------------------------- 
## 
## H0: Parallel Regression Assumption holds
```

### Results of Test: Pass


# Making predictions

## Compute Confusion Matrix and Misclassification Error 


```r
predict.quality = predict(model,datatest)
table(datatest$qualityR, predict.quality)
```

```
##    predict.quality
##       0   1   2
##   0   0  18   0
##   1   1 352  18
##   2   0  46  23
```

```r
mean(as.character(datatest$qualityR) != as.character(predict.quality))
```

```
## [1] 0.1812227
```

### Interpretation of the Confusion Matrix
###In the test dataset, 0 times the below average "0" wine quality category is identified correctly. We observe that the model identifies below average wine poorly. This is because there is an inadequate representation of "below average" wine quality category in the traning dataset.
###In the test dataset, 352 times the average "1" wine quality category is identified correctly.
###In the test dataset, 23 times the above average "2" wine quality category is identified correctly.
###Using the confusion matrix, we find that the misclassification error for our model is 18%.


## Interpretation Using Plots: Plotting the Effects

### Fixed Acidity

```r
Effect(focal.predictors = "fixed.acidity",model)
```

```
## 
## fixed.acidity effect (probability) for 0
## fixed.acidity
##         4.6         7.4          10          13          16 
## 0.003745705 0.009383534 0.021852374 0.056715093 0.139277364 
## 
## fixed.acidity effect (probability) for 1
## fixed.acidity
##       4.6       7.4        10        13        16 
## 0.8026304 0.9036027 0.9393070 0.9284919 0.8551744 
## 
## fixed.acidity effect (probability) for 2
## fixed.acidity
##         4.6         7.4          10          13          16 
## 0.193623937 0.087013809 0.038840610 0.014793010 0.005548189
```

```r
plot(Effect(focal.predictors = "fixed.acidity",model))
```

<img src="Ordinal-345678_files/figure-html/unnamed-chunk-20-1.png" width="672" />

### Volatile Acidity

```r
Effect(focal.predictors = "volatile.acidity",model)
```

```
## 
## volatile.acidity effect (probability) for 0
## volatile.acidity
##         0.1         0.5         0.8           1           2 
## 0.001909428 0.011022147 0.040115153 0.091627614 0.892041480 
## 
## volatile.acidity effect (probability) for 1
## volatile.acidity
##       0.1       0.5       0.8         1         2 
## 0.6774838 0.9140440 0.9387396 0.8995018 0.1078493 
## 
## volatile.acidity effect (probability) for 2
## volatile.acidity
##          0.1          0.5          0.8            1            2 
## 0.3206067810 0.0749338450 0.0211452954 0.0088705950 0.0001092469
```

```r
plot(Effect(focal.predictors = "volatile.acidity",model))
```

<img src="Ordinal-345678_files/figure-html/unnamed-chunk-21-1.png" width="672" />

### Citric Acid

```r
Effect(focal.predictors = "citric.acid",model)
```

```
## 
## citric.acid effect (probability) for 0
## citric.acid
##           0         0.2         0.5         0.8           1 
## 0.015799763 0.013374963 0.010410120 0.008097103 0.006846212 
## 
## citric.acid effect (probability) for 1
## citric.acid
##         0       0.2       0.5       0.8         1 
## 0.9309580 0.9241877 0.9105535 0.8923234 0.8773555 
## 
## citric.acid effect (probability) for 2
## citric.acid
##          0        0.2        0.5        0.8          1 
## 0.05324227 0.06243738 0.07903634 0.09957945 0.11579834
```

```r
plot(Effect(focal.predictors = "citric.acid",model))
```

<img src="Ordinal-345678_files/figure-html/unnamed-chunk-22-1.png" width="672" />

### Residual Sugar

```r
Effect(focal.predictors = "residual.sugar",model)
```

```
## 
## residual.sugar effect (probability) for 0
## residual.sugar
##        0.9          5          8         10         20 
## 0.01144997 0.01465561 0.01754684 0.01977843 0.03580439 
## 
## residual.sugar effect (probability) for 1
## residual.sugar
##       0.9         5         8        10        20 
## 0.9162425 0.9281205 0.9343380 0.9373955 0.9404610 
## 
## residual.sugar effect (probability) for 2
## residual.sugar
##        0.9          5          8         10         20 
## 0.07230751 0.05722390 0.04811520 0.04282605 0.02373458
```

```r
plot(Effect(focal.predictors = "residual.sugar",model))
```

<img src="Ordinal-345678_files/figure-html/unnamed-chunk-23-1.png" width="672" />

### Chlorides

```r
Effect(focal.predictors = "chlorides",model)
```

```
## 
## chlorides effect (probability) for 0
## chlorides
##        0.01         0.2         0.3         0.5         0.6 
## 0.006393624 0.033943836 0.079067827 0.338897397 0.556068864 
## 
## chlorides effect (probability) for 1
## chlorides
##      0.01       0.2       0.3       0.5       0.6 
## 0.8705700 0.9410061 0.9105265 0.6593446 0.4432109 
## 
## chlorides effect (probability) for 2
## chlorides
##         0.01          0.2          0.3          0.5          0.6 
## 0.1230364234 0.0250500509 0.0104056661 0.0017580099 0.0007202097
```

```r
plot(Effect(focal.predictors = "chlorides",model))
```

<img src="Ordinal-345678_files/figure-html/unnamed-chunk-24-1.png" width="672" />

### Free Sulfur Dioxide

```r
Effect(focal.predictors = "free.sulfur.dioxide",model)
```

```
## 
## free.sulfur.dioxide effect (probability) for 0
## free.sulfur.dioxide
##          1         20         30         50         70 
## 0.01297336 0.01253314 0.01230741 0.01186791 0.01144394 
## 
## free.sulfur.dioxide effect (probability) for 1
## free.sulfur.dioxide
##         1        20        30        50        70 
## 0.9227561 0.9210611 0.9201367 0.9182204 0.9162128 
## 
## free.sulfur.dioxide effect (probability) for 2
## free.sulfur.dioxide
##          1         20         30         50         70 
## 0.06427051 0.06640574 0.06755585 0.06991166 0.07234326
```

```r
plot(Effect(focal.predictors = "free.sulfur.dioxide",model))
```

<img src="Ordinal-345678_files/figure-html/unnamed-chunk-25-1.png" width="672" />

### Total Sulfur Dioxide

```r
Effect(focal.predictors = "total.sulfur.dioxide",model)
```

```
## 
## total.sulfur.dioxide effect (probability) for 0
## total.sulfur.dioxide
##          6         80        100        200        300 
## 0.01067861 0.01457942 0.01585581 0.02407693 0.03640297 
## 
## total.sulfur.dioxide effect (probability) for 1
## total.sulfur.dioxide
##         6        80       100       200       300 
## 0.9121382 0.9279106 0.9310830 0.9406217 0.9402578 
## 
## total.sulfur.dioxide effect (probability) for 2
## total.sulfur.dioxide
##          6         80        100        200        300 
## 0.07718324 0.05750995 0.05306119 0.03530132 0.02333926
```

```r
plot(Effect(focal.predictors = "total.sulfur.dioxide",model))
```

<img src="Ordinal-345678_files/figure-html/unnamed-chunk-26-1.png" width="672" />

### Density

```r
Effect(focal.predictors = "density",model)
```

```
## 
## density effect (probability) for 0
## density
##      0.9901      0.9935      0.9969           1       1.004 
## 0.072720490 0.030021754 0.012067829 0.005205832 0.001749727 
## 
## density effect (probability) for 1
## density
##    0.9901    0.9935    0.9969         1     1.004 
## 0.9158988 0.9416366 0.9191119 0.8476614 0.6582922 
## 
## density effect (probability) for 2
## density
##     0.9901     0.9935     0.9969          1      1.004 
## 0.01138067 0.02834160 0.06882027 0.14713276 0.33995810
```

```r
plot(Effect(focal.predictors = "density",model))
```

<img src="Ordinal-345678_files/figure-html/unnamed-chunk-27-1.png" width="672" />

### pH

```r
Effect(focal.predictors = "pH",model)
```

```
## 
## pH effect (probability) for 0
## pH
##         2.7         3.1         3.4         3.7           4 
## 0.001014512 0.005305499 0.018168836 0.060328184 0.182164511 
## 
## pH effect (probability) for 1
## pH
##       2.7       3.1       3.4       3.7         4 
## 0.5283748 0.8499380 0.9353146 0.9258050 0.8137988 
## 
## pH effect (probability) for 2
## pH
##         2.7         3.1         3.4         3.7           4 
## 0.470610725 0.144756509 0.046516572 0.013866804 0.004036736
```

```r
plot(Effect(focal.predictors = "fixed.acidity",model))
```

<img src="Ordinal-345678_files/figure-html/unnamed-chunk-28-1.png" width="672" />

### Sulphates

```r
Effect(focal.predictors = "sulphates",model)
```

```
## 
## sulphates effect (probability) for 0
## sulphates
##        0.33        0.75         1.2         1.6           2 
## 0.022393042 0.010742981 0.004857529 0.002392169 0.001176583 
## 
## sulphates effect (probability) for 1
## sulphates
##      0.33      0.75       1.2       1.6         2 
## 0.9396887 0.9125054 0.8390598 0.7240936 0.5649491 
## 
## sulphates effect (probability) for 2
## sulphates
##       0.33       0.75        1.2        1.6          2 
## 0.03791823 0.07675165 0.15608264 0.27351427 0.43387434
```

```r
plot(Effect(focal.predictors = "sulphates",model))
```

<img src="Ordinal-345678_files/figure-html/unnamed-chunk-29-1.png" width="672" />

### Alcohol

```r
Effect(focal.predictors = "alcohol",model)
```

```
## 
## alcohol effect (probability) for 0
## alcohol
##          8.4           10           12           13           15 
## 1.100732e-01 2.048170e-02 2.261531e-03 7.457238e-04 8.089082e-05 
## 
## alcohol effect (probability) for 1
## alcohol
##        8.4         10         12         13         15 
## 0.88268075 0.93813031 0.71289786 0.45180143 0.08215834 
## 
## alcohol effect (probability) for 2
## alcohol
##         8.4          10          12          13          15 
## 0.007246012 0.041387990 0.284840614 0.547452842 0.917760774
```

```r
plot(Effect(focal.predictors = "alcohol",model))
```

<img src="Ordinal-345678_files/figure-html/unnamed-chunk-30-1.png" width="672" />












**** SECTIONS OF THE CODE BELOW IS TEMPORARILY DISABLED BY HASH TAGS ****

## Assumption #5: Logit Linearity *** MODIFY TO FIT NEW ANALYSIS ***

### create probabilities

```r
#prob <- predict(model, type="probs")
#head(prob)
```

### define predictors

```r
#wine1 <- wine_wrangled %>% dplyr::select_if(is.numeric)
#predictors <- colnames(wine1)
```

### 

```r
#wine1 <- wine1 %>% mutate(logit=log(prob/(1-prob))) %>%
#gather(key= "predictors", value="predictor.value", -logit)
```

## Assumption #5: Absence of Influential Outliers *** MODIFY TO FIT NEW ANALYSIS ***


```r
#infl <- influence.measures(model)
#summary(infl)
```
### Results indicate no evidence of **influential** outliers

Box Plot Method

### all box plots

```r
boxplot(wine_wrangled)
```

<img src="Ordinal-345678_files/figure-html/unnamed-chunk-35-1.png" width="672" />

```r
ggplot(stack(wine_wrangled), aes(x = ind, y = values))+
  geom_boxplot(fill='rosybrown', color="darkred") +
  coord_flip()
```

```
## Warning in stack.data.frame(wine_wrangled): non-vector columns will be ignored
```

<img src="Ordinal-345678_files/figure-html/unnamed-chunk-35-2.png" width="672" />

All variables have outliers.

*********************** MOVING FORWARD **************************

# Correcting for Violation of Assumptions

## Corrections for Violation of Multicollinearity



## Corrections for Violation of Logit Linearity



