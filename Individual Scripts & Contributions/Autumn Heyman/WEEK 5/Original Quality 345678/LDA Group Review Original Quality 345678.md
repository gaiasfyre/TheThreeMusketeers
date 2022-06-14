# Linear Discriminant Analysis: Group Review
# Quality: 345678, Outliers: Original

## Set Working Directory



## Import Packages


```R
library("tidyr")
library("corrplot")
library("MASS")
library("ggplot2")
library("tibble")
library("reshape2")
```

## Load in Data


```R
wine <- read.csv("WineQT.csv")
head(wine)
```


<table class="dataframe">
<caption>A data.frame: 6 × 13</caption>
<thead>
	<tr><th></th><th scope=col>fixed.acidity</th><th scope=col>volatile.acidity</th><th scope=col>citric.acid</th><th scope=col>residual.sugar</th><th scope=col>chlorides</th><th scope=col>free.sulfur.dioxide</th><th scope=col>total.sulfur.dioxide</th><th scope=col>density</th><th scope=col>pH</th><th scope=col>sulphates</th><th scope=col>alcohol</th><th scope=col>quality</th><th scope=col>Id</th></tr>
	<tr><th></th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><th scope=row>1</th><td> 7.4</td><td>0.70</td><td>0.00</td><td>1.9</td><td>0.076</td><td>11</td><td>34</td><td>0.9978</td><td>3.51</td><td>0.56</td><td>9.4</td><td>5</td><td>0</td></tr>
	<tr><th scope=row>2</th><td> 7.8</td><td>0.88</td><td>0.00</td><td>2.6</td><td>0.098</td><td>25</td><td>67</td><td>0.9968</td><td>3.20</td><td>0.68</td><td>9.8</td><td>5</td><td>1</td></tr>
	<tr><th scope=row>3</th><td> 7.8</td><td>0.76</td><td>0.04</td><td>2.3</td><td>0.092</td><td>15</td><td>54</td><td>0.9970</td><td>3.26</td><td>0.65</td><td>9.8</td><td>5</td><td>2</td></tr>
	<tr><th scope=row>4</th><td>11.2</td><td>0.28</td><td>0.56</td><td>1.9</td><td>0.075</td><td>17</td><td>60</td><td>0.9980</td><td>3.16</td><td>0.58</td><td>9.8</td><td>6</td><td>3</td></tr>
	<tr><th scope=row>5</th><td> 7.4</td><td>0.70</td><td>0.00</td><td>1.9</td><td>0.076</td><td>11</td><td>34</td><td>0.9978</td><td>3.51</td><td>0.56</td><td>9.4</td><td>5</td><td>4</td></tr>
	<tr><th scope=row>6</th><td> 7.4</td><td>0.66</td><td>0.00</td><td>1.8</td><td>0.075</td><td>13</td><td>40</td><td>0.9978</td><td>3.51</td><td>0.56</td><td>9.4</td><td>5</td><td>5</td></tr>
</tbody>
</table>



### View Structure of the Dataset


```R
str(wine)
```

    'data.frame':	1143 obs. of  13 variables:
     $ fixed.acidity       : num  7.4 7.8 7.8 11.2 7.4 7.4 7.9 7.3 7.8 6.7 ...
     $ volatile.acidity    : num  0.7 0.88 0.76 0.28 0.7 0.66 0.6 0.65 0.58 0.58 ...
     $ citric.acid         : num  0 0 0.04 0.56 0 0 0.06 0 0.02 0.08 ...
     $ residual.sugar      : num  1.9 2.6 2.3 1.9 1.9 1.8 1.6 1.2 2 1.8 ...
     $ chlorides           : num  0.076 0.098 0.092 0.075 0.076 0.075 0.069 0.065 0.073 0.097 ...
     $ free.sulfur.dioxide : num  11 25 15 17 11 13 15 15 9 15 ...
     $ total.sulfur.dioxide: num  34 67 54 60 34 40 59 21 18 65 ...
     $ density             : num  0.998 0.997 0.997 0.998 0.998 ...
     $ pH                  : num  3.51 3.2 3.26 3.16 3.51 3.51 3.3 3.39 3.36 3.28 ...
     $ sulphates           : num  0.56 0.68 0.65 0.58 0.56 0.56 0.46 0.47 0.57 0.54 ...
     $ alcohol             : num  9.4 9.8 9.8 9.8 9.4 9.4 9.4 10 9.5 9.2 ...
     $ quality             : int  5 5 5 6 5 5 5 7 7 5 ...
     $ Id                  : int  0 1 2 3 4 5 6 7 8 10 ...


## Data Wrangling

### Remove Unnecessary Columns


```R
wine_wrangled <- wine[, c(2,5,8,9,12)]
head(wine_wrangled)
```


<table class="dataframe">
<caption>A data.frame: 6 × 5</caption>
<thead>
	<tr><th></th><th scope=col>volatile.acidity</th><th scope=col>chlorides</th><th scope=col>density</th><th scope=col>pH</th><th scope=col>quality</th></tr>
	<tr><th></th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><th scope=row>1</th><td>0.70</td><td>0.076</td><td>0.9978</td><td>3.51</td><td>5</td></tr>
	<tr><th scope=row>2</th><td>0.88</td><td>0.098</td><td>0.9968</td><td>3.20</td><td>5</td></tr>
	<tr><th scope=row>3</th><td>0.76</td><td>0.092</td><td>0.9970</td><td>3.26</td><td>5</td></tr>
	<tr><th scope=row>4</th><td>0.28</td><td>0.075</td><td>0.9980</td><td>3.16</td><td>6</td></tr>
	<tr><th scope=row>5</th><td>0.70</td><td>0.076</td><td>0.9978</td><td>3.51</td><td>5</td></tr>
	<tr><th scope=row>6</th><td>0.66</td><td>0.075</td><td>0.9978</td><td>3.51</td><td>5</td></tr>
</tbody>
</table>



### Summarizing the Dataset


```R
summary(wine_wrangled)
```


     volatile.acidity   chlorides          density             pH       
     Min.   :0.1200   Min.   :0.01200   Min.   :0.9901   Min.   :2.740  
     1st Qu.:0.3925   1st Qu.:0.07000   1st Qu.:0.9956   1st Qu.:3.205  
     Median :0.5200   Median :0.07900   Median :0.9967   Median :3.310  
     Mean   :0.5313   Mean   :0.08693   Mean   :0.9967   Mean   :3.311  
     3rd Qu.:0.6400   3rd Qu.:0.09000   3rd Qu.:0.9978   3rd Qu.:3.400  
     Max.   :1.5800   Max.   :0.61100   Max.   :1.0037   Max.   :4.010  
        quality     
     Min.   :3.000  
     1st Qu.:5.000  
     Median :6.000  
     Mean   :5.657  
     3rd Qu.:6.000  
     Max.   :8.000  


## Scale the Data: Scale Each Predictor Value


```R
wine_wrangled[1:4] <- scale(wine_wrangled[1:4])
```

### Find the Mean of Each Predictor Value:  Mean Should == 0
### Find the Standard Deviation of Each Predictor Value:  Standard Deviation Should == 1


```R
apply(wine_wrangled[1:4], 2, mean)
apply(wine_wrangled[1:4], 2, sd)
```


<style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>volatile.acidity</dt><dd>-1.65284676650836e-16</dd><dt>chlorides</dt><dd>-3.6624216588394e-17</dd><dt>density</dt><dd>1.03974798629694e-16</dd><dt>pH</dt><dd>-8.34781225565631e-16</dd></dl>




<style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>volatile.acidity</dt><dd>1</dd><dt>chlorides</dt><dd>1</dd><dt>density</dt><dd>1</dd><dt>pH</dt><dd>1</dd></dl>



## Create Training and Test Samples

### Making this Example Reproducible


```R
set.seed(1)
```

### Using 70% of Dataset as Training Set, Remaining 30% as Testing Set


```R
sample <- sample(c(TRUE, FALSE), nrow(wine_wrangled), replace=TRUE, prob=c(0.7,0.3))
train <- wine_wrangled[sample, ]
test <- wine_wrangled[!sample, ]
```

## Fit LDA Model


```R
model <- lda(quality~., data=train)
```

### View Model Output


```R
model
```


    Call:
    lda(quality ~ ., data = train)
    
    Prior probabilities of groups:
          3       4       5       6       7       8 
    0.00375 0.03500 0.41000 0.40375 0.13875 0.00875 
    
    Group means:
      volatile.acidity   chlorides     density          pH
    3        1.6162274  0.57969628  0.75127534 -0.13413972
    4        0.9389212 -0.17689181 -0.07423699  0.60219472
    5        0.3060258  0.06676452  0.17778146 -0.05006982
    6       -0.1265455 -0.02660845 -0.06272600  0.08146231
    7       -0.7517112 -0.20746900 -0.24422054 -0.17324330
    8       -0.2619386 -0.41867761 -0.73859231 -0.56271828
    
    Coefficients of linear discriminants:
                             LD1         LD2        LD3         LD4
    volatile.acidity -0.98724052  0.18916601 -0.4664482 -0.07840115
    chlorides        -0.16585074 -0.05781727  0.6957015 -0.99078930
    density          -0.33540027 -0.01726327  0.7285571  0.74287367
    pH               -0.09262148 -1.06106451  0.4185101  0.07261674
    
    Proportion of trace:
       LD1    LD2    LD3    LD4 
    0.8629 0.0804 0.0517 0.0050 


Prior probabilities of group: The proportions of each 'quality' rating in the training set.

Group means: The mean values for each predictor variable for each 'quality' rating.

Coefficients of linear discriminants: Linear combination of predictor variables used to form the decision rule of the LDA model.

### View Model Info


```R
model$counts
model$prior
model$scaling
model$svd
```


<style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>3</dt><dd>3</dd><dt>4</dt><dd>28</dd><dt>5</dt><dd>328</dd><dt>6</dt><dd>323</dd><dt>7</dt><dd>111</dd><dt>8</dt><dd>7</dd></dl>




<style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>3</dt><dd>0.00375</dd><dt>4</dt><dd>0.035</dd><dt>5</dt><dd>0.41</dd><dt>6</dt><dd>0.40375</dd><dt>7</dt><dd>0.13875</dd><dt>8</dt><dd>0.00875</dd></dl>




<table class="dataframe">
<caption>A matrix: 4 × 4 of type dbl</caption>
<thead>
	<tr><th></th><th scope=col>LD1</th><th scope=col>LD2</th><th scope=col>LD3</th><th scope=col>LD4</th></tr>
</thead>
<tbody>
	<tr><th scope=row>volatile.acidity</th><td>-0.98724052</td><td> 0.18916601</td><td>-0.4664482</td><td>-0.07840115</td></tr>
	<tr><th scope=row>chlorides</th><td>-0.16585074</td><td>-0.05781727</td><td> 0.6957015</td><td>-0.99078930</td></tr>
	<tr><th scope=row>density</th><td>-0.33540027</td><td>-0.01726327</td><td> 0.7285571</td><td> 0.74287367</td></tr>
	<tr><th scope=row>pH</th><td>-0.09262148</td><td>-1.06106451</td><td> 0.4185101</td><td> 0.07261674</td></tr>
</tbody>
</table>




<style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>5.89906816149435</li><li>1.80075333761168</li><li>1.44425194598688</li><li>0.44841591012837</li></ol>



### Store Model Means


```R
modelmeans <- model$means
```

### Convert Matrix to a Data Frame


```R
modelmeans <- as.data.frame(modelmeans)
str(modelmeans)
```

    'data.frame':	6 obs. of  4 variables:
     $ volatile.acidity: num  1.616 0.939 0.306 -0.127 -0.752 ...
     $ chlorides       : num  0.5797 -0.1769 0.0668 -0.0266 -0.2075 ...
     $ density         : num  0.7513 -0.0742 0.1778 -0.0627 -0.2442 ...
     $ pH              : num  -0.1341 0.6022 -0.0501 0.0815 -0.1732 ...


### Convert Row Labels to Columns


```R
modelmeans <- tibble::rownames_to_column(modelmeans, "quality")
```

## Plot Means

### Melt Data Frame


```R
mmodelmeans <- melt(modelmeans, id.vars="quality")
```

### All Variables on the Same Plot


```R
ggplot(mmodelmeans, aes(quality, value, group=variable, col=variable)) + ggtitle("All Variables vs. Quality")+ geom_line() + geom_point() + stat_smooth()
```

    `geom_smooth()` using method = 'loess' and formula 'y ~ x'
    
    Warning message in simpleLoess(y, x, w, span, degree = degree, parametric = parametric, :
    “Chernobyl! trL>n 6”
    Warning message in simpleLoess(y, x, w, span, degree = degree, parametric = parametric, :
    “Chernobyl! trL>n 6”
    Warning message in sqrt(sum.squares/one.delta):
    “NaNs produced”
    Warning message in stats::qt(level/2 + 0.5, pred$df):
    “NaNs produced”
    Warning message in simpleLoess(y, x, w, span, degree = degree, parametric = parametric, :
    “Chernobyl! trL>n 6”
    Warning message in simpleLoess(y, x, w, span, degree = degree, parametric = parametric, :
    “Chernobyl! trL>n 6”
    Warning message in sqrt(sum.squares/one.delta):
    “NaNs produced”
    Warning message in stats::qt(level/2 + 0.5, pred$df):
    “NaNs produced”
    Warning message in simpleLoess(y, x, w, span, degree = degree, parametric = parametric, :
    “Chernobyl! trL>n 6”
    Warning message in simpleLoess(y, x, w, span, degree = degree, parametric = parametric, :
    “Chernobyl! trL>n 6”
    Warning message in sqrt(sum.squares/one.delta):
    “NaNs produced”
    Warning message in stats::qt(level/2 + 0.5, pred$df):
    “NaNs produced”
    Warning message in simpleLoess(y, x, w, span, degree = degree, parametric = parametric, :
    “Chernobyl! trL>n 6”
    Warning message in simpleLoess(y, x, w, span, degree = degree, parametric = parametric, :
    “Chernobyl! trL>n 6”
    Warning message in sqrt(sum.squares/one.delta):
    “NaNs produced”
    Warning message in stats::qt(level/2 + 0.5, pred$df):
    “NaNs produced”
    Warning message in max(ids, na.rm = TRUE):
    “no non-missing arguments to max; returning -Inf”
    Warning message in max(ids, na.rm = TRUE):
    “no non-missing arguments to max; returning -Inf”
    Warning message in max(ids, na.rm = TRUE):
    “no non-missing arguments to max; returning -Inf”
    Warning message in max(ids, na.rm = TRUE):
    “no non-missing arguments to max; returning -Inf”



    
![png](output_40_1.png)
    


### Variables on Separate Plots


```R
ggplot(mmodelmeans, aes(quality, value, group=variable, col=variable)) + ggtitle ("Separately Plotted Variables vs. Quality")+
  geom_point() + 
  stat_smooth() +
  facet_wrap(~variable)
```

    `geom_smooth()` using method = 'loess' and formula 'y ~ x'
    
    Warning message in simpleLoess(y, x, w, span, degree = degree, parametric = parametric, :
    “Chernobyl! trL>n 6”
    Warning message in simpleLoess(y, x, w, span, degree = degree, parametric = parametric, :
    “Chernobyl! trL>n 6”
    Warning message in sqrt(sum.squares/one.delta):
    “NaNs produced”
    Warning message in stats::qt(level/2 + 0.5, pred$df):
    “NaNs produced”
    Warning message in simpleLoess(y, x, w, span, degree = degree, parametric = parametric, :
    “Chernobyl! trL>n 6”
    Warning message in simpleLoess(y, x, w, span, degree = degree, parametric = parametric, :
    “Chernobyl! trL>n 6”
    Warning message in sqrt(sum.squares/one.delta):
    “NaNs produced”
    Warning message in stats::qt(level/2 + 0.5, pred$df):
    “NaNs produced”
    Warning message in simpleLoess(y, x, w, span, degree = degree, parametric = parametric, :
    “Chernobyl! trL>n 6”
    Warning message in simpleLoess(y, x, w, span, degree = degree, parametric = parametric, :
    “Chernobyl! trL>n 6”
    Warning message in sqrt(sum.squares/one.delta):
    “NaNs produced”
    Warning message in stats::qt(level/2 + 0.5, pred$df):
    “NaNs produced”
    Warning message in simpleLoess(y, x, w, span, degree = degree, parametric = parametric, :
    “Chernobyl! trL>n 6”
    Warning message in simpleLoess(y, x, w, span, degree = degree, parametric = parametric, :
    “Chernobyl! trL>n 6”
    Warning message in sqrt(sum.squares/one.delta):
    “NaNs produced”
    Warning message in stats::qt(level/2 + 0.5, pred$df):
    “NaNs produced”
    Warning message in max(ids, na.rm = TRUE):
    “no non-missing arguments to max; returning -Inf”
    Warning message in max(ids, na.rm = TRUE):
    “no non-missing arguments to max; returning -Inf”
    Warning message in max(ids, na.rm = TRUE):
    “no non-missing arguments to max; returning -Inf”
    Warning message in max(ids, na.rm = TRUE):
    “no non-missing arguments to max; returning -Inf”



    
![png](output_42_1.png)
    


## Assumptions Testing

### Misclassification Rate


```R
lda.pred = (test$class)
```


```R
lda.error = mean(default$default != lda.pred)
```


    Error in mean(default$default != lda.pred): object 'default' not found
    Traceback:


    1. mean(default$default != lda.pred)



```R
lda.error
```


    Error in eval(expr, envir, enclos): object 'lda.error' not found
    Traceback:



### Confusion Matrix


```R
LDA_model = lda.cm
```


    Error in eval(expr, envir, enclos): object 'lda.cm' not found
    Traceback:




```R
LDA_model
```


    Error in eval(expr, envir, enclos): object 'LDA_model' not found
    Traceback:



## LDA model: Make Predictions on Test Data


```R
predicted <- predict(model, test)
names(predicted)
```


<style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>'class'</li><li>'posterior'</li><li>'x'</li></ol>



### View Predicted Class (First Six Observations)


```R
head(predicted$class)
```


<style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>6</li><li>5</li><li>5</li><li>6</li><li>5</li><li>6</li></ol>

<details>
	<summary style=display:list-item;cursor:pointer>
		<strong>Levels</strong>:
	</summary>
	<style>
	.list-inline {list-style: none; margin:0; padding: 0}
	.list-inline>li {display: inline-block}
	.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
	</style>
	<ol class=list-inline><li>'3'</li><li>'4'</li><li>'5'</li><li>'6'</li><li>'7'</li><li>'8'</li></ol>
</details>


### View Predicted Posterior Probability (First Six Observations)¶


```R
head(predicted$posterior)
```


<table class="dataframe">
<caption>A matrix: 6 × 6 of type dbl</caption>
<thead>
	<tr><th></th><th scope=col>3</th><th scope=col>4</th><th scope=col>5</th><th scope=col>6</th><th scope=col>7</th><th scope=col>8</th></tr>
</thead>
<tbody>
	<tr><th scope=row>4</th><td>5.447785e-05</td><td>0.003475051</td><td>0.2565498</td><td>0.4301147</td><td>0.30296603</td><td>0.0068399848</td></tr>
	<tr><th scope=row>6</th><td>1.987033e-03</td><td>0.073402627</td><td>0.4912910</td><td>0.3942343</td><td>0.03847096</td><td>0.0006141343</td></tr>
	<tr><th scope=row>7</th><td>8.915412e-04</td><td>0.031148641</td><td>0.4676033</td><td>0.4060456</td><td>0.08754223</td><td>0.0067686941</td></tr>
	<tr><th scope=row>15</th><td>1.416444e-04</td><td>0.017983603</td><td>0.3067431</td><td>0.5451008</td><td>0.12954337</td><td>0.0004875773</td></tr>
	<tr><th scope=row>17</th><td>4.426605e-04</td><td>0.011806595</td><td>0.4147467</td><td>0.4146365</td><td>0.14686400</td><td>0.0115036090</td></tr>
	<tr><th scope=row>18</th><td>1.175394e-04</td><td>0.013746531</td><td>0.2949952</td><td>0.5243992</td><td>0.16488502</td><td>0.0018565266</td></tr>
</tbody>
</table>



### View Linear Discriminants (First Six Observations)¶


```R
head(predicted$x)
```


<table class="dataframe">
<caption>A matrix: 6 × 4 of type dbl</caption>
<thead>
	<tr><th></th><th scope=col>LD1</th><th scope=col>LD2</th><th scope=col>LD3</th><th scope=col>LD4</th></tr>
</thead>
<tbody>
	<tr><th scope=row>2</th><td>-1.9778909</td><td> 0.8537230</td><td>-1.1994157</td><td>-0.6494332</td></tr>
	<tr><th scope=row>6</th><td>-1.0784419</td><td>-0.9136991</td><td> 0.6410399</td><td> 0.9038224</td></tr>
	<tr><th scope=row>8</th><td>-0.2825177</td><td>-0.6473146</td><td>-1.0742852</td><td>-0.5272994</td></tr>
	<tr><th scope=row>10</th><td>-0.1433853</td><td> 0.1363726</td><td>-0.4166806</td><td>-0.6304414</td></tr>
	<tr><th scope=row>11</th><td>-0.2406689</td><td>-1.7901037</td><td> 0.0799071</td><td>-0.6803499</td></tr>
	<tr><th scope=row>12</th><td>-0.6399323</td><td> 0.4674990</td><td> 0.1952364</td><td>-0.3206199</td></tr>
</tbody>
</table>



## Accuracy of the Model


```R
mean(predicted$class==test$quality)
```


0.559766763848397


## Visualize the Results

### Convert Quality to a Factor


```R
wine_wrangled$quality = factor(wine_wrangled$quality, levels = c("3", "4", "5", "6", "7", "8"), ordered = TRUE)
str(wine_wrangled)
```

    'data.frame':	1143 obs. of  5 variables:
     $ volatile.acidity: num  0.939 1.941 1.273 -1.399 0.939 ...
     $ chlorides       : num  -0.231 0.234 0.107 -0.252 -0.231 ...
     $ density         : num  0.5556 0.0361 0.14 0.6595 0.5556 ...
     $ pH              : num  1.27 -0.709 -0.326 -0.964 1.27 ...
     $ quality         : Ord.factor w/ 6 levels "3"<"4"<"5"<"6"<..: 3 3 3 4 3 3 3 5 5 3 ...


### Rebuild Model with 'Quality' as a Factor


```R
set.seed(1)
```


```R
sample <- sample(c(TRUE, FALSE), nrow(wine_wrangled), replace=TRUE, prob=c(0.7,0.3))
train <- wine_wrangled[sample, ]
test <- wine_wrangled[!sample, ]
```


```R
LDAmodel <- lda(quality~., data=train)
```

### Define Data to Plot


```R
lda_plot <- cbind(train, predict(LDAmodel)$x)
```

### Create Plot


```R
ggplot(lda_plot, aes(LD1, LD2, LD3, LD4)) +
  geom_point(aes(color = quality))
```

    Warning message:
    “Duplicated aesthetics after name standardisation: ”



    
![png](output_71_1.png)
    


## Identify Outliers

### Density


```R
bp.density <- ggplot(wine_wrangled, aes(x = "", y = density)) + geom_boxplot() + xlab("")
bp.density
```


    
![png](output_74_0.png)
    



```R
boxplot.stats(wine_wrangled$density)
```


<dl>
	<dt>$stats</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>-2.35337829387856</li><li>-0.602789991249114</li><li>-0.0261867224306141</li><li>0.578986978626774</li><li>2.2178908643228</li></ol>
</dd>
	<dt>$n</dt>
		<dd>1143</dd>
	<dt>$conf</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>-0.0814160167405449</li><li>0.0290425718793166</li></ol>
</dd>
	<dt>$out</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>-2.66505573648317</li><li>-2.66505573648317</li><li>2.42567582605932</li><li>2.47762206649342</li><li>2.47762206649342</li><li>2.63346078779572</li><li>-2.87284069821963</li><li>2.84124574953213</li><li>2.84124574953213</li><li>2.42567582605932</li><li>2.42567582605932</li><li>3.36070815387325</li><li>3.04903071126853</li><li>2.42567582605932</li><li>3.33473503365614</li><li>3.33473503365614</li><li>2.78929950909802</li><li>2.78929950909802</li><li>-2.61310949604907</li><li>3.04903071126853</li><li>-2.40532453431267</li><li>-2.69622348074367</li><li>-3.16373964465063</li><li>-3.16373964465063</li><li>3.1996748085275</li><li>-2.65466648839634</li><li>-3.45983321512496</li><li>-3.3923031025607</li><li>-2.68063960861339</li><li>-3.05984716378243</li><li>-2.50402239113742</li><li>3.61524473200031</li><li>2.95552747848722</li><li>-2.55077400752814</li><li>2.95552747848722</li><li>-2.55077400752814</li></ol>
</dd>
</dl>



#### Density Outlier Count: 36

### Chlorides 


```R
bp.chlorides <- ggplot(wine_wrangled, aes(x = "", y = chlorides)) + geom_boxplot() + xlab("")
bp.chlorides
```


    
![png](output_78_0.png)
    



```R
boxplot.stats(wine_wrangled$chlorides)
```


<dl>
	<dt>$stats</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>-0.971762646483227</li><li>-0.358231162454741</li><li>-0.167824839825211</li><li>0.0648939989442141</li><li>0.6784254829727</li></ol>
</dd>
	<dt>$n</dt>
		<dd>1143</dd>
	<dt>$conf</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>-0.187599217448507</li><li>-0.148050462201916</li></ol>
</dd>
	<dt>$out</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>5.3751147745011</li><li>5.18470845187158</li><li>8.04080329131452</li><li>1.92664470909962</li><li>11.0661481953171</li><li>3.87302045153481</li><li>-1.01407516262312</li><li>5.29048974222131</li><li>3.72492664504518</li><li>11.087304453387</li><li>5.73477116169022</li><li>2.66711374154779</li><li>2.68826999961774</li><li>0.720737999112595</li><li>0.86883180560223</li><li>0.699581741042647</li><li>0.741894257182543</li><li>0.741894257182543</li><li>0.720737999112595</li><li>0.847675547532282</li><li>1.37658199928098</li><li>0.805363031392386</li><li>0.741894257182543</li><li>2.39208238663847</li><li>2.94214509645711</li><li>3.44989529013586</li><li>0.784206773322438</li><li>2.85752006417732</li><li>-1.01407516262312</li><li>1.48236328963071</li><li>7.08877167816688</li><li>-1.11985645297286</li><li>6.3483026457187</li><li>6.94067787167724</li><li>1.48236328963071</li><li>3.25948896750633</li><li>2.18051980593899</li><li>0.953456837882021</li><li>0.826519289462334</li><li>-1.03523142069307</li><li>1.6516133541903</li><li>1.27080070893124</li><li>-1.58529413051171</li><li>-1.58529413051171</li><li>2.26514483821878</li><li>0.953456837882021</li><li>1.56698832191051</li><li>0.699581741042647</li><li>0.699581741042647</li><li>0.763050515252491</li><li>0.763050515252491</li><li>6.91952161360729</li><li>1.77855090260998</li><li>1.92664470909962</li><li>1.67276961226024</li><li>1.03808187016181</li><li>0.953456837882021</li><li>0.953456837882021</li><li>0.763050515252491</li><li>0.763050515252491</li><li>6.68680277483787</li><li>1.05923812823176</li><li>6.91952161360729</li><li>1.67276961226024</li><li>1.71508212840014</li><li>6.94067787167724</li><li>1.39773825735092</li><li>6.94067787167724</li><li>0.763050515252491</li><li>2.68826999961774</li><li>1.73623838647009</li><li>2.49786367698821</li><li>2.49786367698821</li><li>-1.01407516262312</li><li>3.13255141908664</li><li>3.0267701287369</li><li>-1.03523142069307</li></ol>
</dd>
</dl>



#### Chlorides Outlier Count: 77

### Volatile Acidity


```R
bp.volatile.acidity <- ggplot(wine_wrangled, aes(x = "", y = volatile.acidity)) + geom_boxplot() + xlab("")
bp.volatile.acidity
```


    
![png](output_82_0.png)
    



```R
boxplot.stats(wine_wrangled$volatile.acidity)
```


<dl>
	<dt>$stats</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>-2.28988070498523</li><li>-0.772900488718453</li><li>-0.0631207545018899</li><li>0.604907230643111</li><li>2.63682568545915</li></ol>
</dd>
	<dt>$n</dt>
		<dd>1143</dd>
	<dt>$conf</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>-0.127511371322851</li><li>0.00126986231907082</li></ol>
</dd>
	<dt>$out</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>2.72032918360228</li><li>2.9986741774127</li><li>4.44606814522686</li><li>4.44606814522686</li><li>2.83166718112645</li><li>3.11001217493686</li><li>2.83166718112645</li><li>2.72032918360228</li><li>2.8038326817454</li><li>2.74816368298332</li><li>2.72032918360228</li><li>5.83779311427895</li><li>3.61103316379561</li><li>2.83166718112645</li></ol>
</dd>
</dl>



#### Volatile Acidity Outliers Count: 14

### pH


```R
bp.ph <- ggplot(wine_wrangled, aes(x = "", y = pH)) + geom_boxplot() + xlab("")
bp.ph
```


    
![png](output_86_0.png)
    



```R
boxplot.stats(wine_wrangled$pH)
```


<dl>
	<dt>$stats</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>-2.495881146627</li><li>-0.676701939772508</li><li>-0.00647802145769478</li><li>0.567999622812145</li><li>2.4190942543483</li></ol>
</dd>
	<dt>$n</dt>
		<dd>1143</dd>
	<dt>$conf</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>-0.0646480405732629</li><li>0.0516919976578733</li></ol>
</dd>
	<dt>$out</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>3.75954209097792</li><li>2.80207935052819</li><li>-3.64483643516668</li><li>-2.75120454408027</li><li>-2.8788662428069</li><li>2.73824850116488</li><li>2.61058680243825</li><li>-2.68737369471695</li><li>-2.68737369471695</li><li>3.75954209097792</li><li>2.54675595307493</li><li>-2.68737369471695</li><li>2.99357189861814</li><li>2.48292510371161</li><li>2.99357189861814</li><li>4.4616814339744</li><li>-2.62354284535364</li><li>4.4616814339744</li><li>-2.75120454408027</li><li>2.61058680243825</li></ol>
</dd>
</dl>



#### pH Outliers Count: 20

## Graphing the Relationship of the Variables with Quality


```R
aggregate(x = wine_wrangled$volatile.acidity, by = list(wine_wrangled$quality), FUN = mean) 
```


<table class="dataframe">
<caption>A data.frame: 6 × 2</caption>
<thead>
	<tr><th scope=col>Group.1</th><th scope=col>x</th></tr>
	<tr><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>3</td><td> 2.0383839</td></tr>
	<tr><td>4</td><td> 0.9389212</td></tr>
	<tr><td>5</td><td> 0.3002837</td></tr>
	<tr><td>6</td><td>-0.1468652</td></tr>
	<tr><td>7</td><td>-0.7663798</td></tr>
	<tr><td>8</td><td>-0.6754797</td></tr>
</tbody>
</table>




```R
boxplot(wine_wrangled)
```


    
![png](output_91_0.png)
    



```R
ggplot(stack(wine_wrangled), aes(x = ind, y = values))+
  geom_boxplot(fill='rosybrown', color="darkred") +
  coord_flip()
```


    
![png](output_92_0.png)
    


## Remove Outliers

### Chlorides


```R
chloride.Q1 <- quantile(wine_wrangled$chlorides, .25)
chloride.Q3 <- quantile(wine_wrangled$chlorides, .75)
chloride.IQR <- IQR(wine_wrangled$chlorides)
```


```R
no.chlorides.outliers <- subset(wine_wrangled, wine_wrangled$chlorides > (chloride.Q1 - 1.5*chloride.IQR) 
                                & wine_wrangled$chlorides < (chloride.Q3 + 1.5*chloride.IQR))
```


```R
boxplot(no.chlorides.outliers)
```


    
![png](output_97_0.png)
    


### Recheck Outlier Count

#### pH


```R
boxplot.stats(no.chlorides.outliers$pH)
```


<dl>
	<dt>$stats</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>-2.43205029726369</li><li>-0.644786515090851</li><li>0.0573528279056195</li><li>0.567999622812145</li><li>2.35526340498498</li></ol>
</dd>
	<dt>$n</dt>
		<dd>1066</dd>
	<dt>$conf</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>-0.00133698282525775</li><li>0.116042638636497</li></ol>
</dd>
	<dt>$out</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>3.75954209097792</li><li>2.80207935052819</li><li>2.4190942543483</li><li>2.4190942543483</li><li>-2.75120454408027</li><li>-2.8788662428069</li><li>2.73824850116488</li><li>-2.495881146627</li><li>-2.495881146627</li><li>2.61058680243825</li><li>-2.68737369471695</li><li>-2.68737369471695</li><li>-2.495881146627</li><li>2.54675595307493</li><li>2.4190942543483</li><li>-2.68737369471695</li><li>2.99357189861814</li><li>2.48292510371161</li><li>2.99357189861814</li><li>4.4616814339744</li><li>4.4616814339744</li><li>-2.75120454408027</li><li>2.61058680243825</li></ol>
</dd>
</dl>



#### Volatile Acidity


```R
boxplot.stats(no.chlorides.outliers$volatile.acidity)
```


<dl>
	<dt>$stats</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>-2.28988070498523</li><li>-0.786817738408974</li><li>-0.0631207545018899</li><li>0.604907230643111</li><li>2.63682568545915</li></ol>
</dd>
	<dt>$n</dt>
		<dd>1066</dd>
	<dt>$conf</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>-0.130469871110639</li><li>0.00422836210685877</li></ol>
</dd>
	<dt>$out</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>2.72032918360228</li><li>4.44606814522686</li><li>4.44606814522686</li><li>2.83166718112645</li><li>3.11001217493686</li><li>2.83166718112645</li><li>2.72032918360228</li><li>2.8038326817454</li><li>2.74816368298332</li><li>2.72032918360228</li><li>3.61103316379561</li><li>2.83166718112645</li></ol>
</dd>
</dl>



#### Density


```R
density_Q1 <- quantile(no.chlorides.outliers$density, .25)
density_Q3 <- quantile(no.chlorides.outliers$density, .75)
density_IQR <- IQR(no.chlorides.outliers$density)
```


```R
no_outliers_cd <- subset(no.chlorides.outliers, no.chlorides.outliers$density > (density_Q1 - 1.5*density_IQR) & 
                         no.chlorides.outliers$density < (density_Q3 + 1.5*density_IQR))
```


```R
boxplot(no_outliers_cd)
```


    
![png](output_106_0.png)
    


### Recount Outliers


```R
boxplot.stats(no_outliers_cd$volatile.acidity)
```


<dl>
	<dt>$stats</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>-2.28988070498523</li><li>-0.786817738408974</li><li>-0.0631207545018899</li><li>0.604907230643111</li><li>2.63682568545915</li></ol>
</dd>
	<dt>$n</dt>
		<dd>1037</dd>
	<dt>$conf</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>-0.131405096337541</li><li>0.00516358733376149</li></ol>
</dd>
	<dt>$out</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>2.72032918360228</li><li>4.44606814522686</li><li>4.44606814522686</li><li>2.83166718112645</li><li>3.11001217493686</li><li>2.83166718112645</li><li>2.72032918360228</li><li>2.8038326817454</li><li>2.74816368298332</li><li>2.72032918360228</li><li>3.61103316379561</li><li>2.83166718112645</li></ol>
</dd>
</dl>




```R
boxplot.stats(no_outliers_cd$pH)
```


<dl>
	<dt>$stats</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>-2.43205029726369</li><li>-0.644786515090851</li><li>0.0573528279056195</li><li>0.567999622812145</li><li>2.35526340498498</li></ol>
</dd>
	<dt>$n</dt>
		<dd>1037</dd>
	<dt>$conf</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>-0.00215196295152317</li><li>0.116857618762762</li></ol>
</dd>
	<dt>$out</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>3.75954209097792</li><li>2.80207935052819</li><li>2.4190942543483</li><li>2.4190942543483</li><li>-2.75120454408027</li><li>-2.8788662428069</li><li>2.73824850116488</li><li>-2.68737369471695</li><li>-2.68737369471695</li><li>-2.495881146627</li><li>2.4190942543483</li><li>2.99357189861814</li><li>2.99357189861814</li><li>4.4616814339744</li><li>4.4616814339744</li><li>-2.75120454408027</li><li>2.61058680243825</li></ol>
</dd>
</dl>




```R

```
