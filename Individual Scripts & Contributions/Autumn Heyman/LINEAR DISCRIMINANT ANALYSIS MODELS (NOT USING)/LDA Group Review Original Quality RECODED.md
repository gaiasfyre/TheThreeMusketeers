# Linear Discriminant Analysis: Group Review
# Quality: RECODED, Outliers: Original

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

    corrplot 0.92 loaded
    
    Warning message:
    “package ‘tibble’ was built under R version 4.1.2”
    
    Attaching package: ‘reshape2’
    
    
    The following object is masked from ‘package:tidyr’:
    
        smiths
    
    


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

### Recode Quality into Three Groups


```R
wine$qualityR <- NA
wine$qualityR [wine$quality==3] <- 0
wine$qualityR [wine$quality==4] <- 0
wine$qualityR [wine$quality==5] <- 1
wine$qualityR [wine$quality==6] <- 1
wine$qualityR [wine$quality==7] <- 2
wine$qualityR [wine$quality==8] <- 2
```

### Remove Unnecessary Columns


```R
wine_wrangled <- wine[, c(2,5,8,9,14)]
head(wine_wrangled)
```


<table class="dataframe">
<caption>A data.frame: 6 × 5</caption>
<thead>
	<tr><th></th><th scope=col>volatile.acidity</th><th scope=col>chlorides</th><th scope=col>density</th><th scope=col>pH</th><th scope=col>qualityR</th></tr>
	<tr><th></th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><th scope=row>1</th><td>0.70</td><td>0.076</td><td>0.9978</td><td>3.51</td><td>1</td></tr>
	<tr><th scope=row>2</th><td>0.88</td><td>0.098</td><td>0.9968</td><td>3.20</td><td>1</td></tr>
	<tr><th scope=row>3</th><td>0.76</td><td>0.092</td><td>0.9970</td><td>3.26</td><td>1</td></tr>
	<tr><th scope=row>4</th><td>0.28</td><td>0.075</td><td>0.9980</td><td>3.16</td><td>1</td></tr>
	<tr><th scope=row>5</th><td>0.70</td><td>0.076</td><td>0.9978</td><td>3.51</td><td>1</td></tr>
	<tr><th scope=row>6</th><td>0.66</td><td>0.075</td><td>0.9978</td><td>3.51</td><td>1</td></tr>
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
model <- lda(qualityR~., data=train)
```

### View Model Output


```R
model
```


    Call:
    lda(qualityR ~ ., data = train)
    
    Prior probabilities of groups:
          0       1       2 
    0.03875 0.81375 0.14750 
    
    Group means:
      volatile.acidity   chlorides      density          pH
    0       1.00446698 -0.10367361  0.005651296  0.53093655
    1       0.09140134  0.02043661  0.058451338  0.01519113
    2      -0.72265686 -0.21999832 -0.273547676 -0.19634775
    
    Coefficients of linear discriminants:
                           LD1        LD2
    volatile.acidity 0.9106706  0.2070886
    chlorides        0.1985976 -0.7306090
    density          0.3157373 -0.5016395
    pH               0.2912990  0.1980449
    
    Proportion of trace:
       LD1    LD2 
    0.9531 0.0469 


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
</style><dl class=dl-inline><dt>0</dt><dd>31</dd><dt>1</dt><dd>651</dd><dt>2</dt><dd>118</dd></dl>




<style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>0</dt><dd>0.03875</dd><dt>1</dt><dd>0.81375</dd><dt>2</dt><dd>0.1475</dd></dl>




<table class="dataframe">
<caption>A matrix: 4 × 2 of type dbl</caption>
<thead>
	<tr><th></th><th scope=col>LD1</th><th scope=col>LD2</th></tr>
</thead>
<tbody>
	<tr><th scope=row>volatile.acidity</th><td>0.9106706</td><td> 0.2070886</td></tr>
	<tr><th scope=row>chlorides</th><td>0.1985976</td><td>-0.7306090</td></tr>
	<tr><th scope=row>density</th><td>0.3157373</td><td>-0.5016395</td></tr>
	<tr><th scope=row>pH</th><td>0.2912990</td><td> 0.1980449</td></tr>
</tbody>
</table>




<style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>7.95044730356161</li><li>1.76402103328014</li></ol>



### Store Model Means


```R
modelmeans <- model$means
```

### Convert Matrix to a Data Frame


```R
modelmeans <- as.data.frame(modelmeans)
str(modelmeans)
```

    'data.frame':	3 obs. of  4 variables:
     $ volatile.acidity: num  1.0045 0.0914 -0.7227
     $ chlorides       : num  -0.1037 0.0204 -0.22
     $ density         : num  0.00565 0.05845 -0.27355
     $ pH              : num  0.5309 0.0152 -0.1963


### Convert Row Labels to Columns


```R
modelmeans <- tibble::rownames_to_column(modelmeans, "qualityR")
```

## Plot Means

### Melt Data Frame


```R
mmodelmeans <- melt(modelmeans, id.vars="qualityR")
```

### All Variables on the Same Plot


```R
ggplot(mmodelmeans, aes(quality, value, group=variable, col=variable)) + ggtitle("All Variables vs. Quality")+ geom_line() + geom_point() + stat_smooth()
```

    ERROR while rich displaying an object: Error in FUN(X[[i]], ...): object 'quality' not found
    
    Traceback:
    1. FUN(X[[i]], ...)
    2. tryCatch(withCallingHandlers({
     .     if (!mime %in% names(repr::mime2repr)) 
     .         stop("No repr_* for mimetype ", mime, " in repr::mime2repr")
     .     rpr <- repr::mime2repr[[mime]](obj)
     .     if (is.null(rpr)) 
     .         return(NULL)
     .     prepare_content(is.raw(rpr), rpr)
     . }, error = error_handler), error = outer_handler)
    3. tryCatchList(expr, classes, parentenv, handlers)
    4. tryCatchOne(expr, names, parentenv, handlers[[1L]])
    5. doTryCatch(return(expr), name, parentenv, handler)
    6. withCallingHandlers({
     .     if (!mime %in% names(repr::mime2repr)) 
     .         stop("No repr_* for mimetype ", mime, " in repr::mime2repr")
     .     rpr <- repr::mime2repr[[mime]](obj)
     .     if (is.null(rpr)) 
     .         return(NULL)
     .     prepare_content(is.raw(rpr), rpr)
     . }, error = error_handler)
    7. repr::mime2repr[[mime]](obj)
    8. repr_text.default(obj)
    9. paste(capture.output(print(obj)), collapse = "\n")
    10. capture.output(print(obj))
    11. withVisible(...elt(i))
    12. print(obj)
    13. print.ggplot(obj)
    14. ggplot_build(x)
    15. ggplot_build.ggplot(x)
    16. by_layer(function(l, d) l$compute_aesthetics(d, plot))
    17. f(l = layers[[i]], d = data[[i]])
    18. l$compute_aesthetics(d, plot)
    19. f(..., self = self)
    20. scales_add_defaults(plot$scales, data, aesthetics, plot$plot_env)
    21. lapply(aesthetics[new_aesthetics], eval_tidy, data = data)
    22. FUN(X[[i]], ...)



    
![png](output_42_1.png)
    


### Variables on Separate Plots


```R
ggplot(mmodelmeans, aes(quality, value, group=variable, col=variable)) + ggtitle ("Separately Plotted Variables vs. Quality")+
  geom_point() + 
  stat_smooth() +
  facet_wrap(~variable)
```

    ERROR while rich displaying an object: Error in FUN(X[[i]], ...): object 'quality' not found
    
    Traceback:
    1. FUN(X[[i]], ...)
    2. tryCatch(withCallingHandlers({
     .     if (!mime %in% names(repr::mime2repr)) 
     .         stop("No repr_* for mimetype ", mime, " in repr::mime2repr")
     .     rpr <- repr::mime2repr[[mime]](obj)
     .     if (is.null(rpr)) 
     .         return(NULL)
     .     prepare_content(is.raw(rpr), rpr)
     . }, error = error_handler), error = outer_handler)
    3. tryCatchList(expr, classes, parentenv, handlers)
    4. tryCatchOne(expr, names, parentenv, handlers[[1L]])
    5. doTryCatch(return(expr), name, parentenv, handler)
    6. withCallingHandlers({
     .     if (!mime %in% names(repr::mime2repr)) 
     .         stop("No repr_* for mimetype ", mime, " in repr::mime2repr")
     .     rpr <- repr::mime2repr[[mime]](obj)
     .     if (is.null(rpr)) 
     .         return(NULL)
     .     prepare_content(is.raw(rpr), rpr)
     . }, error = error_handler)
    7. repr::mime2repr[[mime]](obj)
    8. repr_text.default(obj)
    9. paste(capture.output(print(obj)), collapse = "\n")
    10. capture.output(print(obj))
    11. withVisible(...elt(i))
    12. print(obj)
    13. print.ggplot(obj)
    14. ggplot_build(x)
    15. ggplot_build.ggplot(x)
    16. by_layer(function(l, d) l$compute_aesthetics(d, plot))
    17. f(l = layers[[i]], d = data[[i]])
    18. l$compute_aesthetics(d, plot)
    19. f(..., self = self)
    20. scales_add_defaults(plot$scales, data, aesthetics, plot$plot_env)
    21. lapply(aesthetics[new_aesthetics], eval_tidy, data = data)
    22. FUN(X[[i]], ...)



    
![png](output_44_1.png)
    


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
<ol class=list-inline><li>1</li><li>1</li><li>1</li><li>1</li><li>1</li><li>1</li></ol>

<details>
	<summary style=display:list-item;cursor:pointer>
		<strong>Levels</strong>:
	</summary>
	<style>
	.list-inline {list-style: none; margin:0; padding: 0}
	.list-inline>li {display: inline-block}
	.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
	</style>
	<ol class=list-inline><li>'0'</li><li>'1'</li><li>'2'</li></ol>
</details>


### View Predicted Posterior Probability (First Six Observations)¶


```R
head(predicted$posterior)
```


<table class="dataframe">
<caption>A matrix: 6 × 3 of type dbl</caption>
<thead>
	<tr><th></th><th scope=col>0</th><th scope=col>1</th><th scope=col>2</th></tr>
</thead>
<tbody>
	<tr><th scope=row>4</th><td>0.003678563</td><td>0.6901224</td><td>0.30619903</td></tr>
	<tr><th scope=row>6</th><td>0.075592617</td><td>0.8853544</td><td>0.03905293</td></tr>
	<tr><th scope=row>7</th><td>0.032025977</td><td>0.8712111</td><td>0.09676291</td></tr>
	<tr><th scope=row>15</th><td>0.018379246</td><td>0.8634723</td><td>0.11814842</td></tr>
	<tr><th scope=row>17</th><td>0.012521832</td><td>0.8269567</td><td>0.16052150</td></tr>
	<tr><th scope=row>18</th><td>0.014032561</td><td>0.8260119</td><td>0.15995554</td></tr>
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


0.868804664723032


## Visualize the Results

### Convert Quality to a Factor


```R
wine_wrangled$quality = factor(wine_wrangled$qualityR, levels = c("0", "1", "2"), ordered = TRUE)
str(wine_wrangled)
```

    'data.frame':	1143 obs. of  6 variables:
     $ volatile.acidity: num  0.939 1.941 1.273 -1.399 0.939 ...
     $ chlorides       : num  -0.231 0.234 0.107 -0.252 -0.231 ...
     $ density         : num  0.5556 0.0361 0.14 0.6595 0.5556 ...
     $ pH              : num  1.27 -0.709 -0.326 -0.964 1.27 ...
     $ qualityR        : num  1 1 1 1 1 1 1 2 2 1 ...
     $ quality         : Ord.factor w/ 3 levels "0"<"1"<"2": 2 2 2 2 2 2 2 3 3 2 ...


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


    Error in lda.default(x, grouping, ...): variable 5 appears to be constant within groups
    Traceback:


    1. lda(quality ~ ., data = train)

    2. lda.formula(quality ~ ., data = train)

    3. lda.default(x, grouping, ...)

    4. stop(sprintf(ngettext(length(const), "variable %s appears to be constant within groups", 
     .     "variables %s appear to be constant within groups"), paste(const, 
     .     collapse = " ")), domain = NA)


### Define Data to Plot


```R
lda_plot <- cbind(train, predict(LDAmodel)$x)
```


    Error in model.matrix.default(Terms, newdata, contrasts = object$contrasts): model frame and formula mismatch in model.matrix()
    Traceback:


    1. cbind(train, predict(LDAmodel)$x)

    2. cbind(deparse.level, ...)

    3. data.frame(..., check.names = FALSE)

    4. predict(LDAmodel)

    5. predict.lda(LDAmodel)

    6. model.matrix(Terms, newdata, contrasts = object$contrasts)

    7. model.matrix.default(Terms, newdata, contrasts = object$contrasts)

    8. stop("model frame and formula mismatch in model.matrix()")


### Create Plot


```R
ggplot(lda_plot, aes(LD1, LD2)) +
  geom_point(aes(color = qualityR))
```


    Error in ggplot(lda_plot, aes(LD1, LD2)): object 'lda_plot' not found
    Traceback:


    1. ggplot(lda_plot, aes(LD1, LD2))


## Identify Outliers

### Density


```R
bp.density <- ggplot(wine_wrangled, aes(x = "", y = density)) + geom_boxplot() + xlab("")
bp.density
```


    
![png](output_76_0.png)
    



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


    
![png](output_80_0.png)
    



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


    
![png](output_84_0.png)
    



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


    
![png](output_88_0.png)
    



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
<caption>A data.frame: 3 × 2</caption>
<thead>
	<tr><th scope=col>Group.1</th><th scope=col>x</th></tr>
	<tr><th scope=col>&lt;ord&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>0</td><td> 1.10806933</td></tr>
	<tr><td>1</td><td> 0.08167755</td></tr>
	<tr><td>2</td><td>-0.75723264</td></tr>
</tbody>
</table>




```R
boxplot(wine_wrangled)
```


    
![png](output_93_0.png)
    



```R
ggplot(stack(wine_wrangled), aes(x = ind, y = values))+
  geom_boxplot(fill='rosybrown', color="darkred") +
  coord_flip()
```


    
![png](output_94_0.png)
    


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


    
![png](output_99_0.png)
    


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


    
![png](output_108_0.png)
    


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
