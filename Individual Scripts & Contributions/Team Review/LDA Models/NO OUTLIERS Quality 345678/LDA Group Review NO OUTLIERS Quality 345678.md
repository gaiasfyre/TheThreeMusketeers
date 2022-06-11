# Linear Discriminant Analysis: Group Review
# Quality: 345678, Outliers: NONE

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



## Identify Outliers

### Density


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
<ol class=list-inline><li>0.9922</li><li>0.99557</li><li>0.99668</li><li>0.997845</li><li>1.001</li></ol>
</dd>
	<dt>$n</dt>
		<dd>1143</dd>
	<dt>$conf</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>0.996573679900897</li><li>0.996786320099103</li></ol>
</dd>
	<dt>$out</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>0.9916</li><li>0.9916</li><li>1.0014</li><li>1.0015</li><li>1.0015</li><li>1.0018</li><li>0.9912</li><li>1.0022</li><li>1.0022</li><li>1.0014</li><li>1.0014</li><li>1.0032</li><li>1.0026</li><li>1.0014</li><li>1.00315</li><li>1.00315</li><li>1.0021</li><li>1.0021</li><li>0.9917</li><li>1.0026</li><li>0.9921</li><li>0.99154</li><li>0.99064</li><li>0.99064</li><li>1.00289</li><li>0.99162</li><li>0.99007</li><li>0.9902</li><li>0.99157</li><li>0.99084</li><li>0.99191</li><li>1.00369</li><li>1.00242</li><li>0.99182</li><li>1.00242</li><li>0.99182</li></ol>
</dd>
</dl>



#### Outliers: 36

### Chlorides


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
<ol class=list-inline><li>0.041</li><li>0.07</li><li>0.079</li><li>0.09</li><li>0.119</li></ol>
</dd>
	<dt>$n</dt>
		<dd>1143</dd>
	<dt>$conf</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>0.0780653178100817</li><li>0.0799346821899183</li></ol>
</dd>
	<dt>$out</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>0.341</li><li>0.332</li><li>0.467</li><li>0.178</li><li>0.61</li><li>0.27</li><li>0.039</li><li>0.337</li><li>0.263</li><li>0.611</li><li>0.358</li><li>0.213</li><li>0.214</li><li>0.121</li><li>0.128</li><li>0.12</li><li>0.122</li><li>0.122</li><li>0.121</li><li>0.127</li><li>0.152</li><li>0.125</li><li>0.122</li><li>0.2</li><li>0.226</li><li>0.25</li><li>0.124</li><li>0.222</li><li>0.039</li><li>0.157</li><li>0.422</li><li>0.034</li><li>0.387</li><li>0.415</li><li>0.157</li><li>0.241</li><li>0.19</li><li>0.132</li><li>0.126</li><li>0.038</li><li>0.165</li><li>0.147</li><li>0.012</li><li>0.012</li><li>0.194</li><li>0.132</li><li>0.161</li><li>0.12</li><li>0.12</li><li>0.123</li><li>0.123</li><li>0.414</li><li>0.171</li><li>0.178</li><li>0.166</li><li>0.136</li><li>0.132</li><li>0.132</li><li>0.123</li><li>0.123</li><li>0.403</li><li>0.137</li><li>0.414</li><li>0.166</li><li>0.168</li><li>0.415</li><li>0.153</li><li>0.415</li><li>0.123</li><li>0.214</li><li>0.169</li><li>0.205</li><li>0.205</li><li>0.039</li><li>0.235</li><li>0.23</li><li>0.038</li></ol>
</dd>
</dl>



#### Outliers: 77

### Volatile Acidity


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
<ol class=list-inline><li>0.12</li><li>0.3925</li><li>0.52</li><li>0.64</li><li>1.005</li></ol>
</dd>
	<dt>$n</dt>
		<dd>1143</dd>
	<dt>$conf</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>0.508433307899761</li><li>0.531566692100239</li></ol>
</dd>
	<dt>$out</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>1.02</li><li>1.07</li><li>1.33</li><li>1.33</li><li>1.04</li><li>1.09</li><li>1.04</li><li>1.02</li><li>1.035</li><li>1.025</li><li>1.02</li><li>1.58</li><li>1.18</li><li>1.04</li></ol>
</dd>
</dl>



#### Outliers: 14

### pH


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
<ol class=list-inline><li>2.92</li><li>3.205</li><li>3.31</li><li>3.4</li><li>3.69</li></ol>
</dd>
	<dt>$n</dt>
		<dd>1143</dd>
	<dt>$conf</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>3.3008868486483</li><li>3.3191131513517</li></ol>
</dd>
	<dt>$out</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>3.9</li><li>3.75</li><li>2.74</li><li>2.88</li><li>2.86</li><li>3.74</li><li>3.72</li><li>2.89</li><li>2.89</li><li>3.9</li><li>3.71</li><li>2.89</li><li>3.78</li><li>3.7</li><li>3.78</li><li>4.01</li><li>2.9</li><li>4.01</li><li>2.88</li><li>3.72</li></ol>
</dd>
</dl>



#### Outliers: 20

## Remove Outliers


```R
chloride.Q1 <- quantile(wine_wrangled$chlorides, .25)
chloride.Q3 <- quantile(wine_wrangled$chlorides, .75)
chloride.IQR <- IQR(wine_wrangled$chlorides)
```


```R
no_outliers_c <- subset(wine_wrangled, wine_wrangled$chlorides > (chloride.Q1 - 1.5*chloride.IQR) & wine_wrangled$chlorides < (chloride.Q3 + 1.5*chloride.IQR))
```


```R
boxplot(no_outliers_c)
```


    
![png](output_28_0.png)
    


### Recheck Outlier Count


```R
boxplot.stats(no_outliers_c$pH)
```


<dl>
	<dt>$stats</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>2.93</li><li>3.21</li><li>3.32</li><li>3.4</li><li>3.68</li></ol>
</dd>
	<dt>$n</dt>
		<dd>1066</dd>
	<dt>$conf</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>3.31080541598361</li><li>3.32919458401639</li></ol>
</dd>
	<dt>$out</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>3.9</li><li>3.75</li><li>3.69</li><li>3.69</li><li>2.88</li><li>2.86</li><li>3.74</li><li>2.92</li><li>2.92</li><li>3.72</li><li>2.89</li><li>2.89</li><li>2.92</li><li>3.71</li><li>3.69</li><li>2.89</li><li>3.78</li><li>3.7</li><li>3.78</li><li>4.01</li><li>4.01</li><li>2.88</li><li>3.72</li></ol>
</dd>
</dl>




```R
boxplot.stats(no_outliers_c$volatile.acidity)
```


<dl>
	<dt>$stats</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>0.12</li><li>0.39</li><li>0.52</li><li>0.64</li><li>1.005</li></ol>
</dd>
	<dt>$n</dt>
		<dd>1066</dd>
	<dt>$conf</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>0.507901863136324</li><li>0.532098136863676</li></ol>
</dd>
	<dt>$out</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>1.02</li><li>1.33</li><li>1.33</li><li>1.04</li><li>1.09</li><li>1.04</li><li>1.02</li><li>1.035</li><li>1.025</li><li>1.02</li><li>1.18</li><li>1.04</li></ol>
</dd>
</dl>




```R
boxplot.stats(no_outliers_c$density)
```


<dl>
	<dt>$stats</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>0.9922</li><li>0.99553</li><li>0.996625</li><li>0.9978</li><li>1.001</li></ol>
</dd>
	<dt>$n</dt>
		<dd>1066</dd>
	<dt>$conf</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>0.996515148917278</li><li>0.996734851082722</li></ol>
</dd>
	<dt>$out</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>0.9916</li><li>0.9916</li><li>1.0014</li><li>1.0015</li><li>1.0015</li><li>0.9912</li><li>1.0022</li><li>1.0022</li><li>1.0014</li><li>1.0014</li><li>1.0032</li><li>1.0026</li><li>1.0014</li><li>1.00315</li><li>1.00315</li><li>1.0021</li><li>1.0021</li><li>0.9917</li><li>1.0026</li><li>0.99154</li><li>1.00289</li><li>0.99162</li><li>0.99007</li><li>0.9902</li><li>0.99157</li><li>0.99084</li><li>0.99191</li><li>0.99182</li><li>0.99182</li></ol>
</dd>
</dl>



### Density


```R
density_Q1 <- quantile(no_outliers_c$density, .25)
density_Q3 <- quantile(no_outliers_c$density, .75)
density_IQR <- IQR(no_outliers_c$density)
```


```R
no_outliers_cd <- subset(no_outliers_c, no_outliers_c$density > (density_Q1 - 1.5*density_IQR) & no_outliers_c$density < (density_Q3 + 1.5*density_IQR))
```


```R
boxplot(no_outliers_cd)
```


    
![png](output_36_0.png)
    


#### Recount Outliers


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
<ol class=list-inline><li>0.12</li><li>0.39</li><li>0.52</li><li>0.64</li><li>1.005</li></ol>
</dd>
	<dt>$n</dt>
		<dd>1037</dd>
	<dt>$conf</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>0.50773386564262</li><li>0.53226613435738</li></ol>
</dd>
	<dt>$out</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>1.02</li><li>1.33</li><li>1.33</li><li>1.04</li><li>1.09</li><li>1.04</li><li>1.02</li><li>1.035</li><li>1.025</li><li>1.02</li><li>1.18</li><li>1.04</li></ol>
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
<ol class=list-inline><li>2.93</li><li>3.21</li><li>3.32</li><li>3.4</li><li>3.68</li></ol>
</dd>
	<dt>$n</dt>
		<dd>1037</dd>
	<dt>$conf</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>3.31067773788839</li><li>3.32932226211161</li></ol>
</dd>
	<dt>$out</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>3.9</li><li>3.75</li><li>3.69</li><li>3.69</li><li>2.88</li><li>2.86</li><li>3.74</li><li>2.89</li><li>2.89</li><li>2.92</li><li>3.69</li><li>3.78</li><li>3.78</li><li>4.01</li><li>4.01</li><li>2.88</li><li>3.72</li></ol>
</dd>
</dl>



### pH


```R
ph_Q1 <- quantile(no_outliers_cd$pH, .25)
ph_Q3 <- quantile(no_outliers_cd$pH, .75)
ph_IQR <- IQR(no_outliers_cd$pH)
```


```R
no_outliers_cdp <- subset(no_outliers_cd, no_outliers_cd$pH > (ph_Q1 - 1.5*ph_IQR) & no_outliers_cd$pH < (ph_Q3 + 1.5*ph_IQR))
```


```R
boxplot(no_outliers_cdp)
```


    
![png](output_43_0.png)
    


#### Recount Outliers


```R
boxplot.stats(no_outliers_cdp$volatile.acidity)
```


<dl>
	<dt>$stats</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>0.12</li><li>0.39</li><li>0.52</li><li>0.64</li><li>1.005</li></ol>
</dd>
	<dt>$n</dt>
		<dd>1020</dd>
	<dt>$conf</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>0.507632070251068</li><li>0.532367929748932</li></ol>
</dd>
	<dt>$out</dt>
		<dd><style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>1.33</li><li>1.33</li><li>1.04</li><li>1.09</li><li>1.02</li><li>1.035</li><li>1.025</li><li>1.02</li><li>1.18</li><li>1.04</li></ol>
</dd>
</dl>



### Volatile Acidity


```R
acid_Q1 <- quantile(no_outliers_cdp$volatile.acidity, .25)
acid_Q3 <- quantile(no_outliers_cdp$volatile.acidity, .75)
acid_IQR <- IQR(no_outliers_cdp$volatile.acidity)
```


```R
no_outliers <- subset(no_outliers_cdp, no_outliers_cdp$volatile.acidity > (acid_Q1 - 1.5*acid_IQR) & no_outliers_cdp$volatile.acidity < (acid_Q3 + 1.5*acid_IQR))
```


```R
boxplot(no_outliers)
```


    
![png](output_49_0.png)
    


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



    
![png](output_78_1.png)
    


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



    
![png](output_80_1.png)
    


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
	<tr><th scope=row>4</th><td> 1.2974723</td><td> 0.7632667</td><td> 0.5639303</td><td>0.75481104</td></tr>
	<tr><th scope=row>6</th><td>-0.9630362</td><td>-1.2052765</td><td> 0.4364889</td><td>0.67401211</td></tr>
	<tr><th scope=row>7</th><td>-0.2441581</td><td> 0.1737348</td><td>-0.5868537</td><td>0.18837569</td></tr>
	<tr><th scope=row>15</th><td> 0.4206838</td><td>-1.5694830</td><td> 1.4187161</td><td>0.80411739</td></tr>
	<tr><th scope=row>17</th><td> 0.3149221</td><td> 0.9164348</td><td>-0.2763403</td><td>0.01606496</td></tr>
	<tr><th scope=row>18</th><td> 0.6523267</td><td>-0.9405094</td><td> 0.6666388</td><td>0.15489907</td></tr>
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



    
![png](output_109_1.png)
    



```R

```
