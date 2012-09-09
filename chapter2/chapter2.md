Chapter 2: Exploratory and descriptive methods
========================================================
Last modified `Thu Aug  9 14:22:02 2012` 

One variale descriptions
-------------------------
### Numerical variables

Dataset: The Western Collaborative Group Study (WCGS) was a study designed to inverstigate the association between the "type A" behavior pattern and coronary heart disease. Goal for this exercise: explore the distribution of systolic blood pressure (SBP)

The dataset was analyzed using Stata and my goal is to be able to calculate the same statistics and provide the same or similar output for the analyses. 

The data was downloaded from [here](http://www.epibiostat.ucsf.edu/biostat/vgsm/) in a text format.

Read the data:


```r
getwd()
```

[1] "/Users/vitalina/Documents/personal/RegressionBiostats_RMarkdown/chapter2"

```r
setwd("~/Documents/personal/RegressionBiostats_RMarkdown/chapter2/")
# After the first attempt realized that I needed to include sep because I
# had a problem with line 39 and also row.names=1, because it seems that
# the first column is the patient ID
wcgs <- read.table("./data/wcgs.txt", header = T, sep = "\t", row.names = 1)
```

In the book they show that summarize sbp provides the following descrptive statistics about the data: 
* percentiles (1,5,10,25,50,75,90,95,99) and corresponding data values
* list of 4 smallest values of the variable
* list of 4 largest values of the variable
* number of observations
* sum of the weights (Sum of Wgt.) - by default each of the values is given weight of 1. In this case it will be equal to the number of observations
* mean
* standard deviation
* variance
* skewness 
* kurtosis

We need to compute all these statistics in R. 

Start with **percentiles**:

```r
# Default in quantiles in R can be calculated using the function summary:
with(wcgs, summary(sbp))
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      98     120     126     129     136     230 
```

```r
# Alternatively, we can do it using the function quantile:
with(wcgs, quantile(sbp))
```

```
##   0%  25%  50%  75% 100% 
##   98  120  126  136  230 
```

```r
# Stata presented a slightly different range for the quantiles:
with(wcgs, quantile(sbp, probs = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 
    0.99)))
```

```
##  1%  5% 10% 25% 50% 75% 90% 95% 99% 
## 104 110 112 120 126 136 148 156 176 
```

Calculate **4 smallest values**. I couldn't find a better solution than sort the vector and then listing the first 4 values:


```r
sbp_sorted <- sort(wcgs$sbp)
sbp_sorted[1:4]
```

```
## [1]  98 100 100 100
```


Calculate **4 largest values**, general solution is more appropriate:

```r
sbp_sorted[(length(sbp_sorted) - 3):length(sbp_sorted)]
```

```
## [1] 210 210 212 230
```


**Number of observations = sum of weights:**

```r
with(wcgs, length(sbp))
```

```
## [1] 3154
```

**Mean:**

```r
# by hand:
sum(wcgs$sbp)/length(wcgs$sbp)
```

```
## [1] 128.6
```

```r
# using R function:
mean(wcgs$sbp)
```

```
## [1] 128.6
```


**Variance** first:

```r
# by hand:
sum((wcgs$sbp - mean(wcgs$sbp))^2)/(length(wcgs$sbp) - 1)
```

```
## [1] 228.5
```

```r
# with R function:
var(wcgs$sbp)
```

```
## [1] 228.5
```

**Standard deviation:**

```r
# by hand
sqrt(sum((wcgs$sbp - mean(wcgs$sbp))^2)/(length(wcgs$sbp) - 1))
```

```
## [1] 15.12
```

```r
# which is equivalent to:
sqrt(var(wcgs$sbp))
```

```
## [1] 15.12
```

```r
# using R function:
sd(wcgs$sbp)
```

```
## [1] 15.12
```

**Skewness**, according the the page 285 in ["The R Book"](http://www.amazon.com/The-Book-Michael-J-Crawley/dp/0470510242) by Michael J. Crawley "is the dimensionless version of the third moment about the mean" which is defined as 
$$
m_3 = \frac{\sum(x-\bar{x})^3}{n}
$$ 
and "which is rendered dimensionless by dividing by the cube of the standard deviation" $s_3 = sd(x)^3$:

$$
skew = \frac{m_3}{s_3}
$$
A symmetrical distribution has skew = 0, negative value means skew to the left and positive value means skew to the right. 


```r
m3 = sum((wcgs$sbp - mean(wcgs$sbp))^3)/length(wcgs$sbp)
s3 = sd(wcgs$sbp)^3
skew = m3/s3
skew
```

```
## [1] 1.204
```

So it looks that the distirbution is skewed to the right which is also indicated by the mean and median of SBP (median is less than mean). Michael Crawley also suggests to test whether a particular value of skew is significantly different from 0 by dividing by its approximate standard error, for which he gives the following formula:
$$
SES = \sqrt{\frac{6}{n}},
$$
where n is the number of observations. However [here](http://www.tc3.edu/instruct/sbrown/stat/shape.htm) I found that for small samples it is a poor estimation (doubt we will ever have this problem). But still, they suggest the following formula which we can easily calculate in R:
$$
SES = \sqrt{\frac{6n(n-1)}{(n-2)(n+1)(n+3)}}
$$
Calculate the standard error of skew:

```r
n = length(wcgs$sbp)
ses = sqrt((6 * n * (n - 1))/((n - 2) * (n + 1) * (n + 3)))
```

To calculate the test statistic which measures hw many standard errors separate the sample skewness from 0:


```r
skew/ses
```

```
## [1] 27.61
```

The critical value for the test statistic is 2 at the significance level of 0.05. Now I need to know whether this value is significantly different from 0. 


```r
1 - pt(skew/ses, length(wcgs$sbp) - 2)
```

```
## [1] 0
```

Number of degrees of freedom for the t test is estimated as length(wcgs$sbp)-2 because we estimated mean and the standard deviation from the data. We conclude that these data show significant non-normality (p < 0)

**Kurtosis** "is the measure of non-normality that has to do with the peakyness, or flat-toppedness" of a distribution (from "The R Book" by Michael J. Crawley). Flat distributionis called platykurtic (lower values of kurtosis; more extreme differences from the mean), pointy distribution is callled leptokurtic (higher values of kurtosis; few extreme differences from the mean [Wikipedia](http://en.wikipedia.org/wiki/Kurtosis). Kurtosis is the dimensionless version of the forth moment around the mean (remember, that skewness is the third moment around the mean).

```r
m4 = sum((wcgs$sbp - mean(wcgs$sbp))^4)/length(wcgs$sbp)
s4 = sd(wcgs$sbp)^4
kurtosis = (m4/s4) - 3  #the minus 3 is included because a normal distribution has m4/s4=3
kurtosis
```

```
## [1] 2.789
```

Instead of calculating skewness and kurtosis by hand one can use functions available from e1071 package:


```r
require(e1071)
```

```
## Loading required package: e1071
```

```
## Loading required package: class
```

```r
skewness(wcgs$sbp)
```

```
## [1] 1.204
```

```r
kurtosis(wcgs$sbp)
```

```
## [1] 2.789
```



I would also add a **check for NAs**:

```r
with(wcgs, table(is.na(sbp)))
```

```
## 
## FALSE 
##  3154 
```





