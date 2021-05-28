Week 12 IP
================
Harrieth Rotich
5/20/2021

# AD CLICK ANALYSIS

## Defining the Question.

#### Question:

Which individuals are most likely to click on an online cryptography
course advertisement?

#### Metric for Success:

Individuals upto a 250 are selected who are most likely to click on the
ad

#### Context:

A Kenyan entrepreneur has created an online cryptography course and
would want to advertise it on her blog. She currently targets audiences
originating from various countries. In the past, she ran ads to
advertise a related course on the same blog and collected data in the
process. She would now like to employ your services as a Data Science
Consultant to help her identify which individuals are most likely to
click on her ads.

#### Experimental Design taken:

1.  Loading the data

2.  Cleaning the data

3.  Univariate Analysis

4.  Bivariate Analysis

5.  Conclusion

#### Data Relevance:

The data provided was higly relevant to the research.

## Loading the data

``` r
# Loading the necessary packages
library("data.table")
advert <- fread("http://bit.ly/IPAdvertisingData")
```

## Checking the data

``` r
# Viewing the data
head(advert)
```

    ##    Daily Time Spent on Site Age Area Income Daily Internet Usage
    ## 1:                    68.95  35    61833.90               256.09
    ## 2:                    80.23  31    68441.85               193.77
    ## 3:                    69.47  26    59785.94               236.50
    ## 4:                    74.15  29    54806.18               245.89
    ## 5:                    68.37  35    73889.99               225.58
    ## 6:                    59.99  23    59761.56               226.74
    ##                            Ad Topic Line           City Male    Country
    ## 1:    Cloned 5thgeneration orchestration    Wrightburgh    0    Tunisia
    ## 2:    Monitored national standardization      West Jodi    1      Nauru
    ## 3:      Organic bottom-line service-desk       Davidton    0 San Marino
    ## 4: Triple-buffered reciprocal time-frame West Terrifurt    1      Italy
    ## 5:         Robust logistical utilization   South Manuel    0    Iceland
    ## 6:       Sharable client-driven software      Jamieberg    1     Norway
    ##              Timestamp Clicked on Ad
    ## 1: 2016-03-27 00:53:11             0
    ## 2: 2016-04-04 01:39:02             0
    ## 3: 2016-03-13 20:35:42             0
    ## 4: 2016-01-10 02:31:19             0
    ## 5: 2016-06-03 03:36:18             0
    ## 6: 2016-05-19 14:30:17             0

``` r
# Basic structure of the data
str(advert)
```

    ## Classes 'data.table' and 'data.frame':   1000 obs. of  10 variables:
    ##  $ Daily Time Spent on Site: num  69 80.2 69.5 74.2 68.4 ...
    ##  $ Age                     : int  35 31 26 29 35 23 33 48 30 20 ...
    ##  $ Area Income             : num  61834 68442 59786 54806 73890 ...
    ##  $ Daily Internet Usage    : num  256 194 236 246 226 ...
    ##  $ Ad Topic Line           : chr  "Cloned 5thgeneration orchestration" "Monitored national standardization" "Organic bottom-line service-desk" "Triple-buffered reciprocal time-frame" ...
    ##  $ City                    : chr  "Wrightburgh" "West Jodi" "Davidton" "West Terrifurt" ...
    ##  $ Male                    : int  0 1 0 1 0 1 0 1 1 1 ...
    ##  $ Country                 : chr  "Tunisia" "Nauru" "San Marino" "Italy" ...
    ##  $ Timestamp               : chr  "2016-03-27 00:53:11" "2016-04-04 01:39:02" "2016-03-13 20:35:42" "2016-01-10 02:31:19" ...
    ##  $ Clicked on Ad           : int  0 0 0 0 0 0 0 1 0 0 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

## Tidying the dataset the dataset

``` r
# Identifying duplicates
advert[duplicated(advert), ]
```

    ## Empty data.table (0 rows and 10 cols): Daily Time Spent on Site,Age,Area Income,Daily Internet Usage,Ad Topic Line,City...

There are no duplicates in this dataset.

``` r
# Identifying missing data
length(which(!is.na(advert)))
```

    ## [1] 10000

``` r
colSums(is.na(advert))
```

    ## Daily Time Spent on Site                      Age              Area Income 
    ##                        0                        0                        0 
    ##     Daily Internet Usage            Ad Topic Line                     City 
    ##                        0                        0                        0 
    ##                     Male                  Country                Timestamp 
    ##                        0                        0                        0 
    ##            Clicked on Ad 
    ##                        0

There is no missing data.

``` r
# Checking for outliers

boxplot.stats(advert$`Daily Time Spent on Site`)$out
```

    ## numeric(0)

``` r
boxplot.stats(advert$Age)$out
```

    ## integer(0)

``` r
boxplot.stats(advert$`Area Income`)$out
```

    ## [1] 17709.98 18819.34 15598.29 15879.10 14548.06 13996.50 14775.50 18368.57

``` r
boxplot.stats(advert$`Daily Internet Usage`)$out
```

    ## numeric(0)

``` r
boxplot.stats(advert$`Clicked on Ad`)$out
```

    ## integer(0)

There are eight outliers in the column ‘Area Income’

``` r
# Visualizing the outliers in the 'Area Income' column

# Outlier values
outlier_values <- boxplot.stats(advert$`Area Income`)$out
# Boxplot showing the outliers
boxplot(advert$`Area Income`, main="Area Income", boxwex=0.1)
# Showing the values of the outliers in the column
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
```

![](Moringa_Core_Week_12_IP_Chebet_Harrieth_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# Investigating the individual outliers
outlier1 <- advert$`Area Income` == 17709.98
outlier2 <- advert$`Area Income` == 18819.34 
outlier3 <- advert$`Area Income` == 15598.29 
outlier4 <- advert$`Area Income` == 14548.06 
outlier5 <- advert$`Area Income` == 13996.50 
outlier6 <- advert$`Area Income` == 14775.50 
outlier7 <- advert$`Area Income` == 18368.57
outlier8 <- advert$`Area Income` == 15879.10 

advert[outlier1]
```

    ##    Daily Time Spent on Site Age Area Income Daily Internet Usage
    ## 1:                    49.89  39    17709.98               160.03
    ##                         Ad Topic Line         City Male Country
    ## 1: Enhanced system-worthy application East Michele    1  Belize
    ##              Timestamp Clicked on Ad
    ## 1: 2016-04-16 12:09:25             1

``` r
advert[outlier2]
```

    ##    Daily Time Spent on Site Age Area Income Daily Internet Usage
    ## 1:                    57.86  30    18819.34               166.86
    ##                 Ad Topic Line      City Male Country           Timestamp
    ## 1: Horizontal modular success Estesfurt    0 Algeria 2016-07-08 17:14:01
    ##    Clicked on Ad
    ## 1:             1

``` r
advert[outlier3]
```

    ##    Daily Time Spent on Site Age Area Income Daily Internet Usage
    ## 1:                    64.63  45    15598.29                158.8
    ##                                   Ad Topic Line         City Male    Country
    ## 1: Triple-buffered high-level Internet solution Isaacborough    1 Azerbaijan
    ##              Timestamp Clicked on Ad
    ## 1: 2016-06-12 03:11:04             1

``` r
advert[outlier4]
```

    ##    Daily Time Spent on Site Age Area Income Daily Internet Usage
    ## 1:                    66.26  47    14548.06               179.04
    ##                     Ad Topic Line        City Male Country           Timestamp
    ## 1: Optional full-range projection Matthewtown    1 Lebanon 2016-04-25 19:31:39
    ##    Clicked on Ad
    ## 1:             1

``` r
advert[outlier5]
```

    ##    Daily Time Spent on Site Age Area Income Daily Internet Usage
    ## 1:                    68.58  41     13996.5               171.54
    ##                  Ad Topic Line             City Male     Country
    ## 1: Exclusive discrete firmware New Williamville    1 El Salvador
    ##              Timestamp Clicked on Ad
    ## 1: 2016-07-06 12:04:29             1

``` r
advert[outlier6]
```

    ##    Daily Time Spent on Site Age Area Income Daily Internet Usage
    ## 1:                    52.67  44     14775.5               191.26
    ##                               Ad Topic Line          City Male Country
    ## 1: Persevering 5thgeneration knowledge user New Hollyberg    0  Jersey
    ##              Timestamp Clicked on Ad
    ## 1: 2016-05-19 06:37:38             1

``` r
advert[outlier7]
```

    ##    Daily Time Spent on Site Age Area Income Daily Internet Usage
    ## 1:                    62.79  36    18368.57               231.87
    ##             Ad Topic Line      City Male    Country           Timestamp
    ## 1: Total coherent archive New James    1 Luxembourg 2016-05-30 20:08:51
    ##    Clicked on Ad
    ## 1:             1

``` r
advert[outlier8]
```

    ##    Daily Time Spent on Site Age Area Income Daily Internet Usage
    ## 1:                    58.05  32     15879.1               195.54
    ##                      Ad Topic Line        City Male    Country
    ## 1: Total asynchronous architecture Sanderstown    1 Tajikistan
    ##              Timestamp Clicked on Ad
    ## 1: 2016-02-12 10:39:10             1

## Univariate Analysis

``` r
# Summary of the dataset
summary(advert)
```

    ##  Daily Time Spent on Site      Age         Area Income    Daily Internet Usage
    ##  Min.   :32.60            Min.   :19.00   Min.   :13996   Min.   :104.8       
    ##  1st Qu.:51.36            1st Qu.:29.00   1st Qu.:47032   1st Qu.:138.8       
    ##  Median :68.22            Median :35.00   Median :57012   Median :183.1       
    ##  Mean   :65.00            Mean   :36.01   Mean   :55000   Mean   :180.0       
    ##  3rd Qu.:78.55            3rd Qu.:42.00   3rd Qu.:65471   3rd Qu.:218.8       
    ##  Max.   :91.43            Max.   :61.00   Max.   :79485   Max.   :270.0       
    ##  Ad Topic Line          City                Male         Country         
    ##  Length:1000        Length:1000        Min.   :0.000   Length:1000       
    ##  Class :character   Class :character   1st Qu.:0.000   Class :character  
    ##  Mode  :character   Mode  :character   Median :0.000   Mode  :character  
    ##                                        Mean   :0.481                     
    ##                                        3rd Qu.:1.000                     
    ##                                        Max.   :1.000                     
    ##   Timestamp         Clicked on Ad
    ##  Length:1000        Min.   :0.0  
    ##  Class :character   1st Qu.:0.0  
    ##  Mode  :character   Median :0.5  
    ##                     Mean   :0.5  
    ##                     3rd Qu.:1.0  
    ##                     Max.   :1.0

``` r
# Getting the time period of the data
range(advert$Timestamp)
```

    ## [1] "2016-01-01 02:52:10" "2016-07-24 00:22:16"

``` r
# Getting variance and std.deviation of Daily time spent on site

var(advert$`Daily Time Spent on Site`)
```

    ## [1] 251.3371

``` r
sd(advert$`Daily Time Spent on Site`)
```

    ## [1] 15.85361

``` r
# Getting variance and std.deviation of Area Income

var(advert$`Area Income`)
```

    ## [1] 179952406

``` r
sd(advert$`Area Income`)
```

    ## [1] 13414.63

``` r
# Getting variance and std.deviation of Daily Internet Usage
var(advert$`Daily Internet Usage`)
```

    ## [1] 1927.415

``` r
sd(advert$`Daily Internet Usage`)
```

    ## [1] 43.90234

``` r
# Function to get mode
mode <- function(v){
  uniq <- unique(as.integer(v))
  uniq[which.max(tabulate(match(as.integer(v), uniq)))]
}

# Mode of daily time spent on site
daily.site <- mode(advert$`Daily Time Spent on Site`)
daily.site
```

    ## [1] 78

Most people spend 78 minutes browsing on a site. 78 is the 3rd quantile,
it shows that most of the people spend a lot of time on the site.

``` r
# Getting the most number of people coming from a certain age
age.mode <- mode(advert$Age)
age.mode
```

    ## [1] 31

``` r
# Distribution of the genders
gender <- advert$Male
gen <- table(gender)
barplot(gen, main = "Gender Distribution", 
        ylab = "Distribution")
```

![](Moringa_Core_Week_12_IP_Chebet_Harrieth_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

Females browsing on sites are more than males by a small difference, of
20.

``` r
# Distribution of whether one clicked or did not click on an ad
click <- advert$`Clicked on Ad`
clicked <- table(click)
barplot(clicked, main = "Ad Click Distribution", 
        ylab = "Distribution")
```

![](Moringa_Core_Week_12_IP_Chebet_Harrieth_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

There are 50% chances that a person can click on an ad while browsing on
the internet. Although most people spend a lot of time on the site, one
cannot tell whether they’ll click on an ad or not.

## Bivariate Analysis

``` r
# loading the dplyr library
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
# Getting correlation of the continuous variables
res <- cor(advert %>% select(1:4))
# Rounding off the correlations 
round(res, 2)
```

    ##                          Daily Time Spent on Site   Age Area Income
    ## Daily Time Spent on Site                     1.00 -0.33        0.31
    ## Age                                         -0.33  1.00       -0.18
    ## Area Income                                  0.31 -0.18        1.00
    ## Daily Internet Usage                         0.52 -0.37        0.34
    ##                          Daily Internet Usage
    ## Daily Time Spent on Site                 0.52
    ## Age                                     -0.37
    ## Area Income                              0.34
    ## Daily Internet Usage                     1.00

``` r
# Correlation plot package
library(corrplot)
```

    ## corrplot 0.84 loaded

``` r
#Plotting a correlation matrix plot
corrplot(res, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
```

![](Moringa_Core_Week_12_IP_Chebet_Harrieth_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
# ggplot package
library(ggplot2)
```

``` r
# Grouping the data by whether one clicked an ad or not
group <- NA
group[advert$`Clicked on Ad` == 1] <- 1
group[advert$`Clicked on Ad` == 0] <- 2

# Plotting a pair plot
pairs(advert[,1:4], pch = 19, lower.panel = NULL, 
      main="Pair Plots showing the relationships between variables",
      col = c("red", "purple")[group])
```

![](Moringa_Core_Week_12_IP_Chebet_Harrieth_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
# Grouping by gender
group1 <- NA
group1[advert$Male == 1] <- 1
group1[advert$Male == 0] <- 2
```

``` r
# Setting the x and y variables
x <- advert$Age
y <- advert$`Daily Time Spent on Site`

# Scatter plot
plot(x, y, xlab = "Age", ylab = "Time Spent on Site", col = c("red", "purple")[group],
     main="Scatter Plot showing age in relation to time spent on Site")
legend("topleft",c("Clicked","Not clicked"), fill=c("red","purple"))
```

![](Moringa_Core_Week_12_IP_Chebet_Harrieth_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

The less the time spent on a site, the more likely one would not click
on an ad. The assumption is, once they clicked the ad, they got
redirected hence why less time was spent on the site. Those younger than
40 years were more likely to click on an ad.

``` r
# Setting a new y 
n <- advert$`Daily Internet Usage`

# Scatter plot
plot(x, n, xlab = "Age", ylab = "Time Spent on Site", col = c("red", "purple")[group1],
     main="Scatter Plot showing age in relation to Internet usage")
legend("topleft",c("Male","Female"), fill=c("red","purple"))
```

![](Moringa_Core_Week_12_IP_Chebet_Harrieth_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

Both genders are equally distributed in terms of time spent on site.

``` r
library(dplyr)
```

``` r
# Getting individuals Who are likely to click on the advertisement.
filter(advert, Age >= 40 & `Daily Time Spent on Site` < 60)
```

    ##      Daily Time Spent on Site Age Area Income Daily Internet Usage
    ##   1:                    47.64  49    45632.51               122.02
    ##   2:                    41.49  52    32635.70               164.83
    ##   3:                    41.39  41    68962.32               167.22
    ##   4:                    51.95  52    58295.82               129.23
    ##   5:                    59.05  57    25583.29               169.23
    ##  ---                                                              
    ## 181:                    41.88  40    44217.68               126.11
    ## 182:                    39.87  48    47929.83               139.34
    ## 183:                    35.79  44    33813.08               165.62
    ## 184:                    51.30  45    67782.17               134.42
    ## 185:                    51.63  51    42415.72               120.37
    ##                               Ad Topic Line               City Male
    ##   1:         Centralized neutral neural-net    West Brandonton    0
    ##   2:  Mandatory disintermediate utilization         South John    0
    ##   3:          Exclusive neutral parallelism      Harperborough    0
    ##   4:         Monitored systematic hierarchy    South Cathyfurt    0
    ##   5:            Digitized global capability North Richardburgh    1
    ##  ---                                                               
    ## 181:            Streamlined exuding adapter        Port Rachel    1
    ## 182: Business-focused user-facing benchmark      South Rebecca    1
    ## 183:         Enterprise-wide tangible model        North Katie    1
    ## 184:        Grass-roots cohesive monitoring        New Darlene    1
    ## 185:           Expanded intangible solution      South Jessica    1
    ##                     Country           Timestamp Clicked on Ad
    ##   1:                  Qatar 2016-03-16 20:19:01             1
    ##   2:                Burundi 2016-05-20 08:49:33             1
    ##   3:                Tokelau 2016-06-13 17:27:09             1
    ##   4:                 Greece 2016-07-19 08:32:10             1
    ##   5:               Maldives 2016-07-15 05:05:14             1
    ##  ---                                                         
    ## 181:                 Cyprus 2016-02-28 23:54:44             1
    ## 182:                 Mexico 2016-06-13 06:11:33             1
    ## 183:                  Tonga 2016-04-20 13:36:42             1
    ## 184: Bosnia and Herzegovina 2016-04-22 02:07:01             1
    ## 185:               Mongolia 2016-02-01 17:24:57             1

## Conclusion

185 individuals who are most likely to click on the ad have been
selected.
