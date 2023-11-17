Finding correlations to predict occurance of diabetes through clinical
predictors
================
Siyuan
2023-11-11

# Introduction

First, we need to apply the packages to be used:

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readr)
library(dplyr)
library(ggplot2)
library(plyr)
```

    ## Warning: package 'plyr' was built under R version 4.3.2

    ## ------------------------------------------------------------------------------
    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)
    ## ------------------------------------------------------------------------------
    ## 
    ## Attaching package: 'plyr'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     compact

``` r
library(scales)
```

    ## Warning: package 'scales' was built under R version 4.3.2

    ## 
    ## Attaching package: 'scales'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
library(GGally)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

``` r
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
diabetes <- read_csv("C:/NCSU/Statistics/ST558/Project 3/diabetes_binary_health_indicators_BRFSS2015.csv")
```

    ## Rows: 253680 Columns: 22
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (22): Diabetes_binary, HighBP, HighChol, CholCheck, BMI, Smoker, Stroke,...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Now we imported the data set and wanted to check what is inside:

``` r
head (diabetes)
```

    ## # A tibble: 6 × 22
    ##   Diabetes_binary HighBP HighChol CholCheck   BMI Smoker Stroke
    ##             <dbl>  <dbl>    <dbl>     <dbl> <dbl>  <dbl>  <dbl>
    ## 1               0      1        1         1    40      1      0
    ## 2               0      0        0         0    25      1      0
    ## 3               0      1        1         1    28      0      0
    ## 4               0      1        0         1    27      0      0
    ## 5               0      1        1         1    24      0      0
    ## 6               0      1        1         1    25      1      0
    ## # ℹ 15 more variables: HeartDiseaseorAttack <dbl>, PhysActivity <dbl>,
    ## #   Fruits <dbl>, Veggies <dbl>, HvyAlcoholConsump <dbl>, AnyHealthcare <dbl>,
    ## #   NoDocbcCost <dbl>, GenHlth <dbl>, MentHlth <dbl>, PhysHlth <dbl>,
    ## #   DiffWalk <dbl>, Sex <dbl>, Age <dbl>, Education <dbl>, Income <dbl>

``` r
str(diabetes)
```

    ## spc_tbl_ [253,680 × 22] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ Diabetes_binary     : num [1:253680] 0 0 0 0 0 0 0 0 1 0 ...
    ##  $ HighBP              : num [1:253680] 1 0 1 1 1 1 1 1 1 0 ...
    ##  $ HighChol            : num [1:253680] 1 0 1 0 1 1 0 1 1 0 ...
    ##  $ CholCheck           : num [1:253680] 1 0 1 1 1 1 1 1 1 1 ...
    ##  $ BMI                 : num [1:253680] 40 25 28 27 24 25 30 25 30 24 ...
    ##  $ Smoker              : num [1:253680] 1 1 0 0 0 1 1 1 1 0 ...
    ##  $ Stroke              : num [1:253680] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ HeartDiseaseorAttack: num [1:253680] 0 0 0 0 0 0 0 0 1 0 ...
    ##  $ PhysActivity        : num [1:253680] 0 1 0 1 1 1 0 1 0 0 ...
    ##  $ Fruits              : num [1:253680] 0 0 1 1 1 1 0 0 1 0 ...
    ##  $ Veggies             : num [1:253680] 1 0 0 1 1 1 0 1 1 1 ...
    ##  $ HvyAlcoholConsump   : num [1:253680] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ AnyHealthcare       : num [1:253680] 1 0 1 1 1 1 1 1 1 1 ...
    ##  $ NoDocbcCost         : num [1:253680] 0 1 1 0 0 0 0 0 0 0 ...
    ##  $ GenHlth             : num [1:253680] 5 3 5 2 2 2 3 3 5 2 ...
    ##  $ MentHlth            : num [1:253680] 18 0 30 0 3 0 0 0 30 0 ...
    ##  $ PhysHlth            : num [1:253680] 15 0 30 0 0 2 14 0 30 0 ...
    ##  $ DiffWalk            : num [1:253680] 1 0 1 0 0 0 0 1 1 0 ...
    ##  $ Sex                 : num [1:253680] 0 0 0 0 0 1 0 0 0 1 ...
    ##  $ Age                 : num [1:253680] 9 7 9 11 11 10 9 11 9 8 ...
    ##  $ Education           : num [1:253680] 4 6 4 3 5 6 6 4 5 4 ...
    ##  $ Income              : num [1:253680] 3 1 8 6 4 8 7 4 1 3 ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   Diabetes_binary = col_double(),
    ##   ..   HighBP = col_double(),
    ##   ..   HighChol = col_double(),
    ##   ..   CholCheck = col_double(),
    ##   ..   BMI = col_double(),
    ##   ..   Smoker = col_double(),
    ##   ..   Stroke = col_double(),
    ##   ..   HeartDiseaseorAttack = col_double(),
    ##   ..   PhysActivity = col_double(),
    ##   ..   Fruits = col_double(),
    ##   ..   Veggies = col_double(),
    ##   ..   HvyAlcoholConsump = col_double(),
    ##   ..   AnyHealthcare = col_double(),
    ##   ..   NoDocbcCost = col_double(),
    ##   ..   GenHlth = col_double(),
    ##   ..   MentHlth = col_double(),
    ##   ..   PhysHlth = col_double(),
    ##   ..   DiffWalk = col_double(),
    ##   ..   Sex = col_double(),
    ##   ..   Age = col_double(),
    ##   ..   Education = col_double(),
    ##   ..   Income = col_double()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

``` r
summary(diabetes)
```

    ##  Diabetes_binary      HighBP         HighChol        CholCheck     
    ##  Min.   :0.0000   Min.   :0.000   Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:1.0000  
    ##  Median :0.0000   Median :0.000   Median :0.0000   Median :1.0000  
    ##  Mean   :0.1393   Mean   :0.429   Mean   :0.4241   Mean   :0.9627  
    ##  3rd Qu.:0.0000   3rd Qu.:1.000   3rd Qu.:1.0000   3rd Qu.:1.0000  
    ##  Max.   :1.0000   Max.   :1.000   Max.   :1.0000   Max.   :1.0000  
    ##       BMI            Smoker           Stroke        HeartDiseaseorAttack
    ##  Min.   :12.00   Min.   :0.0000   Min.   :0.00000   Min.   :0.00000     
    ##  1st Qu.:24.00   1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.00000     
    ##  Median :27.00   Median :0.0000   Median :0.00000   Median :0.00000     
    ##  Mean   :28.38   Mean   :0.4432   Mean   :0.04057   Mean   :0.09419     
    ##  3rd Qu.:31.00   3rd Qu.:1.0000   3rd Qu.:0.00000   3rd Qu.:0.00000     
    ##  Max.   :98.00   Max.   :1.0000   Max.   :1.00000   Max.   :1.00000     
    ##   PhysActivity        Fruits          Veggies       HvyAlcoholConsump
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   
    ##  1st Qu.:1.0000   1st Qu.:0.0000   1st Qu.:1.0000   1st Qu.:0.0000   
    ##  Median :1.0000   Median :1.0000   Median :1.0000   Median :0.0000   
    ##  Mean   :0.7565   Mean   :0.6343   Mean   :0.8114   Mean   :0.0562   
    ##  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:0.0000   
    ##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   
    ##  AnyHealthcare     NoDocbcCost         GenHlth         MentHlth     
    ##  Min.   :0.0000   Min.   :0.00000   Min.   :1.000   Min.   : 0.000  
    ##  1st Qu.:1.0000   1st Qu.:0.00000   1st Qu.:2.000   1st Qu.: 0.000  
    ##  Median :1.0000   Median :0.00000   Median :2.000   Median : 0.000  
    ##  Mean   :0.9511   Mean   :0.08418   Mean   :2.511   Mean   : 3.185  
    ##  3rd Qu.:1.0000   3rd Qu.:0.00000   3rd Qu.:3.000   3rd Qu.: 2.000  
    ##  Max.   :1.0000   Max.   :1.00000   Max.   :5.000   Max.   :30.000  
    ##     PhysHlth         DiffWalk           Sex              Age        
    ##  Min.   : 0.000   Min.   :0.0000   Min.   :0.0000   Min.   : 1.000  
    ##  1st Qu.: 0.000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.: 6.000  
    ##  Median : 0.000   Median :0.0000   Median :0.0000   Median : 8.000  
    ##  Mean   : 4.242   Mean   :0.1682   Mean   :0.4403   Mean   : 8.032  
    ##  3rd Qu.: 3.000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:10.000  
    ##  Max.   :30.000   Max.   :1.0000   Max.   :1.0000   Max.   :13.000  
    ##    Education        Income     
    ##  Min.   :1.00   Min.   :1.000  
    ##  1st Qu.:4.00   1st Qu.:5.000  
    ##  Median :5.00   Median :7.000  
    ##  Mean   :5.05   Mean   :6.054  
    ##  3rd Qu.:6.00   3rd Qu.:8.000  
    ##  Max.   :6.00   Max.   :8.000

# Data

There are 22 variables. The variable named “Diabetes_binary” has the
value of either 0 or 1, which indicates whether a specific subject has
been diagnosed as having diabetes or not. It should be used as the
response vector. All variables are coded as numeric variables, but
obviously a majority of those variables are actually factor variables.
Next we need to transform those variables into factors to better
manipulate them.

``` r
diabetes<-diabetes%>%mutate_at(vars(Diabetes_binary, HighBP, HighChol, CholCheck, Smoker, Stroke, HeartDiseaseorAttack,PhysActivity, Fruits, Veggies, HvyAlcoholConsump, AnyHealthcare, NoDocbcCost, GenHlth,DiffWalk, Sex, Age, Education, Income), as.factor)
str(diabetes)
```

    ## tibble [253,680 × 22] (S3: tbl_df/tbl/data.frame)
    ##  $ Diabetes_binary     : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 2 1 ...
    ##  $ HighBP              : Factor w/ 2 levels "0","1": 2 1 2 2 2 2 2 2 2 1 ...
    ##  $ HighChol            : Factor w/ 2 levels "0","1": 2 1 2 1 2 2 1 2 2 1 ...
    ##  $ CholCheck           : Factor w/ 2 levels "0","1": 2 1 2 2 2 2 2 2 2 2 ...
    ##  $ BMI                 : num [1:253680] 40 25 28 27 24 25 30 25 30 24 ...
    ##  $ Smoker              : Factor w/ 2 levels "0","1": 2 2 1 1 1 2 2 2 2 1 ...
    ##  $ Stroke              : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ HeartDiseaseorAttack: Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 2 1 ...
    ##  $ PhysActivity        : Factor w/ 2 levels "0","1": 1 2 1 2 2 2 1 2 1 1 ...
    ##  $ Fruits              : Factor w/ 2 levels "0","1": 1 1 2 2 2 2 1 1 2 1 ...
    ##  $ Veggies             : Factor w/ 2 levels "0","1": 2 1 1 2 2 2 1 2 2 2 ...
    ##  $ HvyAlcoholConsump   : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ AnyHealthcare       : Factor w/ 2 levels "0","1": 2 1 2 2 2 2 2 2 2 2 ...
    ##  $ NoDocbcCost         : Factor w/ 2 levels "0","1": 1 2 2 1 1 1 1 1 1 1 ...
    ##  $ GenHlth             : Factor w/ 5 levels "1","2","3","4",..: 5 3 5 2 2 2 3 3 5 2 ...
    ##  $ MentHlth            : num [1:253680] 18 0 30 0 3 0 0 0 30 0 ...
    ##  $ PhysHlth            : num [1:253680] 15 0 30 0 0 2 14 0 30 0 ...
    ##  $ DiffWalk            : Factor w/ 2 levels "0","1": 2 1 2 1 1 1 1 2 2 1 ...
    ##  $ Sex                 : Factor w/ 2 levels "0","1": 1 1 1 1 1 2 1 1 1 2 ...
    ##  $ Age                 : Factor w/ 13 levels "1","2","3","4",..: 9 7 9 11 11 10 9 11 9 8 ...
    ##  $ Education           : Factor w/ 6 levels "1","2","3","4",..: 4 6 4 3 5 6 6 4 5 4 ...
    ##  $ Income              : Factor w/ 8 levels "1","2","3","4",..: 3 1 8 6 4 8 7 4 1 3 ...

Now all variables have been adjusted. Before doing the actual
exploratory data analysis, we wanted to focus on only one education
level just to ease the workload for computing. Merge Education level 1
and 2 into a new Education level 1

``` r
diabetes_edumerge<-diabetes%>%mutate(Edu=if_else(diabetes$Education==2,1,
                                                 if_else(diabetes$Education==1,1,
                                                         if_else(diabetes$Education==3,3,
                                                                 if_else(diabetes$Education==4,4,
                                                                         if_else(diabetes$Education==5,5,6))))))
diabetes_edumerge<-diabetes_edumerge%>%mutate_at(vars(Edu), as.factor)
diabetes_Edu1<-diabetes_edumerge%>%filter(Edu==1)
```

# Summarization

Let’s make some categorical data tables to explore the data

``` r
table(diabetes_Edu1$Diabetes_binary,diabetes_Edu1$PhysActivity)
```

    ##    
    ##        0    1
    ##   0 1230 1757
    ##   1  591  639

``` r
table(diabetes_Edu1$Diabetes_binary,diabetes_Edu1$Fruits)
```

    ##    
    ##        0    1
    ##   0 1247 1740
    ##   1  538  692

``` r
table(diabetes_Edu1$Diabetes_binary,diabetes_Edu1$Veggies)
```

    ##    
    ##        0    1
    ##   0  889 2098
    ##   1  406  824

``` r
table(diabetes_Edu1$Diabetes_binary,diabetes_Edu1$PhysHlth)
```

    ##    
    ##        0    1    2    3    4    5    6    7    8    9   10   12   13   14   15
    ##   0 1627   57  127   94   53   94   23   77   19    4   82    8    1   37  104
    ##   1  471   18   41   48   26   32   16   28   11    5   50    8    0   18   57
    ##    
    ##       16   17   18   19   20   21   22   24   25   26   27   28   29   30
    ##   0    1    0    2    1   69   16    4    1   27    3    1   11    6  438
    ##   1    1    4    2    0   52   10    2    2   17    0    2    2    4  303

``` r
table(diabetes_Edu1$Diabetes_binary,diabetes_Edu1$MentHlth)
```

    ##    
    ##        0    1    2    3    4    5    6    7    8    9   10   11   12   13   14
    ##   0 2031   49  102   72   46   83   16   39   17    1   57    1    4    1   13
    ##   1  730   18   40   30   20   31    7   19    7    1   40    0    4    0   12
    ##    
    ##       15   16   18   19   20   21   22   25   28   29   30
    ##   0   97    3    3    1   56    9    0   10    7    4  265
    ##   1   47    0    1    0   38    4    1   11    3    1  165

``` r
table(diabetes_Edu1$Diabetes_binary,diabetes_Edu1$GenHlth)
```

    ##    
    ##        1    2    3    4    5
    ##   0  243  399 1010  924  411
    ##   1   32   62  255  510  371

``` r
table(diabetes_Edu1$Diabetes_binary,diabetes_Edu1$Income)
```

    ##    
    ##       1   2   3   4   5   6   7   8
    ##   0 588 488 552 473 373 252 131 130
    ##   1 349 278 216 150 127  59  26  25

``` r
table(diabetes_Edu1$Diabetes_binary,diabetes_Edu1$HvyAlcoholConsump)
```

    ##    
    ##        0    1
    ##   0 2888   99
    ##   1 1217   13

## BAR PLOTS

It looks like diabetes incidence is positively correlated with PhysHlth,
MentHlth and GenHlth, negatively corrlated with PhysActivity, Income and
suprisingly, HvyAlcoholConsump and weakly assoiciated with Fruit and
Veggie consumpution.# We decided to make barplot for each of them and
put them together in the same grid.

``` r
PHlthplot<-ggplot(diabetes_Edu1,aes(x=PhysHlth,fill=Diabetes_binary))+
  geom_density(adjust=1,alpha=0.5)+
  scale_fill_discrete(name="Diabetic?", label=c("No","Yes"))+
  labs(x="Number of days physical health not good")
MHlthplot<-ggplot(diabetes_Edu1,aes(x=MentHlth,fill=Diabetes_binary))+
  geom_density(adjust=1,alpha=0.5)+
  scale_fill_discrete(name="Diabetic?", label=c("No","Yes"))+
  labs(x="Number of days mental health not good")
Physplot<-ggplot(diabetes_Edu1,aes(x=PhysActivity))+
  geom_bar(aes(fill=(Diabetes_binary)))+
  labs(x="Physically Active?")+
  scale_x_discrete(labels=c("No","Yes"))+
  scale_fill_discrete(name="Diabetic?", label=c("No","Yes"))
Fruitplot<-ggplot(diabetes_Edu1,aes(x=Fruits))+
  geom_bar(aes(fill=(Diabetes_binary)))+
  labs(x="Fruit lover?")+
  scale_x_discrete(labels=c("No","Yes"))+
  scale_fill_discrete(name="Diabetic?", label=c("No","Yes"))
Veggieplot<-ggplot(diabetes_Edu1,aes(x=Veggies))+
  geom_bar(aes(fill=(Diabetes_binary)))+
  labs(x="Veggies eater?")+
  scale_x_discrete(labels=c("No","Yes"))+
  scale_fill_discrete(name="Diabetic?", label=c("No","Yes"))
GHlthplot<-ggplot(diabetes_Edu1,aes(x=GenHlth))+
  geom_bar(aes(fill=(Diabetes_binary)))+
  labs(x="General Health Condition")+
  scale_x_discrete(labels=c("E","VG", "G", "F", "P"))+
  scale_fill_discrete(name="Diabetic?", label=c("No","Yes"))
Incomeplot<-ggplot(diabetes_Edu1,aes(x=Income))+
  geom_bar(aes(fill=(Diabetes_binary)))+
  labs(x="Annual Household Income level")+
  scale_x_discrete(labels=c("1","2", "3","4", "5","6","7", "8"))+
  scale_fill_discrete(name="Diabetic?", label=c("No","Yes"))
Alcoholplot<-ggplot(diabetes_Edu1,aes(x=HvyAlcoholConsump))+
  geom_bar(aes(fill=(Diabetes_binary)))+
  labs(x="Heavy Drinker?")+
  scale_x_discrete(labels=c("No","Yes"))+
  scale_fill_discrete(name="Diabetic?", label=c("No","Yes"))
```

It would be very curbumsome to view each graph indenpendently, so put
them in a tile

``` r
grid.arrange(Physplot,Fruitplot,Veggieplot,GHlthplot,Alcoholplot,Incomeplot,PHlthplot,MHlthplot,ncol=3)
```

![](work_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

It is a bit surprising to me to see eating more fruit and veggies has
little impact on one’s chance to get diabetes. Split the data according
to Fruit/veggie

``` r
diabetes_Edu1_nofruit<-diabetes_Edu1%>%filter(Fruits=="0")
diabetes_Edu1_yesfruit<-diabetes_Edu1%>%filter(Fruits=="1")
diabetes_Edu1_noveggie<-diabetes_Edu1%>%filter(Veggies=="0")
diabetes_Edu1_yesveggie<-diabetes_Edu1%>%filter(Veggies=="1")
Physnofruitplot<-ggplot(diabetes_Edu1_nofruit,aes(x=PhysActivity))+
  geom_bar(aes(fill=(Diabetes_binary)))+
  ggtitle("Fruit hater")+
  labs(x="Physically Active?")+
  scale_x_discrete(labels=c("No","Yes"))+
  scale_fill_discrete(name="Diabetic?", label=c("No","Yes"))
Physyesfruitplot<-ggplot(diabetes_Edu1_yesfruit,aes(x=PhysActivity))+
  geom_bar(aes(fill=(Diabetes_binary)))+
  ggtitle("Fruit lover")+
  labs(x="Physically Active?")+
  scale_x_discrete(labels=c("No","Yes"))+
  scale_fill_discrete(name="Diabetic?", label=c("No","Yes"))
Physnoveggieplot<-ggplot(diabetes_Edu1_noveggie,aes(x=PhysActivity))+
  geom_bar(aes(fill=(Diabetes_binary)))+
  ggtitle("Veggie hater")+
  labs(x="Physically Active?")+
  scale_x_discrete(labels=c("No","Yes"))+
  scale_fill_discrete(name="Diabetic?", label=c("No","Yes"))
Physyesveggieplot<-ggplot(diabetes_Edu1_yesveggie,aes(x=PhysActivity))+
  geom_bar(aes(fill=(Diabetes_binary)))+
  ggtitle("Veggie lover")+
  labs(x="Physically Active?")+
  scale_x_discrete(labels=c("No","Yes"))+
  scale_fill_discrete(name="Diabetic?", label=c("No","Yes"))
```

``` r
grid.arrange(Physnofruitplot,Physyesfruitplot,Physnoveggieplot,Physyesveggieplot)
```

![](work_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## CORRELATION PLOTS

Obviously Fruit/Veggie+Physical activity may prevent diabetes, at least
among people of lower education.Howl-ever, people who are fruit and
veggie lovers while keeping physically active maybe likely to have more
income, which also confound the factors. To straighten this out, and
also for the purpose of modelling, we need to find a way to characterize
the correlation among variables, especially those categorical ones. Need
to transform the data set back into numeric dataframe.

``` r
diabetes_Edu1_n<-diabetes_Edu1%>%mutate_at(vars(Diabetes_binary, HighBP, HighChol, CholCheck, Smoker, Stroke, HeartDiseaseorAttack,PhysActivity, Fruits, Veggies, HvyAlcoholConsump, AnyHealthcare, NoDocbcCost, GenHlth,DiffWalk, Sex, Age, Education, Income, Edu),as.numeric)%>%select(-Education, -Edu)
Correlation<-cor(diabetes_Edu1_n)
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
corrplot(Correlation,type="upper",method="color", addrect=2, tl.cex=0.6, cl.cex=0.6,tl.pos="lt")
corrplot(Correlation,type="lower",method="number",add=TRUE, diag=FALSE, tl.pos="n",number.cex = 0.8, number.digits = 1)
```

![](work_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

From the correlation plot, we could see that diabetes occurance is not
strongly associated with above mentioned factors, except for general
health status and physical health. Across all variables, the most
strongly postive associations ocurred between PhysHlth and GenHlth,
which explains for itself. The factor that correlates with Diabetes
status is High BP and bad General Health.

# Modelling

## PREPROCESSING DATA

Before modeling, we need to divide the dataset into training and testing
dataset.

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 4.3.2

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(glmnet)
```

    ## Warning: package 'glmnet' was built under R version 4.3.2

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

    ## Loaded glmnet 4.1-8

``` r
library(forecast)
```

    ## Warning: package 'forecast' was built under R version 4.3.2

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

``` r
set.seed(20)
index <- createDataPartition(diabetes_Edu1$Diabetes_binary, p = 0.70, list = FALSE)
train <- diabetes_Edu1[index, ]
test <- diabetes_Edu1[-index, ]
```

## LASSO LOGISTIC REGRESSION

Compared with basic logistric regression, LASSO logistic regression
model help with the selection of variables for prediction without prior
knowledge. For this data set, the correlation between diabetes and its
predictors are relatively weak, so try the LASSO method to help select
variables. LASSO regression model uses a panelty based method to panelty
against using irrelavant predictors and also could prevent over-fitting
because LASSO could reduce the “weight” for each predictor. Using the
caret package to fit a model to the training set. Try the Lasso
logistric regression

``` r
train<-train%>%mutate(Diabetes_binary=factor(Diabetes_binary,
                      labels = make.names(levels(Diabetes_binary))))
test<-test%>%mutate(Diabetes_binary=factor(Diabetes_binary,
                      labels = make.names(levels(Diabetes_binary))))
```

``` r
lasso_log_reg<-train(Diabetes_binary~HighBP + HighChol + CholCheck + BMI + Smoker + Stroke + HeartDiseaseorAttack+ PhysActivity + Fruits + Veggies + HvyAlcoholConsump + AnyHealthcare + NoDocbcCost + GenHlth + MentHlth ,
                     data=train,
                     method="glmnet",
                     metric="logLoss",
                     preProcess=c("center","scale"),
                     trControl=trainControl(method="cv", number = 5, classProbs=TRUE, summaryFunction=mnLogLoss),
                     tuneGrid = expand.grid(alpha = seq(0,1,by =0.1),
                                            lambda = seq (0,1,by=0.1)))
head(lasso_log_reg$results)
```

    ##   alpha lambda   logLoss   logLossSD
    ## 1     0    0.0 0.5174215 0.010699362
    ## 2     0    0.1 0.5230186 0.008455558
    ## 3     0    0.2 0.5308069 0.006792170
    ## 4     0    0.3 0.5376337 0.005723992
    ## 5     0    0.4 0.5434101 0.004968930
    ## 6     0    0.5 0.5483034 0.004401285

``` r
lasso_log_reg$bestTune
```

    ##     alpha lambda
    ## 111     1      0

``` r
plot(lasso_log_reg)
```

![](work_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

When alpha = 0.7, lambda =0, we could get the best fit for the training
data set using plot(lasso_log_reg). The graph demonstrates that the max
accuracy is observed for alpha =0.1, given choosing lambda =
0.0024742657. Now we could calculate the accuracy for the test dataset
using the same settings.

``` r
fitted_lasso<-data.frame(obs=test$Diabetes_binary,
                         pred=predict(lasso_log_reg,test),
                        predict(lasso_log_reg,test,type = "prob"))
c <- mnLogLoss(fitted_lasso, lev = levels(fitted_lasso$obs))
fitted_lasso
```

    ##      obs pred        X0          X1
    ## 1     X0   X1 0.3686557 0.631344253
    ## 2     X0   X0 0.6391783 0.360821708
    ## 3     X0   X0 0.7870639 0.212936069
    ## 4     X0   X0 0.5138106 0.486189436
    ## 5     X0   X0 0.7122993 0.287700702
    ## 6     X0   X0 0.9483448 0.051655226
    ## 7     X0   X0 0.7009377 0.299062338
    ## 8     X1   X0 0.5081016 0.491898400
    ## 9     X0   X0 0.8694504 0.130549641
    ## 10    X1   X1 0.3869556 0.613044407
    ## 11    X0   X1 0.3638948 0.636105208
    ## 12    X0   X0 0.7359295 0.264070544
    ## 13    X1   X1 0.4531319 0.546868058
    ## 14    X1   X0 0.5996195 0.400380526
    ## 15    X1   X0 0.6339805 0.366019450
    ## 16    X0   X0 0.5909628 0.409037234
    ## 17    X0   X0 0.6239094 0.376090579
    ## 18    X1   X0 0.6362673 0.363732679
    ## 19    X0   X1 0.3550876 0.644912431
    ## 20    X0   X0 0.7927408 0.207259192
    ## 21    X0   X0 0.7076106 0.292389404
    ## 22    X0   X0 0.7081112 0.291888758
    ## 23    X0   X0 0.6095898 0.390410236
    ## 24    X1   X0 0.6052025 0.394797519
    ## 25    X1   X0 0.5166819 0.483318138
    ## 26    X0   X0 0.7394656 0.260534360
    ## 27    X0   X0 0.8466707 0.153329285
    ## 28    X0   X0 0.6848756 0.315124388
    ## 29    X0   X0 0.7015343 0.298465661
    ## 30    X1   X0 0.8521615 0.147838490
    ## 31    X1   X0 0.9052216 0.094778411
    ## 32    X0   X0 0.9563284 0.043671561
    ## 33    X1   X1 0.4132114 0.586788598
    ## 34    X0   X0 0.5244015 0.475598532
    ## 35    X1   X0 0.8119595 0.188040472
    ## 36    X0   X0 0.7062920 0.293708005
    ## 37    X0   X0 0.8299773 0.170022737
    ## 38    X0   X1 0.3858833 0.614116732
    ## 39    X1   X0 0.8374091 0.162590932
    ## 40    X1   X1 0.2571084 0.742891593
    ## 41    X1   X1 0.3733445 0.626655499
    ## 42    X0   X0 0.8790774 0.120922637
    ## 43    X0   X0 0.6730931 0.326906878
    ## 44    X0   X0 0.9189772 0.081022805
    ## 45    X0   X0 0.7488096 0.251190415
    ## 46    X0   X0 0.9383197 0.061680281
    ## 47    X0   X0 0.9398645 0.060135500
    ## 48    X0   X0 0.7020381 0.297961927
    ## 49    X0   X0 0.9341192 0.065880795
    ## 50    X0   X0 0.5545716 0.445428418
    ## 51    X0   X0 0.9418692 0.058130752
    ## 52    X1   X1 0.4791839 0.520816052
    ## 53    X0   X0 0.6628452 0.337154769
    ## 54    X0   X0 0.9145923 0.085407681
    ## 55    X1   X1 0.3184992 0.681500825
    ## 56    X0   X0 0.7844575 0.215542529
    ## 57    X1   X0 0.7393986 0.260601368
    ## 58    X0   X1 0.3028774 0.697122608
    ## 59    X1   X1 0.4383239 0.561676122
    ## 60    X0   X0 0.7628509 0.237149064
    ## 61    X0   X0 0.6479056 0.352094389
    ## 62    X1   X1 0.2310796 0.768920371
    ## 63    X1   X0 0.8591972 0.140802810
    ## 64    X1   X1 0.4793767 0.520623288
    ## 65    X1   X1 0.3825272 0.617472773
    ## 66    X1   X0 0.5191676 0.480832437
    ## 67    X0   X0 0.8110881 0.188911866
    ## 68    X0   X0 0.8835281 0.116471875
    ## 69    X1   X1 0.4531319 0.546868058
    ## 70    X0   X1 0.4382115 0.561788516
    ## 71    X0   X0 0.8781486 0.121851447
    ## 72    X1   X0 0.6780331 0.321966937
    ## 73    X0   X0 0.8955729 0.104427064
    ## 74    X1   X0 0.5223117 0.477688309
    ## 75    X1   X0 0.5989302 0.401069771
    ## 76    X0   X0 0.8476364 0.152363635
    ## 77    X1   X0 0.5912280 0.408771988
    ## 78    X1   X1 0.3300487 0.669951325
    ## 79    X0   X0 0.9554839 0.044516074
    ## 80    X0   X0 0.8833974 0.116602592
    ## 81    X0   X0 0.5500539 0.449946141
    ## 82    X1   X1 0.3945794 0.605420640
    ## 83    X0   X0 0.9328625 0.067137522
    ## 84    X1   X0 0.5890440 0.410955952
    ## 85    X1   X0 0.6603712 0.339628785
    ## 86    X1   X0 0.8208173 0.179182698
    ## 87    X0   X0 0.9095989 0.090401071
    ## 88    X1   X0 0.6301218 0.369878225
    ## 89    X0   X0 0.8591407 0.140859259
    ## 90    X0   X1 0.4815339 0.518466091
    ## 91    X0   X0 0.7436225 0.256377498
    ## 92    X1   X0 0.9524502 0.047549788
    ## 93    X0   X0 0.5342822 0.465717828
    ## 94    X0   X1 0.4575268 0.542473246
    ## 95    X0   X0 0.7511212 0.248878828
    ## 96    X1   X0 0.5729128 0.427087172
    ## 97    X0   X0 0.6814105 0.318589489
    ## 98    X0   X0 0.6864755 0.313524486
    ## 99    X0   X0 0.9391072 0.060892825
    ## 100   X0   X0 0.9238499 0.076150117
    ## 101   X0   X0 0.8972186 0.102781355
    ## 102   X0   X0 0.6520016 0.347998354
    ## 103   X1   X0 0.6821164 0.317883629
    ## 104   X0   X0 0.5385498 0.461450230
    ## 105   X0   X0 0.7966913 0.203308749
    ## 106   X0   X0 0.9061511 0.093848884
    ## 107   X0   X0 0.9187810 0.081218972
    ## 108   X0   X0 0.6581206 0.341879352
    ## 109   X0   X0 0.7887385 0.211261549
    ## 110   X0   X0 0.8977830 0.102217040
    ## 111   X0   X0 0.7251268 0.274873211
    ## 112   X0   X0 0.5820022 0.417997819
    ## 113   X0   X1 0.4705433 0.529456688
    ## 114   X0   X0 0.7298379 0.270162059
    ## 115   X1   X1 0.4254986 0.574501409
    ## 116   X0   X0 0.6130919 0.386908052
    ## 117   X0   X0 0.9427702 0.057229753
    ## 118   X1   X0 0.7338321 0.266167885
    ## 119   X1   X0 0.7033384 0.296661583
    ## 120   X1   X0 0.6243906 0.375609395
    ## 121   X1   X0 0.8564016 0.143598374
    ## 122   X0   X0 0.5688182 0.431181778
    ## 123   X0   X0 0.5124647 0.487535314
    ## 124   X0   X0 0.7316884 0.268311598
    ## 125   X0   X0 0.9770440 0.022956030
    ## 126   X0   X0 0.9287778 0.071222219
    ## 127   X0   X0 0.7963296 0.203670407
    ## 128   X0   X0 0.7075474 0.292452553
    ## 129   X0   X0 0.8975519 0.102448139
    ## 130   X0   X0 0.8384689 0.161531111
    ## 131   X0   X0 0.8658971 0.134102938
    ## 132   X0   X0 0.6870156 0.312984374
    ## 133   X0   X0 0.9518797 0.048120325
    ## 134   X1   X0 0.5507229 0.449277108
    ## 135   X0   X0 0.8902413 0.109758749
    ## 136   X0   X0 0.7582844 0.241715572
    ## 137   X1   X0 0.6794825 0.320517500
    ## 138   X0   X0 0.8654629 0.134537071
    ## 139   X0   X0 0.8872596 0.112740386
    ## 140   X0   X0 0.9344836 0.065516385
    ## 141   X1   X0 0.5083943 0.491605734
    ## 142   X1   X0 0.6978209 0.302179062
    ## 143   X1   X1 0.3903878 0.609612232
    ## 144   X1   X0 0.7157225 0.284277546
    ## 145   X0   X0 0.7889074 0.211092642
    ## 146   X0   X0 0.9284707 0.071529271
    ## 147   X0   X0 0.9291635 0.070836518
    ## 148   X0   X0 0.7857852 0.214214792
    ## 149   X1   X0 0.6856352 0.314364776
    ## 150   X0   X0 0.9254051 0.074594875
    ## 151   X0   X0 0.9348971 0.065102894
    ## 152   X0   X0 0.8926925 0.107307503
    ## 153   X1   X0 0.8150374 0.184962582
    ## 154   X1   X0 0.5711875 0.428812544
    ## 155   X0   X0 0.7180543 0.281945672
    ## 156   X0   X0 0.9266916 0.073308426
    ## 157   X0   X0 0.6405740 0.359426014
    ## 158   X0   X0 0.8278013 0.172198707
    ## 159   X0   X0 0.5358155 0.464184476
    ## 160   X0   X0 0.9337755 0.066224529
    ## 161   X0   X0 0.7626039 0.237396053
    ## 162   X1   X1 0.4974594 0.502540584
    ## 163   X1   X0 0.5679358 0.432064220
    ## 164   X0   X0 0.7950231 0.204976911
    ## 165   X0   X0 0.7834329 0.216567098
    ## 166   X0   X0 0.9200674 0.079932628
    ## 167   X0   X0 0.8987454 0.101254636
    ## 168   X0   X1 0.4762664 0.523733614
    ## 169   X0   X0 0.7892938 0.210706237
    ## 170   X0   X0 0.9079209 0.092079139
    ## 171   X0   X0 0.6057431 0.394256932
    ## 172   X1   X0 0.8509884 0.149011571
    ## 173   X0   X0 0.9065304 0.093469555
    ## 174   X0   X0 0.8940304 0.105969619
    ## 175   X0   X0 0.9265491 0.073450904
    ## 176   X0   X0 0.9143646 0.085635370
    ## 177   X0   X0 0.9412076 0.058792397
    ## 178   X0   X0 0.9434242 0.056575800
    ## 179   X0   X0 0.9853775 0.014622537
    ## 180   X0   X0 0.9304102 0.069589760
    ## 181   X0   X0 0.5937367 0.406263313
    ## 182   X0   X0 0.9485797 0.051420296
    ## 183   X0   X0 0.7892938 0.210706237
    ## 184   X1   X0 0.7639293 0.236070707
    ## 185   X0   X0 0.9188708 0.081129168
    ## 186   X1   X0 0.7832900 0.216710019
    ## 187   X0   X0 0.9575456 0.042454393
    ## 188   X0   X0 0.8743657 0.125634337
    ## 189   X0   X0 0.9035613 0.096438664
    ## 190   X0   X0 0.9429807 0.057019275
    ## 191   X0   X0 0.9234038 0.076596218
    ## 192   X0   X0 0.6340071 0.365992925
    ## 193   X0   X0 0.8122413 0.187758701
    ## 194   X0   X0 0.8919573 0.108042673
    ## 195   X0   X0 0.6421851 0.357814923
    ## 196   X0   X0 0.8668554 0.133144585
    ## 197   X0   X0 0.9268649 0.073135112
    ## 198   X1   X0 0.8415568 0.158443211
    ## 199   X1   X0 0.8162709 0.183729101
    ## 200   X0   X0 0.8537352 0.146264804
    ## 201   X1   X0 0.7432360 0.256764004
    ## 202   X0   X0 0.6790393 0.320960651
    ## 203   X0   X0 0.7187672 0.281232810
    ## 204   X0   X0 0.6052802 0.394719772
    ## 205   X0   X0 0.9156532 0.084346797
    ## 206   X0   X0 0.8736777 0.126322311
    ## 207   X0   X0 0.6124340 0.387566039
    ## 208   X0   X0 0.7642754 0.235724644
    ## 209   X0   X0 0.6872785 0.312721480
    ## 210   X0   X0 0.6837933 0.316206745
    ## 211   X0   X0 0.7694397 0.230560262
    ## 212   X0   X0 0.9060350 0.093965011
    ## 213   X0   X1 0.4948187 0.505181346
    ## 214   X0   X0 0.8442556 0.155744422
    ## 215   X0   X0 0.9176924 0.082307626
    ## 216   X0   X0 0.7697362 0.230263774
    ## 217   X0   X0 0.7832866 0.216713398
    ## 218   X0   X0 0.5803132 0.419686821
    ## 219   X1   X0 0.8485714 0.151428610
    ## 220   X1   X0 0.5163258 0.483674228
    ## 221   X1   X0 0.6919569 0.308043081
    ## 222   X0   X0 0.9001948 0.099805230
    ## 223   X1   X1 0.4893063 0.510693666
    ## 224   X0   X0 0.6727361 0.327263916
    ## 225   X1   X1 0.4324674 0.567532597
    ## 226   X0   X0 0.7907161 0.209283920
    ## 227   X0   X0 0.8638264 0.136173630
    ## 228   X1   X0 0.6031667 0.396833341
    ## 229   X1   X1 0.3223916 0.677608365
    ## 230   X0   X0 0.9312961 0.068703904
    ## 231   X0   X0 0.9146935 0.085306510
    ## 232   X0   X0 0.8256397 0.174360254
    ## 233   X1   X0 0.5524815 0.447518474
    ## 234   X0   X0 0.8810446 0.118955401
    ## 235   X0   X0 0.8907263 0.109273681
    ## 236   X0   X0 0.8908525 0.109147525
    ## 237   X0   X0 0.8962835 0.103716543
    ## 238   X0   X0 0.8354184 0.164581577
    ## 239   X0   X1 0.4458528 0.554147184
    ## 240   X0   X0 0.8327501 0.167249932
    ## 241   X0   X0 0.9324436 0.067556411
    ## 242   X0   X0 0.7023962 0.297603821
    ## 243   X0   X0 0.8180526 0.181947425
    ## 244   X0   X1 0.3845675 0.615432539
    ## 245   X1   X0 0.6190562 0.380943816
    ## 246   X1   X0 0.7943821 0.205617925
    ## 247   X1   X1 0.4786546 0.521345441
    ## 248   X1   X0 0.8472962 0.152703827
    ## 249   X0   X0 0.9341994 0.065800579
    ## 250   X1   X0 0.6383371 0.361662930
    ## 251   X1   X1 0.3322356 0.667764419
    ## 252   X0   X0 0.8303819 0.169618102
    ## 253   X0   X1 0.3903541 0.609645898
    ## 254   X0   X1 0.4145766 0.585423385
    ## 255   X0   X0 0.7305523 0.269447749
    ## 256   X0   X0 0.5114535 0.488546477
    ## 257   X1   X0 0.8466958 0.153304177
    ## 258   X0   X0 0.9330167 0.066983282
    ## 259   X1   X0 0.7209910 0.279008967
    ## 260   X1   X0 0.9842049 0.015795055
    ## 261   X1   X0 0.5729585 0.427041458
    ## 262   X0   X0 0.9503349 0.049665078
    ## 263   X1   X1 0.4252507 0.574749252
    ## 264   X0   X0 0.9029419 0.097058138
    ## 265   X0   X0 0.8772246 0.122775413
    ## 266   X0   X0 0.7985850 0.201415011
    ## 267   X1   X0 0.5561533 0.443846663
    ## 268   X0   X0 0.7409809 0.259019123
    ## 269   X0   X0 0.8285316 0.171468381
    ## 270   X0   X0 0.9155699 0.084430090
    ## 271   X0   X0 0.9334884 0.066511577
    ## 272   X1   X0 0.5757055 0.424294497
    ## 273   X0   X0 0.6145360 0.385463987
    ## 274   X0   X0 0.9422192 0.057780804
    ## 275   X0   X1 0.3336263 0.666373678
    ## 276   X0   X0 0.7956095 0.204390550
    ## 277   X0   X0 0.5100744 0.489925565
    ## 278   X0   X0 0.9807545 0.019245460
    ## 279   X0   X0 0.9142796 0.085720377
    ## 280   X0   X0 0.6733570 0.326643003
    ## 281   X1   X1 0.4671260 0.532874023
    ## 282   X1   X0 0.5624271 0.437572942
    ## 283   X0   X0 0.9740245 0.025975547
    ## 284   X0   X0 0.7208371 0.279162938
    ## 285   X0   X0 0.8378825 0.162117464
    ## 286   X0   X0 0.6643011 0.335698938
    ## 287   X1   X1 0.2693415 0.730658451
    ## 288   X1   X1 0.4096712 0.590328815
    ## 289   X0   X0 0.8365938 0.163406154
    ## 290   X1   X0 0.5160206 0.483979389
    ## 291   X0   X1 0.4841099 0.515890144
    ## 292   X0   X0 0.5211958 0.478804166
    ## 293   X0   X1 0.4676983 0.532301705
    ## 294   X1   X1 0.4430735 0.556926522
    ## 295   X1   X0 0.6519375 0.348062521
    ## 296   X0   X0 0.5532066 0.446793417
    ## 297   X1   X0 0.7732829 0.226717088
    ## 298   X0   X0 0.8954923 0.104507690
    ## 299   X0   X1 0.4355635 0.564436525
    ## 300   X1   X0 0.5786239 0.421376106
    ## 301   X0   X0 0.5640853 0.435914710
    ## 302   X0   X1 0.4352649 0.564735067
    ## 303   X0   X0 0.5355423 0.464457713
    ## 304   X0   X0 0.7553522 0.244647753
    ## 305   X0   X0 0.9463876 0.053612378
    ## 306   X0   X0 0.7468154 0.253184629
    ## 307   X0   X0 0.9496388 0.050361189
    ## 308   X0   X0 0.5416354 0.458364623
    ## 309   X0   X0 0.8976773 0.102322712
    ## 310   X1   X0 0.5934688 0.406531227
    ## 311   X0   X0 0.8265780 0.173421996
    ## 312   X1   X1 0.3201422 0.679857805
    ## 313   X0   X0 0.6868829 0.313117143
    ## 314   X0   X0 0.8359698 0.164030186
    ## 315   X0   X0 0.9270785 0.072921531
    ## 316   X1   X0 0.7311706 0.268829388
    ## 317   X0   X0 0.8749442 0.125055836
    ## 318   X0   X0 0.9011785 0.098821481
    ## 319   X0   X0 0.8555262 0.144473774
    ## 320   X0   X0 0.7716387 0.228361308
    ## 321   X1   X0 0.5201699 0.479830071
    ## 322   X0   X0 0.9122678 0.087732234
    ## 323   X0   X0 0.9682338 0.031766224
    ## 324   X1   X0 0.6209682 0.379031782
    ## 325   X0   X0 0.7708299 0.229170108
    ## 326   X1   X0 0.6057537 0.394246348
    ## 327   X1   X1 0.4132114 0.586788598
    ## 328   X0   X0 0.7803115 0.219688465
    ## 329   X0   X0 0.9664350 0.033564970
    ## 330   X0   X0 0.7189323 0.281067660
    ## 331   X0   X0 0.5475891 0.452410930
    ## 332   X1   X0 0.8909909 0.109009126
    ## 333   X0   X0 0.9159849 0.084015056
    ## 334   X0   X0 0.9435610 0.056438973
    ## 335   X1   X0 0.7436805 0.256319482
    ## 336   X0   X1 0.3583438 0.641656209
    ## 337   X1   X0 0.5755218 0.424478186
    ## 338   X0   X0 0.6113665 0.388633485
    ## 339   X0   X0 0.9713237 0.028676348
    ## 340   X0   X0 0.9044790 0.095520963
    ## 341   X0   X0 0.9682649 0.031735078
    ## 342   X0   X0 0.9018959 0.098104050
    ## 343   X1   X0 0.8675975 0.132402482
    ## 344   X1   X0 0.5272933 0.472706707
    ## 345   X0   X0 0.6991191 0.300880930
    ## 346   X0   X0 0.7878551 0.212144906
    ## 347   X0   X0 0.8195269 0.180473080
    ## 348   X0   X1 0.4362958 0.563704162
    ## 349   X0   X0 0.7196612 0.280338760
    ## 350   X0   X1 0.4978149 0.502185060
    ## 351   X0   X0 0.8431436 0.156856356
    ## 352   X0   X0 0.6724944 0.327505570
    ## 353   X0   X0 0.6353474 0.364652598
    ## 354   X0   X0 0.7426834 0.257316617
    ## 355   X1   X0 0.9555443 0.044455693
    ## 356   X0   X0 0.5653392 0.434660772
    ## 357   X0   X0 0.7886101 0.211389929
    ## 358   X0   X0 0.8127966 0.187203351
    ## 359   X0   X0 0.8674595 0.132540488
    ## 360   X0   X0 0.7771986 0.222801389
    ## 361   X1   X1 0.4639486 0.536051390
    ## 362   X1   X0 0.8511220 0.148878021
    ## 363   X1   X0 0.6183808 0.381619191
    ## 364   X0   X0 0.9344436 0.065556371
    ## 365   X1   X1 0.3912314 0.608768607
    ## 366   X0   X1 0.4815748 0.518425167
    ## 367   X0   X0 0.8039257 0.196074289
    ## 368   X0   X0 0.9424112 0.057588815
    ## 369   X1   X0 0.7642983 0.235701685
    ## 370   X0   X0 0.9025136 0.097486363
    ## 371   X1   X0 0.6973206 0.302679439
    ## 372   X0   X0 0.9586979 0.041302142
    ## 373   X0   X1 0.3341746 0.665825360
    ## 374   X0   X0 0.8108579 0.189142088
    ## 375   X1   X0 0.5715926 0.428407374
    ## 376   X0   X0 0.7909885 0.209011470
    ## 377   X0   X0 0.9867545 0.013245501
    ## 378   X0   X0 0.8558298 0.144170220
    ## 379   X0   X0 0.8688894 0.131110556
    ## 380   X0   X0 0.7121354 0.287864600
    ## 381   X0   X0 0.7444942 0.255505806
    ## 382   X1   X1 0.3746774 0.625322616
    ## 383   X0   X0 0.8019835 0.198016499
    ## 384   X0   X0 0.5171659 0.482834138
    ## 385   X1   X0 0.6089559 0.391044077
    ## 386   X0   X0 0.5410404 0.458959619
    ## 387   X0   X0 0.9069303 0.093069690
    ## 388   X1   X0 0.5445334 0.455466614
    ## 389   X1   X0 0.5348302 0.465169848
    ## 390   X0   X0 0.9459081 0.054091946
    ## 391   X0   X0 0.7943102 0.205689803
    ## 392   X0   X0 0.5162426 0.483757418
    ## 393   X0   X0 0.8627561 0.137243851
    ## 394   X0   X0 0.7942039 0.205796080
    ## 395   X0   X0 0.7416573 0.258342717
    ## 396   X0   X0 0.9403205 0.059679545
    ## 397   X0   X0 0.7329452 0.267054787
    ## 398   X0   X0 0.7602477 0.239752326
    ## 399   X0   X0 0.9032863 0.096713678
    ## 400   X0   X0 0.6714733 0.328526685
    ## 401   X0   X0 0.7501551 0.249844949
    ## 402   X1   X0 0.8460604 0.153939557
    ## 403   X1   X1 0.3286535 0.671346540
    ## 404   X1   X0 0.5276045 0.472395532
    ## 405   X0   X0 0.9434449 0.056555129
    ## 406   X0   X0 0.9790965 0.020903481
    ## 407   X0   X0 0.9041377 0.095862303
    ## 408   X1   X1 0.4338276 0.566172436
    ## 409   X0   X0 0.8729530 0.127047004
    ## 410   X0   X0 0.7346294 0.265370602
    ## 411   X0   X0 0.8448599 0.155140089
    ## 412   X0   X1 0.4624339 0.537566143
    ## 413   X0   X1 0.2084413 0.791558710
    ## 414   X0   X1 0.3914601 0.608539874
    ## 415   X0   X0 0.7416306 0.258369410
    ## 416   X0   X0 0.8909113 0.109088688
    ## 417   X0   X0 0.9114768 0.088523160
    ## 418   X1   X0 0.6866197 0.313380340
    ## 419   X0   X0 0.7170296 0.282970362
    ## 420   X0   X1 0.3314743 0.668525734
    ## 421   X0   X0 0.9422212 0.057778815
    ## 422   X0   X0 0.7723422 0.227657844
    ## 423   X0   X0 0.7785561 0.221443927
    ## 424   X0   X0 0.8754423 0.124557660
    ## 425   X0   X0 0.9807048 0.019295196
    ## 426   X0   X1 0.4356925 0.564307516
    ## 427   X1   X0 0.6441723 0.355827729
    ## 428   X0   X0 0.9354201 0.064579873
    ## 429   X0   X0 0.7559512 0.244048783
    ## 430   X1   X0 0.5121765 0.487823454
    ## 431   X1   X0 0.7267087 0.273291266
    ## 432   X1   X0 0.5464915 0.453508516
    ## 433   X0   X1 0.4318041 0.568195851
    ## 434   X0   X0 0.7236773 0.276322749
    ## 435   X0   X0 0.9096894 0.090310614
    ## 436   X0   X0 0.6481320 0.351868018
    ## 437   X0   X0 0.7114048 0.288595221
    ## 438   X0   X0 0.9094206 0.090579437
    ## 439   X0   X0 0.6992965 0.300703488
    ## 440   X0   X1 0.3870977 0.612902311
    ## 441   X1   X0 0.7248970 0.275102975
    ## 442   X0   X1 0.3969906 0.603009407
    ## 443   X1   X0 0.8300497 0.169950349
    ## 444   X0   X0 0.5403360 0.459663994
    ## 445   X0   X0 0.6124082 0.387591752
    ## 446   X0   X0 0.9218384 0.078161643
    ## 447   X0   X0 0.9291085 0.070891492
    ## 448   X0   X0 0.6340290 0.365970967
    ## 449   X0   X1 0.4556463 0.544353737
    ## 450   X0   X1 0.4160733 0.583926655
    ## 451   X1   X0 0.5000816 0.499918388
    ## 452   X0   X0 0.8080415 0.191958537
    ## 453   X0   X0 0.5957401 0.404259938
    ## 454   X0   X0 0.6947006 0.305299395
    ## 455   X1   X1 0.3780329 0.621967136
    ## 456   X1   X1 0.3011448 0.698855212
    ## 457   X0   X0 0.5675036 0.432496433
    ## 458   X1   X1 0.3726788 0.627321174
    ## 459   X0   X0 0.6290140 0.370985983
    ## 460   X0   X0 0.6702355 0.329764541
    ## 461   X1   X1 0.4979837 0.502016292
    ## 462   X0   X0 0.6982818 0.301718233
    ## 463   X1   X0 0.5010140 0.498985978
    ## 464   X1   X1 0.2342821 0.765717890
    ## 465   X1   X1 0.3173660 0.682633963
    ## 466   X1   X1 0.4646767 0.535323286
    ## 467   X0   X1 0.3399751 0.660024889
    ## 468   X0   X0 0.8258389 0.174161108
    ## 469   X0   X1 0.4033142 0.596685781
    ## 470   X1   X1 0.4643673 0.535632693
    ## 471   X0   X0 0.6937685 0.306231545
    ## 472   X0   X0 0.7869341 0.213065903
    ## 473   X1   X1 0.4809418 0.519058216
    ## 474   X1   X1 0.4458983 0.554101702
    ## 475   X0   X0 0.8570397 0.142960303
    ## 476   X1   X1 0.4935509 0.506449076
    ## 477   X0   X1 0.4107243 0.589275690
    ## 478   X1   X1 0.3774875 0.622512511
    ## 479   X0   X1 0.4979004 0.502099586
    ## 480   X0   X0 0.9446351 0.055364862
    ## 481   X1   X1 0.3346766 0.665323446
    ## 482   X1   X0 0.5485942 0.451405827
    ## 483   X0   X0 0.7157938 0.284206174
    ## 484   X0   X0 0.6251346 0.374865419
    ## 485   X1   X0 0.5683051 0.431694867
    ## 486   X0   X0 0.9212044 0.078795598
    ## 487   X0   X0 0.7951220 0.204877963
    ## 488   X0   X0 0.8555684 0.144431590
    ## 489   X0   X0 0.7481724 0.251827632
    ## 490   X1   X1 0.3907483 0.609251682
    ## 491   X0   X0 0.5743775 0.425622489
    ## 492   X1   X0 0.6087764 0.391223552
    ## 493   X0   X0 0.8906110 0.109388961
    ## 494   X0   X0 0.7290451 0.270954928
    ## 495   X0   X0 0.8415203 0.158479664
    ## 496   X1   X0 0.9263826 0.073617381
    ## 497   X0   X0 0.5854635 0.414536541
    ## 498   X1   X0 0.6565747 0.343425341
    ## 499   X0   X0 0.7530534 0.246946641
    ## 500   X0   X0 0.6380821 0.361917921
    ## 501   X0   X0 0.9563901 0.043609947
    ## 502   X1   X0 0.5410404 0.458959619
    ## 503   X0   X0 0.6995187 0.300481282
    ## 504   X1   X1 0.2316086 0.768391393
    ## 505   X0   X0 0.5670194 0.432980614
    ## 506   X0   X0 0.6989988 0.301001180
    ## 507   X1   X0 0.7340520 0.265947977
    ## 508   X0   X0 0.8013571 0.198642950
    ## 509   X0   X0 0.5876224 0.412377624
    ## 510   X1   X0 0.5350352 0.464964841
    ## 511   X0   X0 0.8967427 0.103257325
    ## 512   X0   X1 0.4654790 0.534521010
    ## 513   X0   X0 0.9013833 0.098616716
    ## 514   X0   X0 0.8587107 0.141289317
    ## 515   X0   X0 0.8492442 0.150755790
    ## 516   X0   X0 0.9586369 0.041363098
    ## 517   X0   X0 0.5821288 0.417871241
    ## 518   X0   X0 0.6798018 0.320198168
    ## 519   X0   X0 0.8541670 0.145833047
    ## 520   X0   X0 0.7922289 0.207771051
    ## 521   X0   X0 0.6653584 0.334641571
    ## 522   X1   X1 0.2680892 0.731910774
    ## 523   X1   X1 0.3975137 0.602486341
    ## 524   X1   X1 0.4162536 0.583746364
    ## 525   X0   X0 0.7494766 0.250523448
    ## 526   X1   X1 0.2941992 0.705800846
    ## 527   X1   X0 0.9075790 0.092421023
    ## 528   X1   X1 0.3404822 0.659517821
    ## 529   X1   X0 0.6169207 0.383079308
    ## 530   X0   X0 0.9453469 0.054653112
    ## 531   X0   X0 0.6739934 0.326006616
    ## 532   X0   X0 0.9811145 0.018885538
    ## 533   X0   X0 0.8985094 0.101490620
    ## 534   X1   X0 0.8323161 0.167683882
    ## 535   X0   X0 0.8622260 0.137774021
    ## 536   X0   X0 0.6929538 0.307046173
    ## 537   X1   X0 0.8568924 0.143107595
    ## 538   X0   X0 0.6719690 0.328031029
    ## 539   X1   X0 0.5386474 0.461352586
    ## 540   X0   X0 0.8919573 0.108042673
    ## 541   X0   X0 0.9160103 0.083989702
    ## 542   X0   X0 0.9491435 0.050856456
    ## 543   X0   X0 0.7112792 0.288720795
    ## 544   X0   X0 0.5362048 0.463795192
    ## 545   X0   X0 0.9337867 0.066213256
    ## 546   X0   X0 0.7956057 0.204394294
    ## 547   X1   X1 0.4819337 0.518066312
    ## 548   X0   X0 0.8410348 0.158965165
    ## 549   X0   X0 0.7672892 0.232710782
    ## 550   X1   X0 0.5526227 0.447377263
    ## 551   X0   X0 0.7020045 0.297995454
    ## 552   X1   X1 0.4585635 0.541436511
    ## 553   X1   X0 0.6978192 0.302180839
    ## 554   X0   X1 0.3164840 0.683516018
    ## 555   X0   X0 0.9164216 0.083578384
    ## 556   X0   X0 0.7143083 0.285691677
    ## 557   X1   X0 0.7160669 0.283933140
    ## 558   X0   X0 0.8923437 0.107656293
    ## 559   X0   X0 0.8727859 0.127214109
    ## 560   X0   X0 0.6805949 0.319405134
    ## 561   X0   X0 0.5160667 0.483933307
    ## 562   X1   X0 0.5404265 0.459573533
    ## 563   X1   X0 0.5659525 0.434047470
    ## 564   X1   X1 0.3278496 0.672150416
    ## 565   X1   X1 0.3657336 0.634266431
    ## 566   X0   X0 0.7988044 0.201195580
    ## 567   X0   X0 0.8630089 0.136991133
    ## 568   X1   X0 0.7147232 0.285276778
    ## 569   X0   X0 0.5715257 0.428474314
    ## 570   X1   X0 0.7986414 0.201358589
    ## 571   X1   X1 0.4360927 0.563907303
    ## 572   X1   X0 0.5704020 0.429598043
    ## 573   X0   X1 0.4874081 0.512591947
    ## 574   X1   X1 0.4711089 0.528891129
    ## 575   X0   X0 0.9246068 0.075393228
    ## 576   X0   X0 0.8707542 0.129245818
    ## 577   X0   X0 0.9390620 0.060938019
    ## 578   X1   X1 0.2981051 0.701894867
    ## 579   X1   X0 0.7529907 0.247009313
    ## 580   X0   X1 0.3089330 0.691067005
    ## 581   X0   X0 0.6826037 0.317396291
    ## 582   X0   X0 0.8706441 0.129355930
    ## 583   X1   X0 0.5256463 0.474353719
    ## 584   X0   X0 0.9461988 0.053801238
    ## 585   X0   X0 0.6674745 0.332525512
    ## 586   X1   X1 0.4300893 0.569910675
    ## 587   X1   X0 0.7342483 0.265751661
    ## 588   X0   X0 0.6281521 0.371847870
    ## 589   X0   X0 0.9545250 0.045475015
    ## 590   X0   X0 0.8404305 0.159569491
    ## 591   X0   X0 0.7613703 0.238629722
    ## 592   X0   X0 0.9928223 0.007177657
    ## 593   X0   X0 0.5519992 0.448000818
    ## 594   X1   X0 0.5522616 0.447738425
    ## 595   X0   X0 0.9453686 0.054631444
    ## 596   X0   X0 0.8559527 0.144047292
    ## 597   X1   X0 0.8188729 0.181127130
    ## 598   X0   X0 0.9602171 0.039782916
    ## 599   X0   X0 0.7895466 0.210453391
    ## 600   X0   X0 0.8462681 0.153731870
    ## 601   X0   X0 0.7944373 0.205562707
    ## 602   X0   X0 0.8805582 0.119441774
    ## 603   X0   X0 0.6133485 0.386651545
    ## 604   X0   X0 0.6359133 0.364086737
    ## 605   X0   X0 0.7659848 0.234015172
    ## 606   X0   X1 0.4379444 0.562055639
    ## 607   X1   X0 0.7685336 0.231466405
    ## 608   X0   X0 0.5175066 0.482493354
    ## 609   X0   X1 0.3449923 0.655007745
    ## 610   X0   X0 0.6592558 0.340744244
    ## 611   X0   X0 0.9381776 0.061822431
    ## 612   X0   X0 0.6225697 0.377430330
    ## 613   X1   X1 0.3541033 0.645896702
    ## 614   X1   X0 0.6958435 0.304156504
    ## 615   X0   X0 0.6891471 0.310852930
    ## 616   X0   X0 0.9139607 0.086039287
    ## 617   X0   X0 0.6705804 0.329419639
    ## 618   X0   X0 0.7331266 0.266873433
    ## 619   X1   X0 0.7722834 0.227716614
    ## 620   X0   X0 0.9381776 0.061822431
    ## 621   X0   X0 0.6792099 0.320790071
    ## 622   X0   X0 0.7338321 0.266167885
    ## 623   X1   X0 0.5738672 0.426132848
    ## 624   X0   X0 0.9367427 0.063257258
    ## 625   X0   X0 0.7813164 0.218683570
    ## 626   X1   X0 0.7116671 0.288332948
    ## 627   X0   X0 0.8745673 0.125432689
    ## 628   X0   X0 0.8258586 0.174141401
    ## 629   X0   X0 0.9344215 0.065578491
    ## 630   X1   X0 0.8815101 0.118489862
    ## 631   X0   X0 0.9240374 0.075962576
    ## 632   X1   X1 0.4923083 0.507691741
    ## 633   X1   X1 0.2131057 0.786894327
    ## 634   X0   X0 0.8592718 0.140728198
    ## 635   X0   X0 0.8558876 0.144112426
    ## 636   X1   X0 0.6100714 0.389928636
    ## 637   X1   X0 0.6190954 0.380904585
    ## 638   X0   X0 0.7122139 0.287786096
    ## 639   X0   X1 0.3006881 0.699311918
    ## 640   X0   X0 0.9454733 0.054526713
    ## 641   X0   X0 0.8911605 0.108839489
    ## 642   X0   X0 0.8161741 0.183825858
    ## 643   X0   X0 0.9896425 0.010357484
    ## 644   X1   X1 0.4607793 0.539220693
    ## 645   X0   X1 0.3909319 0.609068067
    ## 646   X0   X0 0.8144500 0.185549987
    ## 647   X0   X0 0.6901722 0.309827778
    ## 648   X0   X0 0.8503519 0.149648050
    ## 649   X0   X0 0.9189725 0.081027486
    ## 650   X1   X1 0.4876093 0.512390745
    ## 651   X0   X0 0.5172859 0.482714139
    ## 652   X0   X1 0.3970998 0.602900151
    ## 653   X1   X0 0.7020491 0.297950903
    ## 654   X0   X0 0.6587490 0.341250986
    ## 655   X0   X0 0.9266181 0.073381928
    ## 656   X0   X0 0.9548343 0.045165734
    ## 657   X0   X0 0.6826737 0.317326261
    ## 658   X0   X0 0.8162297 0.183770349
    ## 659   X0   X1 0.4304283 0.569571739
    ## 660   X1   X0 0.8486363 0.151363724
    ## 661   X0   X0 0.5257567 0.474243294
    ## 662   X0   X0 0.9060350 0.093965011
    ## 663   X1   X0 0.6472085 0.352791486
    ## 664   X0   X0 0.8786354 0.121364583
    ## 665   X0   X0 0.8777608 0.122239178
    ## 666   X0   X0 0.8462622 0.153737759
    ## 667   X0   X0 0.8720946 0.127905440
    ## 668   X0   X0 0.7348609 0.265139108
    ## 669   X0   X0 0.8927865 0.107213518
    ## 670   X0   X0 0.9100111 0.089988942
    ## 671   X1   X0 0.7103269 0.289673073
    ## 672   X0   X0 0.8805621 0.119437931
    ## 673   X0   X1 0.4190817 0.580918308
    ## 674   X0   X0 0.8752187 0.124781286
    ## 675   X0   X0 0.8398546 0.160145366
    ## 676   X0   X0 0.8920654 0.107934556
    ## 677   X0   X1 0.3886529 0.611347140
    ## 678   X0   X0 0.7180849 0.281915053
    ## 679   X1   X0 0.8211284 0.178871586
    ## 680   X0   X0 0.5575434 0.442456569
    ## 681   X0   X0 0.9381223 0.061877741
    ## 682   X0   X0 0.8369955 0.163004463
    ## 683   X0   X0 0.7438185 0.256181543
    ## 684   X0   X0 0.6497484 0.350251589
    ## 685   X1   X1 0.4933072 0.506692830
    ## 686   X0   X0 0.8426575 0.157342521
    ## 687   X0   X0 0.9176924 0.082307626
    ## 688   X0   X0 0.6269982 0.373001845
    ## 689   X0   X0 0.6504768 0.349523226
    ## 690   X1   X0 0.6987190 0.301280993
    ## 691   X1   X0 0.8179834 0.182016618
    ## 692   X0   X0 0.8345247 0.165475330
    ## 693   X0   X0 0.6912908 0.308709215
    ## 694   X1   X0 0.6547295 0.345270516
    ## 695   X1   X0 0.5801518 0.419848211
    ## 696   X0   X0 0.6711722 0.328827821
    ## 697   X0   X0 0.9022581 0.097741910
    ## 698   X0   X0 0.8145398 0.185460194
    ## 699   X1   X1 0.4093412 0.590658832
    ## 700   X0   X0 0.9195857 0.080414293
    ## 701   X0   X0 0.5829384 0.417061573
    ## 702   X1   X0 0.8208173 0.179182698
    ## 703   X0   X0 0.8691223 0.130877681
    ## 704   X0   X0 0.8135005 0.186499491
    ## 705   X0   X0 0.9267044 0.073295644
    ## 706   X0   X1 0.4094148 0.590585249
    ## 707   X0   X0 0.9080025 0.091997457
    ## 708   X1   X1 0.4091644 0.590835608
    ## 709   X0   X0 0.9429602 0.057039800
    ## 710   X1   X0 0.8606203 0.139379701
    ## 711   X1   X0 0.6902066 0.309793401
    ## 712   X0   X0 0.7405509 0.259449110
    ## 713   X0   X0 0.7400505 0.259949493
    ## 714   X1   X0 0.6042735 0.395726538
    ## 715   X1   X0 0.8871850 0.112815015
    ## 716   X0   X0 0.8881824 0.111817582
    ## 717   X1   X0 0.8301725 0.169827525
    ## 718   X1   X0 0.6971913 0.302808679
    ## 719   X0   X0 0.9846195 0.015380496
    ## 720   X0   X0 0.8692991 0.130700876
    ## 721   X0   X0 0.9051427 0.094857270
    ## 722   X0   X0 0.7704472 0.229552796
    ## 723   X0   X0 0.7139512 0.286048840
    ## 724   X1   X0 0.5662731 0.433726936
    ## 725   X0   X0 0.8591337 0.140866256
    ## 726   X0   X0 0.8812949 0.118705090
    ## 727   X1   X0 0.9331017 0.066898311
    ## 728   X0   X0 0.6208006 0.379199369
    ## 729   X0   X0 0.8416008 0.158399186
    ## 730   X0   X1 0.4147738 0.585226188
    ## 731   X0   X0 0.9093081 0.090691923
    ## 732   X0   X0 0.8076179 0.192382073
    ## 733   X0   X0 0.5716542 0.428345757
    ## 734   X0   X0 0.6735841 0.326415873
    ## 735   X0   X0 0.6073510 0.392649009
    ## 736   X0   X1 0.1575460 0.842454047
    ## 737   X0   X0 0.9298032 0.070196752
    ## 738   X0   X0 0.7596302 0.240369842
    ## 739   X0   X1 0.3679981 0.632001876
    ## 740   X0   X0 0.7113508 0.288649240
    ## 741   X1   X0 0.5046126 0.495387382
    ## 742   X1   X0 0.6289876 0.371012432
    ## 743   X0   X0 0.7055448 0.294455185
    ## 744   X0   X0 0.9877377 0.012262347
    ## 745   X0   X0 0.8010786 0.198921400
    ## 746   X1   X0 0.9166297 0.083370316
    ## 747   X1   X0 0.6674745 0.332525512
    ## 748   X0   X0 0.9299741 0.070025949
    ## 749   X0   X0 0.8527401 0.147259882
    ## 750   X0   X1 0.2473686 0.752631387
    ## 751   X1   X1 0.4847046 0.515295370
    ## 752   X0   X0 0.8776163 0.122383724
    ## 753   X0   X0 0.7012014 0.298798586
    ## 754   X0   X1 0.2128585 0.787141509
    ## 755   X0   X1 0.4647937 0.535206330
    ## 756   X1   X0 0.6634076 0.336592401
    ## 757   X0   X0 0.5345779 0.465422117
    ## 758   X0   X0 0.5431823 0.456817667
    ## 759   X0   X0 0.9409428 0.059057184
    ## 760   X0   X0 0.8884977 0.111502293
    ## 761   X0   X0 0.9178150 0.082184995
    ## 762   X0   X0 0.6773355 0.322664461
    ## 763   X0   X0 0.8810751 0.118924938
    ## 764   X0   X0 0.9214884 0.078511618
    ## 765   X0   X0 0.8221288 0.177871240
    ## 766   X0   X0 0.8506030 0.149397016
    ## 767   X0   X0 0.8568092 0.143190812
    ## 768   X0   X0 0.5982135 0.401786485
    ## 769   X0   X0 0.8905018 0.109498155
    ## 770   X0   X0 0.9078361 0.092163942
    ## 771   X0   X0 0.9150341 0.084965873
    ## 772   X1   X0 0.6175424 0.382457551
    ## 773   X0   X0 0.9050945 0.094905508
    ## 774   X0   X0 0.9322422 0.067757826
    ## 775   X0   X0 0.8990718 0.100928198
    ## 776   X0   X0 0.8356060 0.164394014
    ## 777   X0   X0 0.5612886 0.438711385
    ## 778   X0   X0 0.9504318 0.049568161
    ## 779   X0   X0 0.7922258 0.207774212
    ## 780   X0   X0 0.8767819 0.123218054
    ## 781   X1   X0 0.6062921 0.393707886
    ## 782   X0   X0 0.8923091 0.107690936
    ## 783   X0   X0 0.9102104 0.089789642
    ## 784   X0   X0 0.9677475 0.032252481
    ## 785   X0   X0 0.7656757 0.234324318
    ## 786   X1   X1 0.4568011 0.543198879
    ## 787   X0   X0 0.5030718 0.496928214
    ## 788   X0   X0 0.8926272 0.107372762
    ## 789   X0   X0 0.7695646 0.230435383
    ## 790   X1   X0 0.6964883 0.303511689
    ## 791   X0   X0 0.5708150 0.429185038
    ## 792   X0   X0 0.7720889 0.227911052
    ## 793   X0   X0 0.8822668 0.117733213
    ## 794   X0   X0 0.8840951 0.115904948
    ## 795   X0   X0 0.9842377 0.015762280
    ## 796   X1   X0 0.7201531 0.279846927
    ## 797   X0   X0 0.8911605 0.108839489
    ## 798   X1   X0 0.6289776 0.371022367
    ## 799   X0   X0 0.7434450 0.256554964
    ## 800   X0   X0 0.7959810 0.204019027
    ## 801   X0   X0 0.5486738 0.451326212
    ## 802   X0   X0 0.7498203 0.250179740
    ## 803   X1   X0 0.8673274 0.132672640
    ## 804   X0   X0 0.7764556 0.223544402
    ## 805   X1   X1 0.4484913 0.551508735
    ## 806   X1   X0 0.6008439 0.399156068
    ## 807   X0   X0 0.5551078 0.444892195
    ## 808   X0   X0 0.5228439 0.477156084
    ## 809   X0   X0 0.7222396 0.277760400
    ## 810   X0   X0 0.8133951 0.186604897
    ## 811   X0   X0 0.8081251 0.191874864
    ## 812   X0   X1 0.4515362 0.548463784
    ## 813   X1   X0 0.5480901 0.451909851
    ## 814   X0   X0 0.8376037 0.162396331
    ## 815   X0   X0 0.7211754 0.278824602
    ## 816   X1   X0 0.5344425 0.465557452
    ## 817   X1   X0 0.7108186 0.289181438
    ## 818   X0   X1 0.3807606 0.619239352
    ## 819   X0   X0 0.8536307 0.146369303
    ## 820   X0   X0 0.7604324 0.239567560
    ## 821   X1   X0 0.6190988 0.380901246
    ## 822   X0   X1 0.4989358 0.501064242
    ## 823   X0   X1 0.3558972 0.644102818
    ## 824   X0   X0 0.8586285 0.141371502
    ## 825   X0   X0 0.8254773 0.174522695
    ## 826   X0   X0 0.7374050 0.262595020
    ## 827   X1   X0 0.5930283 0.406971717
    ## 828   X0   X0 0.8174400 0.182560037
    ## 829   X0   X0 0.8160220 0.183978036
    ## 830   X0   X0 0.6626051 0.337394915
    ## 831   X0   X0 0.8548257 0.145174268
    ## 832   X1   X1 0.4013137 0.598686291
    ## 833   X1   X0 0.5892497 0.410750346
    ## 834   X0   X0 0.6791613 0.320838744
    ## 835   X0   X1 0.4586768 0.541323239
    ## 836   X1   X0 0.6499147 0.350085330
    ## 837   X1   X1 0.4768601 0.523139920
    ## 838   X0   X0 0.7480754 0.251924609
    ## 839   X1   X0 0.7578359 0.242164105
    ## 840   X0   X0 0.7666589 0.233341053
    ## 841   X0   X0 0.9498518 0.050148174
    ## 842   X1   X0 0.8741668 0.125833162
    ## 843   X1   X1 0.4855876 0.514412408
    ## 844   X0   X0 0.8845420 0.115458006
    ## 845   X0   X1 0.4093495 0.590650461
    ## 846   X0   X0 0.8336660 0.166333979
    ## 847   X0   X1 0.4601641 0.539835938
    ## 848   X1   X1 0.4672362 0.532763782
    ## 849   X0   X0 0.9195607 0.080439261
    ## 850   X0   X0 0.7957148 0.204285169
    ## 851   X0   X0 0.5854635 0.414536541
    ## 852   X0   X0 0.6197882 0.380211785
    ## 853   X1   X0 0.6810849 0.318915127
    ## 854   X0   X0 0.9036143 0.096385728
    ## 855   X0   X0 0.7165679 0.283432141
    ## 856   X0   X0 0.8477673 0.152232729
    ## 857   X1   X0 0.6753535 0.324646515
    ## 858   X0   X1 0.1888592 0.811140761
    ## 859   X0   X0 0.6240208 0.375979205
    ## 860   X0   X0 0.7554601 0.244539850
    ## 861   X1   X1 0.4495882 0.550411806
    ## 862   X0   X1 0.4828073 0.517192723
    ## 863   X0   X0 0.8464952 0.153504848
    ## 864   X0   X0 0.5537711 0.446228920
    ## 865   X1   X1 0.3413939 0.658606052
    ## 866   X1   X0 0.6433043 0.356695714
    ## 867   X0   X0 0.6117410 0.388258973
    ## 868   X0   X0 0.6585210 0.341478959
    ## 869   X0   X0 0.9183583 0.081641671
    ## 870   X1   X1 0.4923083 0.507691741
    ## 871   X0   X0 0.8871252 0.112874783
    ## 872   X1   X0 0.6940712 0.305928841
    ## 873   X0   X0 0.8006186 0.199381408
    ## 874   X0   X0 0.9587786 0.041221360
    ## 875   X0   X0 0.9358362 0.064163838
    ## 876   X1   X0 0.6868183 0.313181674
    ## 877   X0   X0 0.8963720 0.103627967
    ## 878   X1   X1 0.4022519 0.597748079
    ## 879   X0   X0 0.9477607 0.052239288
    ## 880   X1   X0 0.5312500 0.468750038
    ## 881   X1   X1 0.2973030 0.702696985
    ## 882   X0   X0 0.7392139 0.260786090
    ## 883   X0   X0 0.6658759 0.334124108
    ## 884   X1   X0 0.5461469 0.453853123
    ## 885   X1   X0 0.8288111 0.171188934
    ## 886   X1   X1 0.1343091 0.865690877
    ## 887   X1   X0 0.8022121 0.197787856
    ## 888   X0   X0 0.7271101 0.272889926
    ## 889   X0   X0 0.7655138 0.234486245
    ## 890   X1   X1 0.3555335 0.644466484
    ## 891   X0   X1 0.4032153 0.596784683
    ## 892   X0   X1 0.4867224 0.513277568
    ## 893   X0   X0 0.9469450 0.053055037
    ## 894   X1   X0 0.5562934 0.443706638
    ## 895   X0   X0 0.7434501 0.256549950
    ## 896   X1   X0 0.6108811 0.389118908
    ## 897   X0   X0 0.7988044 0.201195580
    ## 898   X0   X0 0.5906672 0.409332842
    ## 899   X0   X0 0.9888055 0.011194452
    ## 900   X1   X1 0.4682196 0.531780399
    ## 901   X0   X1 0.4096712 0.590328815
    ## 902   X0   X0 0.7868982 0.213101828
    ## 903   X1   X0 0.8834783 0.116521675
    ## 904   X0   X0 0.9503297 0.049670264
    ## 905   X0   X0 0.9663514 0.033648640
    ## 906   X0   X0 0.9586979 0.041302142
    ## 907   X1   X1 0.3317665 0.668233537
    ## 908   X0   X0 0.8924801 0.107519871
    ## 909   X0   X0 0.7756131 0.224386887
    ## 910   X0   X0 0.8544963 0.145503727
    ## 911   X0   X0 0.7672320 0.232767953
    ## 912   X1   X0 0.6883834 0.311616581
    ## 913   X0   X0 0.7702105 0.229789515
    ## 914   X0   X0 0.5933446 0.406655380
    ## 915   X0   X0 0.7190542 0.280945812
    ## 916   X0   X1 0.3378433 0.662156674
    ## 917   X0   X0 0.5947665 0.405233473
    ## 918   X0   X0 0.7645333 0.235466685
    ## 919   X0   X0 0.8629038 0.137096187
    ## 920   X0   X0 0.8618798 0.138120209
    ## 921   X0   X0 0.8746058 0.125394210
    ## 922   X0   X0 0.8458747 0.154125269
    ## 923   X0   X0 0.9093081 0.090691923
    ## 924   X0   X0 0.7936335 0.206366513
    ## 925   X0   X0 0.9044820 0.095517974
    ## 926   X0   X0 0.8277243 0.172275724
    ## 927   X1   X0 0.9821989 0.017801123
    ## 928   X1   X0 0.6932441 0.306755868
    ## 929   X0   X1 0.4007803 0.599219689
    ## 930   X0   X0 0.9465098 0.053490237
    ## 931   X0   X0 0.9185134 0.081486550
    ## 932   X0   X0 0.5657747 0.434225276
    ## 933   X0   X0 0.7827149 0.217285148
    ## 934   X0   X0 0.9342497 0.065750274
    ## 935   X0   X0 0.5213364 0.478663642
    ## 936   X0   X0 0.6286195 0.371380487
    ## 937   X1   X0 0.6353366 0.364663368
    ## 938   X0   X0 0.6690141 0.330985904
    ## 939   X0   X1 0.4606402 0.539359795
    ## 940   X1   X0 0.5288144 0.471185561
    ## 941   X1   X0 0.6682632 0.331736839
    ## 942   X1   X0 0.5654044 0.434595555
    ## 943   X0   X1 0.4409951 0.559004922
    ## 944   X0   X0 0.5699042 0.430095810
    ## 945   X1   X0 0.5146741 0.485325918
    ## 946   X0   X1 0.3191567 0.680843321
    ## 947   X0   X0 0.5281604 0.471839558
    ## 948   X0   X0 0.5794993 0.420500711
    ## 949   X0   X0 0.8972409 0.102759128
    ## 950   X0   X0 0.7281201 0.271879853
    ## 951   X1   X0 0.6067273 0.393272657
    ## 952   X1   X0 0.8089008 0.191099151
    ## 953   X0   X0 0.7325852 0.267414826
    ## 954   X1   X0 0.9484532 0.051546798
    ## 955   X1   X0 0.5633984 0.436601551
    ## 956   X0   X0 0.8698376 0.130162447
    ## 957   X1   X0 0.5927455 0.407254508
    ## 958   X0   X0 0.8443977 0.155602275
    ## 959   X0   X0 0.9079209 0.092079139
    ## 960   X0   X0 0.6638343 0.336165661
    ## 961   X0   X0 0.8907792 0.109220785
    ## 962   X0   X1 0.2639386 0.736061417
    ## 963   X0   X0 0.9183221 0.081677910
    ## 964   X0   X0 0.5283345 0.471665470
    ## 965   X0   X0 0.9085201 0.091479851
    ## 966   X0   X1 0.4474025 0.552597504
    ## 967   X0   X0 0.9096505 0.090349548
    ## 968   X1   X0 0.5318998 0.468100166
    ## 969   X0   X1 0.4825497 0.517450312
    ## 970   X1   X0 0.8164548 0.183545189
    ## 971   X1   X1 0.4357301 0.564269908
    ## 972   X1   X1 0.3160120 0.683988000
    ## 973   X1   X0 0.8983204 0.101679621
    ## 974   X0   X0 0.9101573 0.089842688
    ## 975   X1   X0 0.7821115 0.217888462
    ## 976   X0   X0 0.6448118 0.355188181
    ## 977   X0   X0 0.8509038 0.149096244
    ## 978   X0   X0 0.5332391 0.466760870
    ## 979   X0   X0 0.7403872 0.259612793
    ## 980   X1   X0 0.9200623 0.079937653
    ## 981   X0   X0 0.8319833 0.168016746
    ## 982   X0   X1 0.4858855 0.514114486
    ## 983   X1   X0 0.5394453 0.460554730
    ## 984   X1   X1 0.3797592 0.620240779
    ## 985   X0   X0 0.8207946 0.179205405
    ## 986   X0   X0 0.8536963 0.146303708
    ## 987   X0   X0 0.7720072 0.227992849
    ## 988   X1   X1 0.2803099 0.719690150
    ## 989   X0   X0 0.9265527 0.073447272
    ## 990   X1   X0 0.9387856 0.061214423
    ## 991   X0   X0 0.8089652 0.191034780
    ## 992   X0   X0 0.6092228 0.390777200
    ## 993   X0   X0 0.7328843 0.267115705
    ## 994   X0   X0 0.9409922 0.059007840
    ## 995   X1   X1 0.4352872 0.564712798
    ## 996   X0   X0 0.8466781 0.153321875
    ## 997   X1   X0 0.6080171 0.391982901
    ## 998   X0   X0 0.5343221 0.465677919
    ## 999   X1   X1 0.4524799 0.547520121
    ## 1000  X0   X0 0.9130919 0.086908064
    ## 1001  X1   X0 0.7873116 0.212688375
    ## 1002  X0   X0 0.9093081 0.090691923
    ## 1003  X0   X0 0.8531282 0.146871757
    ## 1004  X0   X0 0.9124782 0.087521834
    ## 1005  X0   X0 0.9052476 0.094752355
    ## 1006  X0   X0 0.8088740 0.191125986
    ## 1007  X0   X0 0.8801679 0.119832112
    ## 1008  X1   X0 0.5205092 0.479490757
    ## 1009  X1   X0 0.6624947 0.337505328
    ## 1010  X0   X0 0.7495441 0.250455913
    ## 1011  X0   X0 0.9241726 0.075827410
    ## 1012  X0   X0 0.9466193 0.053380722
    ## 1013  X0   X0 0.5452918 0.454708206
    ## 1014  X0   X0 0.7949655 0.205034490
    ## 1015  X0   X0 0.9400556 0.059944425
    ## 1016  X1   X0 0.7706644 0.229335609
    ## 1017  X0   X0 0.5758245 0.424175468
    ## 1018  X1   X0 0.7426834 0.257316617
    ## 1019  X0   X0 0.8940927 0.105907290
    ## 1020  X0   X0 0.8535306 0.146469429
    ## 1021  X1   X0 0.8008075 0.199192480
    ## 1022  X0   X0 0.7670299 0.232970144
    ## 1023  X0   X0 0.8682381 0.131761880
    ## 1024  X0   X0 0.9108215 0.089178518
    ## 1025  X0   X0 0.9429808 0.057019160
    ## 1026  X1   X0 0.8335551 0.166444855
    ## 1027  X0   X0 0.5764122 0.423587810
    ## 1028  X0   X0 0.7849665 0.215033472
    ## 1029  X1   X0 0.6486073 0.351392699
    ## 1030  X0   X0 0.8108579 0.189142088
    ## 1031  X0   X0 0.8195561 0.180443884
    ## 1032  X0   X0 0.7949655 0.205034490
    ## 1033  X0   X1 0.3900788 0.609921215
    ## 1034  X0   X0 0.9121866 0.087813421
    ## 1035  X0   X0 0.9420921 0.057907909
    ## 1036  X0   X0 0.9455510 0.054449035
    ## 1037  X0   X0 0.6042570 0.395742957
    ## 1038  X0   X0 0.8837210 0.116279046
    ## 1039  X0   X0 0.9483533 0.051646725
    ## 1040  X0   X0 0.8659989 0.134001062
    ## 1041  X0   X0 0.7514417 0.248558284
    ## 1042  X1   X0 0.5037600 0.496240044
    ## 1043  X0   X0 0.7187535 0.281246469
    ## 1044  X0   X0 0.7725180 0.227482043
    ## 1045  X1   X1 0.3742238 0.625776221
    ## 1046  X0   X0 0.8683855 0.131614507
    ## 1047  X1   X0 0.6040603 0.395939670
    ## 1048  X0   X0 0.6443269 0.355673122
    ## 1049  X0   X0 0.5022653 0.497734740
    ## 1050  X0   X0 0.6713658 0.328634235
    ## 1051  X0   X0 0.9291085 0.070891492
    ## 1052  X0   X0 0.8700680 0.129931971
    ## 1053  X0   X0 0.8963720 0.103627967
    ## 1054  X0   X0 0.7934099 0.206590125
    ## 1055  X0   X0 0.9832189 0.016781058
    ## 1056  X0   X0 0.8889131 0.111086886
    ## 1057  X0   X0 0.7104257 0.289574338
    ## 1058  X1   X1 0.3739997 0.626000349
    ## 1059  X0   X0 0.7765520 0.223447998
    ## 1060  X0   X0 0.7357458 0.264254249
    ## 1061  X0   X0 0.6425670 0.357432982
    ## 1062  X1   X1 0.3905954 0.609404571
    ## 1063  X1   X0 0.8752472 0.124752812
    ## 1064  X0   X0 0.9548680 0.045132028
    ## 1065  X0   X0 0.9096444 0.090355555
    ## 1066  X1   X0 0.8577717 0.142228323
    ## 1067  X0   X0 0.9229990 0.077000990
    ## 1068  X1   X0 0.7696289 0.230371075
    ## 1069  X1   X0 0.8118868 0.188113154
    ## 1070  X1   X1 0.3846304 0.615369639
    ## 1071  X0   X0 0.7505545 0.249445509
    ## 1072  X0   X0 0.8849232 0.115076801
    ## 1073  X0   X0 0.6503258 0.349674198
    ## 1074  X0   X0 0.8183991 0.181600917
    ## 1075  X0   X0 0.9205326 0.079467395
    ## 1076  X0   X0 0.7457375 0.254262474
    ## 1077  X1   X0 0.9004020 0.099597951
    ## 1078  X0   X0 0.5693251 0.430674862
    ## 1079  X0   X0 0.9237008 0.076299165
    ## 1080  X0   X0 0.9197579 0.080242116
    ## 1081  X0   X0 0.8740231 0.125976919
    ## 1082  X0   X1 0.4923083 0.507691741
    ## 1083  X0   X0 0.7251606 0.274839425
    ## 1084  X0   X0 0.9626876 0.037312413
    ## 1085  X0   X0 0.8765122 0.123487821
    ## 1086  X1   X0 0.8372428 0.162757240
    ## 1087  X0   X0 0.9124782 0.087521834
    ## 1088  X0   X0 0.7841511 0.215848913
    ## 1089  X0   X0 0.9612753 0.038724663
    ## 1090  X1   X0 0.7791083 0.220891707
    ## 1091  X0   X0 0.8850684 0.114931623
    ## 1092  X0   X0 0.6034136 0.396586449
    ## 1093  X0   X0 0.7703849 0.229615125
    ## 1094  X0   X0 0.7593293 0.240670715
    ## 1095  X0   X0 0.6973463 0.302653742
    ## 1096  X1   X0 0.6448118 0.355188181
    ## 1097  X1   X0 0.8353220 0.164678035
    ## 1098  X0   X0 0.9134083 0.086591687
    ## 1099  X0   X0 0.5463345 0.453665547
    ## 1100  X0   X0 0.9689160 0.031084043
    ## 1101  X0   X0 0.9686907 0.031309317
    ## 1102  X0   X0 0.8584484 0.141551570
    ## 1103  X1   X1 0.4861877 0.513812252
    ## 1104  X0   X0 0.9140825 0.085917459
    ## 1105  X1   X1 0.2804307 0.719569302
    ## 1106  X0   X0 0.7360442 0.263955791
    ## 1107  X0   X0 0.9892952 0.010704773
    ## 1108  X1   X0 0.7140664 0.285933632
    ## 1109  X0   X0 0.7895238 0.210476160
    ## 1110  X0   X0 0.9444442 0.055555838
    ## 1111  X0   X0 0.8809343 0.119065718
    ## 1112  X0   X0 0.5823624 0.417637592
    ## 1113  X0   X0 0.9011970 0.098803015
    ## 1114  X1   X0 0.5622036 0.437796383
    ## 1115  X0   X0 0.9120142 0.087985774
    ## 1116  X0   X0 0.9028128 0.097187161
    ## 1117  X0   X0 0.6114960 0.388504018
    ## 1118  X1   X1 0.3836220 0.616377968
    ## 1119  X0   X0 0.6742145 0.325785493
    ## 1120  X1   X0 0.7783809 0.221619088
    ## 1121  X0   X0 0.5299113 0.470088658
    ## 1122  X1   X0 0.5208849 0.479115064
    ## 1123  X1   X0 0.7184639 0.281536077
    ## 1124  X0   X0 0.5527778 0.447222154
    ## 1125  X0   X0 0.8946011 0.105398889
    ## 1126  X0   X0 0.6554576 0.344542426
    ## 1127  X0   X0 0.9176924 0.082307626
    ## 1128  X0   X1 0.3149987 0.685001295
    ## 1129  X1   X1 0.1491257 0.850874285
    ## 1130  X0   X0 0.5520774 0.447922607
    ## 1131  X1   X1 0.2890135 0.710986492
    ## 1132  X0   X0 0.8617967 0.138203325
    ## 1133  X0   X0 0.6500095 0.349990535
    ## 1134  X0   X0 0.9630963 0.036903677
    ## 1135  X0   X0 0.5877707 0.412229257
    ## 1136  X0   X0 0.7875518 0.212448196
    ## 1137  X0   X1 0.4180159 0.581984117
    ## 1138  X0   X0 0.7402143 0.259785709
    ## 1139  X0   X0 0.8503030 0.149696956
    ## 1140  X1   X0 0.7073841 0.292615865
    ## 1141  X0   X0 0.6606511 0.339348853
    ## 1142  X0   X0 0.7490698 0.250930186
    ## 1143  X1   X0 0.6490847 0.350915263
    ## 1144  X1   X0 0.5221636 0.477836366
    ## 1145  X0   X0 0.9146804 0.085319565
    ## 1146  X0   X0 0.8336660 0.166333979
    ## 1147  X0   X0 0.8715756 0.128424425
    ## 1148  X0   X0 0.6511098 0.348890234
    ## 1149  X0   X0 0.6340071 0.365992920
    ## 1150  X0   X0 0.9372014 0.062798557
    ## 1151  X0   X0 0.8434730 0.156527027
    ## 1152  X0   X0 0.8872323 0.112767735
    ## 1153  X0   X0 0.5264904 0.473509570
    ## 1154  X0   X0 0.6845810 0.315419009
    ## 1155  X1   X0 0.8385441 0.161455936
    ## 1156  X0   X0 0.8014144 0.198585559
    ## 1157  X1   X0 0.9658197 0.034180309
    ## 1158  X1   X0 0.9155477 0.084452262
    ## 1159  X1   X0 0.8991699 0.100830107
    ## 1160  X0   X0 0.9006440 0.099355974
    ## 1161  X1   X0 0.8027704 0.197229633
    ## 1162  X0   X0 0.5473248 0.452675193
    ## 1163  X0   X0 0.8956119 0.104388104
    ## 1164  X0   X0 0.7992270 0.200772957
    ## 1165  X1   X1 0.4944651 0.505534864
    ## 1166  X0   X0 0.6226882 0.377311821
    ## 1167  X0   X0 0.7459128 0.254087164
    ## 1168  X0   X0 0.8896051 0.110394858
    ## 1169  X0   X0 0.8738240 0.126176030
    ## 1170  X1   X0 0.9315602 0.068439761
    ## 1171  X0   X0 0.8360777 0.163922334
    ## 1172  X1   X0 0.6137463 0.386253655
    ## 1173  X0   X0 0.7739574 0.226042574
    ## 1174  X1   X0 0.6732696 0.326730351
    ## 1175  X1   X1 0.4489953 0.551004736
    ## 1176  X1   X0 0.6386542 0.361345792
    ## 1177  X0   X0 0.6480443 0.351955656
    ## 1178  X1   X0 0.5255633 0.474436666
    ## 1179  X0   X0 0.6994943 0.300505745
    ## 1180  X1   X1 0.4658902 0.534109758
    ## 1181  X0   X0 0.6734940 0.326505992
    ## 1182  X0   X0 0.6780331 0.321966937
    ## 1183  X0   X0 0.7602477 0.239752326
    ## 1184  X0   X0 0.8414380 0.158561950
    ## 1185  X0   X0 0.5319919 0.468008148
    ## 1186  X0   X1 0.4053925 0.594607467
    ## 1187  X0   X0 0.6865001 0.313499884
    ## 1188  X0   X0 0.5021604 0.497839604
    ## 1189  X1   X1 0.3365246 0.663475401
    ## 1190  X1   X0 0.8029864 0.197013637
    ## 1191  X1   X1 0.4237346 0.576265394
    ## 1192  X0   X0 0.7468053 0.253194721
    ## 1193  X0   X0 0.6057961 0.394203946
    ## 1194  X0   X0 0.7262070 0.273792985
    ## 1195  X0   X0 0.7424217 0.257578294
    ## 1196  X1   X1 0.3108716 0.689128404
    ## 1197  X0   X1 0.4179304 0.582069587
    ## 1198  X0   X0 0.6635297 0.336470290
    ## 1199  X0   X0 0.7659354 0.234064611
    ## 1200  X0   X1 0.4517524 0.548247593
    ## 1201  X0   X0 0.8472962 0.152703827
    ## 1202  X0   X1 0.4822965 0.517703489
    ## 1203  X0   X0 0.9437301 0.056269912
    ## 1204  X1   X0 0.6647067 0.335293349
    ## 1205  X0   X0 0.8298283 0.170171689
    ## 1206  X1   X0 0.6702137 0.329786335
    ## 1207  X1   X0 0.7920620 0.207938000
    ## 1208  X1   X1 0.4118127 0.588187288
    ## 1209  X1   X0 0.9133005 0.086699453
    ## 1210  X0   X0 0.6961155 0.303884494
    ## 1211  X1   X1 0.3333892 0.666610775
    ## 1212  X0   X0 0.6331607 0.366839284
    ## 1213  X1   X0 0.8668554 0.133144585
    ## 1214  X0   X0 0.5622586 0.437741403
    ## 1215  X0   X0 0.6055440 0.394455957
    ## 1216  X0   X0 0.8859983 0.114001682
    ## 1217  X0   X0 0.7409750 0.259025015
    ## 1218  X1   X0 0.6831888 0.316811179
    ## 1219  X0   X0 0.5566446 0.443355412
    ## 1220  X0   X0 0.5217418 0.478258236
    ## 1221  X1   X0 0.9243541 0.075645863
    ## 1222  X0   X0 0.9571176 0.042882417
    ## 1223  X0   X0 0.7198399 0.280160114
    ## 1224  X0   X0 0.8601912 0.139808801
    ## 1225  X1   X0 0.6828892 0.317110807
    ## 1226  X0   X0 0.9198300 0.080170008
    ## 1227  X0   X0 0.8811827 0.118817255
    ## 1228  X1   X0 0.5743897 0.425610298
    ## 1229  X0   X0 0.8201492 0.179850791
    ## 1230  X0   X0 0.7191094 0.280890593
    ## 1231  X0   X0 0.8759931 0.124006881
    ## 1232  X0   X0 0.6616790 0.338320964
    ## 1233  X0   X0 0.8960356 0.103964377
    ## 1234  X0   X0 0.8477673 0.152232729
    ## 1235  X1   X1 0.4428222 0.557177816
    ## 1236  X0   X1 0.4825497 0.517450312
    ## 1237  X0   X0 0.9189772 0.081022805
    ## 1238  X0   X0 0.9383207 0.061679345
    ## 1239  X1   X0 0.6741524 0.325847606
    ## 1240  X1   X0 0.5216769 0.478323079
    ## 1241  X0   X0 0.5013670 0.498633017
    ## 1242  X0   X0 0.7133826 0.286617365
    ## 1243  X1   X0 0.6556188 0.344381159
    ## 1244  X0   X0 0.5594688 0.440531208
    ## 1245  X1   X0 0.7408405 0.259159483
    ## 1246  X0   X0 0.5032286 0.496771369
    ## 1247  X0   X0 0.6957258 0.304274185
    ## 1248  X1   X0 0.8130042 0.186995778
    ## 1249  X1   X0 0.6725546 0.327445367
    ## 1250  X0   X0 0.8078839 0.192116133
    ## 1251  X0   X0 0.7151961 0.284803943
    ## 1252  X0   X0 0.8842424 0.115757579
    ## 1253  X0   X0 0.9423047 0.057695281
    ## 1254  X1   X1 0.3358749 0.664125123
    ## 1255  X0   X0 0.7564820 0.243517991
    ## 1256  X0   X0 0.9026564 0.097343579
    ## 1257  X0   X0 0.6650702 0.334929845
    ## 1258  X1   X0 0.6239717 0.376028309
    ## 1259  X1   X0 0.5825919 0.417408109
    ## 1260  X1   X0 0.6451709 0.354829092
    ## 1261  X1   X0 0.7173691 0.282630928
    ## 1262  X1   X0 0.5233543 0.476645745
    ## 1263  X0   X1 0.3750828 0.624917159
    ## 1264  X1   X0 0.5601050 0.439894985
    ## 1265  X0   X1 0.4841200 0.515879979

So it turned out not really a good prediction method: its accuracy rate
is worse than “No infomation rate”, although the best parameters for
lasso regression tree was selected.  

## CLASSIFICATION TREE MODEL

At the same time, classification tree method is very straight-forward
and easy to read.Classification tree is try to classify the predictors
into differnt “spaces” and within each space further classification
could be made based other predictors to split up even smaller spaces and
so on. In the end, give a certain response value within each
“space”.This method is very straight-forward for users and sometimes
generates pretty accurate predictions.

``` r
library(tree)
```

    ## Warning: package 'tree' was built under R version 4.3.2

``` r
diabetes_tree<-tree(Diabetes_binary~.,data=train,split="deviance")
diabetes_tree
```

    ## node), split, n, deviance, yval, (yprob)
    ##       * denotes terminal node
    ## 
    ## 1) root 2952 3564.0 X0 ( 0.70833 0.29167 )  
    ##   2) HighBP: 0 1220 1007.0 X0 ( 0.85574 0.14426 )  
    ##     4) Age: 1,2,3,4,5,7 428  201.5 X0 ( 0.93692 0.06308 ) *
    ##     5) Age: 6,8,9,10,11,12,13 792  765.9 X0 ( 0.81187 0.18813 ) *
    ##   3) HighBP: 1 1732 2325.0 X0 ( 0.60450 0.39550 )  
    ##     6) GenHlth: 1,2,3 668  776.5 X0 ( 0.73204 0.26796 ) *
    ##     7) GenHlth: 4,5 1064 1472.0 X0 ( 0.52444 0.47556 ) *

``` r
summary(diabetes_tree)
```

    ## 
    ## Classification tree:
    ## tree(formula = Diabetes_binary ~ ., data = train, split = "deviance")
    ## Variables actually used in tree construction:
    ## [1] "HighBP"  "Age"     "GenHlth"
    ## Number of terminal nodes:  4 
    ## Residual mean deviance:  1.091 = 3216 / 2948 
    ## Misclassification error rate: 0.2917 = 861 / 2952

``` r
plot(diabetes_tree)
text(diabetes_tree)
```

![](work_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

This tree give all classification level of diabetes to no. Use this tree
to predict.

``` r
fullpred<-predict(diabetes_tree,dplyr::select(test,-"Diabetes_binary"),type="class")
fullTbl<-table(data.frame(fullpred,test[,"Diabetes_binary"]))
fullTbl
```

    ##         Diabetes_binary
    ## fullpred  X0  X1
    ##       X0 896 369
    ##       X1   0   0

``` r
sum(diag(fullTbl)/sum(fullTbl))
```

    ## [1] 0.7083004

In the test dataset it gives all prediction to non-diabetes, as
predicted. Try Gini methods.

``` r
diabetes_tree2<-tree(Diabetes_binary~.,data=train, split = "gini")
summary(diabetes_tree2)
```

    ## 
    ## Classification tree:
    ## tree(formula = Diabetes_binary ~ ., data = train, split = "gini")
    ## Variables actually used in tree construction:
    ##  [1] "MentHlth"             "PhysHlth"             "HighBP"              
    ##  [4] "DiffWalk"             "Age"                  "GenHlth"             
    ##  [7] "Income"               "PhysActivity"         "Fruits"              
    ## [10] "BMI"                  "Sex"                  "HighChol"            
    ## [13] "HeartDiseaseorAttack" "Smoker"               "Veggies"             
    ## [16] "NoDocbcCost"          "Stroke"              
    ## Number of terminal nodes:  335 
    ## Residual mean deviance:  0.6786 = 1776 / 2617 
    ## Misclassification error rate: 0.1636 = 483 / 2952

``` r
plot(diabetes_tree2)
text(diabetes_tree2,pretty=0,cex=0.4)
```

![](work_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

This tree is HUGE, may need pruning. It is great to see
misclassification error rate is low. Pruning can also prevent
over-fitting.

``` r
Prune_diabetes_tree2<-cv.tree(diabetes_tree2,FUN=prune.tree)
Prune_diabetes_tree2
```

    ## $size
    ##   [1] 335 331 330 327 326 324 323 320 319 318 317 315 312 311 307 303 302 301
    ##  [19] 299 297 292 290 289 288 286 285 282 281 280 278 277 275 273 272 271 270
    ##  [37] 268 267 265 261 260 256 255 254 252 251 249 248 247 246 245 242 239 238
    ##  [55] 237 236 234 232 231 229 226 225 224 223 221 220 218 217 213 212 210 208
    ##  [73] 206 203 197 195 194 193 192 188 187 186 184 165 164 163 161 158 157 156
    ##  [91] 155 152 151 148 147 144 143 142 141 140 139 136 135 132 131 129 126 124
    ## [109] 123 122 120 119 117 116 114 111 109 108 105 102 101 100  99  96  93  86
    ## [127]  85  82  81  80  79  78  77  76  74  60  59  57  56  54  53  51  50  49
    ## [145]  48  45  38  36  35  30  25  24  23  22  21  19  17  15  13  12  11  10
    ## [163]   9   7   6   5   3   1
    ## 
    ## $dev
    ##   [1] 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709
    ##   [9] 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709
    ##  [17] 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709
    ##  [25] 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709
    ##  [33] 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709
    ##  [41] 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709
    ##  [49] 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709
    ##  [57] 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709
    ##  [65] 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709
    ##  [73] 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709
    ##  [81] 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709
    ##  [89] 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709
    ##  [97] 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709
    ## [105] 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709
    ## [113] 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709
    ## [121] 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709
    ## [129] 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709
    ## [137] 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709
    ## [145] 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709
    ## [153] 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709 3267.709
    ## [161] 3267.709 3267.709 3267.709 3267.709 3267.709 3276.258 3258.879 3272.476
    ## 
    ## $k
    ##   [1]         -Inf  0.000000000  0.009808248  0.052312336  0.066347944
    ##   [6]  0.076027563  0.076630925  0.110320410  0.124356017  0.142497635
    ##  [11]  0.170449129  0.292389861  0.344702198  0.346032120  0.384379699
    ##  [16]  0.402710271  0.415328814  0.428941002  0.449122551  0.451138945
    ##  [21]  0.483145136  0.496146077  0.537349269  0.634971413  0.665509227
    ##  [26]  0.711602338  0.752510350  0.754106426  0.759802163  0.796672617
    ##  [31]  1.098808624  1.131720852  1.228860378  1.247616797  1.309295243
    ##  [36]  1.463293479  1.497635232  1.539141100  1.641471232  1.697969906
    ##  [41]  1.726092435  1.727143682  1.752094317  1.808419097  1.880039358
    ##  [46]  1.941166297  1.949146403  1.988891390  2.014678356  2.066690619
    ##  [51]  2.074598572  2.088842884  2.154346347  2.275193062  2.343876568
    ##  [56]  2.420727844  2.477308843  2.558118221  2.572221468  2.623398603
    ##  [61]  2.749927186  2.782871703  2.792894868  2.806619358  2.831273689
    ##  [66]  2.972923396  2.974122095  3.027056304  3.081106834  3.107482672
    ##  [71]  3.220765374  3.225196045  3.261730608  3.277931801  3.316020525
    ##  [76]  3.378980646  3.444333619  3.460295892  3.468607353  3.556610517
    ##  [81]  3.566795703  3.574125602  3.700948217  3.868170491  3.953515505
    ##  [86]  4.069969003  4.219007494  4.232735191  4.246497836  4.269598291
    ##  [91]  4.360099764  4.437430242  4.484594236  4.508017491  4.687753137
    ##  [96]  4.692085472  4.747444505  4.770764873  4.810919467  4.857470275
    ## [101]  4.864310495  4.870018728  4.957931124  5.040257150  5.170166282
    ## [106]  5.177760115  5.179672906  5.181633483  5.210454530  5.232342983
    ## [111]  5.286068754  5.296748234  5.439594208  5.487169371  5.574000005
    ## [116]  5.579143655  5.579969401  5.594483217  5.684935754  5.742527504
    ## [121]  5.808908829  5.871556864  6.022848103  6.023572414  6.056260007
    ## [126]  6.087309847  6.112117619  6.169508392  6.363349886  6.411878331
    ## [131]  6.616918476  6.688280589  6.720926847  6.997141902  7.082682673
    ## [136]  7.273227515  7.450201280  7.456066457  7.560674637  7.607280018
    ## [141]  7.721804871  8.182450365  8.301709362  8.423124228  8.434548241
    ## [146]  8.588869904  8.649747755  8.756002182  8.947465263  9.307650534
    ## [151]  9.381313718  9.582517839  9.751468740 10.897632187 11.383776304
    ## [156] 12.311991411 12.864092848 14.257699220 14.410662165 16.331866892
    ## [161] 16.652878523 19.312044207 20.715868969 22.860134997 27.693856638
    ## [166] 40.570411567 51.473929825 63.015387322
    ## 
    ## $method
    ## [1] "deviance"
    ## 
    ## attr(,"class")
    ## [1] "prune"         "tree.sequence"

``` r
plot(Prune_diabetes_tree2$size,Prune_diabetes_tree2$dev,type="b")
```

![](work_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

Looks like deviation is the smallest when tree size = 3. Use size =3 as
the best option for tree size. Then we need to prune trees using the
parameter of size.

``` r
Prune_final_diabetes_tree2<-prune.misclass(diabetes_tree2,best=3)
plot(Prune_final_diabetes_tree2)
text(Prune_final_diabetes_tree2)
```

![](work_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
summary(Prune_final_diabetes_tree2)
```

    ## 
    ## Classification tree:
    ## snip.tree(tree = diabetes_tree2, nodes = c(4L, 47L, 10L, 46L, 
    ## 22L, 3L))
    ## Variables actually used in tree construction:
    ## [1] "MentHlth"             "PhysHlth"             "HeartDiseaseorAttack"
    ## [4] "Income"               "BMI"                 
    ## Number of terminal nodes:  6 
    ## Residual mean deviance:  1.173 = 3455 / 2946 
    ## Misclassification error rate: 0.2798 = 826 / 2952

Why the misclassification tree increased?

``` r
fullpred2<-predict(diabetes_tree2,dplyr::select(test,-"Diabetes_binary"),type="class")
prunepred2<-predict(Prune_final_diabetes_tree2,dplyr::select(test,-"Diabetes_binary"),type="class")
fullTbl2<-table(data.frame(fullpred2,test[,"Diabetes_binary"]))
fullTbl2
```

    ##          Diabetes_binary
    ## fullpred2  X0  X1
    ##        X0 710 231
    ##        X1 186 138

``` r
sum(diag(fullTbl2)/sum(fullTbl2))
```

    ## [1] 0.6703557

``` r
pruneTbl2<-table(data.frame(prunepred2,test[,"Diabetes_binary"]))
pruneTbl2
```

    ##           Diabetes_binary
    ## prunepred2  X0  X1
    ##         X0 881 353
    ##         X1  15  16

``` r
sum(diag(pruneTbl2)/sum(pruneTbl2))
```

    ## [1] 0.7090909

Although the misclassification increased after pruning, the
misclassfication rate on the prediction dataset actually decreased.
