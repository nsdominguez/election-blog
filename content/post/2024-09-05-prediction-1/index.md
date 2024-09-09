---
title: Blog 1
author: Nick Dominguez
date: '2024-09-09'
slug: prediction-1
---


``` r
# load data and packages 
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
## ✔ purrr     1.0.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(maps)
```

```
## 
## Attaching package: 'maps'
## 
## The following object is masked from 'package:purrr':
## 
##     map
```


``` r
# create custom ggplot theme

# add 50 state labels
```

# Prediction #1:
#Using Past Election Results

September 9, 2024 -- Welcome to the first week of Nick's Picks, my forecasting model for the 2024 Presidential Election! For this week, I'm starting the basics of my model with a simple factor of prediction: what has happened before. 

The graph below displays the Electoral College map of every presidential election since 1980. Though there is some variation, it is clear that since 2000, and especially in the past few election cycles, many votes consistently vote for the same party each election on the presidential level, such as the West Coast and New England states voting consistently for the Democrats, and the Deep South and Great Plains states mainly voting for Republican candidates. However, there are a few states in the past few elections that have swung between Democrats and Republicans, including, but not limited to, states such as Wisconsin, Michigan, Pennsylvania, Ohio, Florida, Arizona, and North Carolina. 


``` r
# create facet map with previous election results
```

Looking in more closely, 


``` r
# create 2016 and 2020 shaded maps. 
```

Using these two past presidential elections, I construct my model to predict the two-party vote share and Electoral College results for the 2024 Presidential Election. Although I include both sets of election results, I weigh the 2020 Presidential Election between Trump and Biden more heavily as it was more recent than the 2016 Presidential Election between Trump and Clinton. This division, of a 3/4 weight on 2020 and a 1/4 weight on 2016 is based on [intert name's] model. 

Based on this weighted lag model of past presidential election results, I predict Vice President Kamala Harris wins the national two-party popular with x% of the vote compared to the Republican candidate, former President Donald Trump. With this two-party vote share model, I predict that Harris will win the 2024 Presidential Election with x electoral votes compared to Trump's x. 


``` r
# create prediction map for 2024 
```

