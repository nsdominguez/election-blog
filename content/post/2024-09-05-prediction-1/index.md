---
title: Blog 1
date: '2024-09-09'
slug: prediction-1
---



# Prediction #1: Using Past Election Results

September 9, 2024 – Welcome to the first week of Nick’s Picks, my forecasting model for the 2024 Presidential Election! For this week, I’m starting the basics of my model with a simple factor of prediction: what has happened before.

The graph below displays the Electoral College map of every presidential election since 1980. Though there is some variation, it is clear that since 2000, and especially in the past few election cycles, many votes consistently vote for the same party each election on the presidential level, such as the West Coast and New England states voting consistently for the Democrats, and the Deep South and Great Plains states mainly voting for Republican candidates. However, there are a few states in the past few elections that have swung between Democrats and Republicans, including, but not limited to, states such as Wisconsin, Michigan, Pennsylvania, Ohio, Florida, Arizona, and North Carolina.
 

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-3-1.png" width="672" />


Looking more closely at the two most recent presidential elections, we see that many of these states that have swung between parties in the past few election cycles, were decided by extremely narrow margins. We can see that in 2020, Joe Biden narrowly won the states of Arizona, Georgia, Michigan, Pennsylvania, and Wisconsin, all of which were states narrowly won by Trump in 2016. However, the remainder of the 50 states voted consistently for Republicans or Democrats over these two elections. Therefore, in order to predict the outcome of the 2024 election, it is a good idea to look at historical election data of this time period. 


```
## Warning: Removed 20 rows containing missing values or values outside the scale range
## (`geom_text()`).
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" width="672" />

```
## Warning: Removed 20 rows containing missing values or values outside the scale range
## (`geom_text()`).
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-2.png" width="672" />

Using these two past presidential elections, I construct my model to predict the two-party vote share and Electoral College results for the 2024 Presidential Election. Although I include both sets of election results, I weigh the 2020 Presidential Election between Trump and Biden more heavily as it was more recent than the 2016 Presidential Election between Trump and Clinton. This division, of a 3/4 weight on the 2020 election and a 1/4 weight on the 2016 election is a simple and common method of distributing model influence when using historical election data. 



``` r
# predict electoral college

d_pvstate_wide <- read_csv("clean_wide_state_2pv_1948_2020.csv")
```

```
## Rows: 959 Columns: 14
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (1): state
## dbl (13): year, D_pv, R_pv, D_pv2p, R_pv2p, D_pv_lag1, R_pv_lag1, D_pv2p_lag...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
d_pvstate_wide$region <- tolower(d_pvstate_wide$state)

pv2p_2024_states <- d_pvstate_wide %>%
  filter(year == 2020) %>%
  group_by(state) %>%
  summarize(R_pv2p_2024 = 0.75*R_pv2p + 0.25*R_pv2p_lag1, 
            D_pv2p_2024 = 0.75*D_pv2p + 0.25*D_pv2p_lag1) %>%
  mutate(pv2p_2024_margin = R_pv2p_2024 - D_pv2p_2024, 
         winner = ifelse(R_pv2p_2024 > D_pv2p_2024, "R", "D"), 
         region = tolower(state))

ec <- read_csv("ec_full.csv")
```

```
## Rows: 936 Columns: 3
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (1): state
## dbl (2): electors, year
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
pv2p_2024_states <- pv2p_2024_states %>% 
  mutate(year = 2024) |> 
  left_join(ec, by = c("state", "year"))

pv2p_2024_states_ec <-  pv2p_2024_states %>%
  group_by(winner) %>%
  summarize(electoral_votes = sum(electors))
```

Based on this weighted lagged model of past presidential election results, my model predicts Vice President Kamala Harris wins the national two-party popular vote (disregarding third party candidates) with *51.99%* of the vote compared to the Republican candidate, former President Donald Trump's *48.01%*, representing an approximate D+4 environment. However, the winner of the presidential election is not the popular vote winner. The candidate who wins the presidency wins the majority of Electoral College votes based on winning individual states. Therefore, with this two-party vote share model applied to each individual state, my model predicts that Harris will win the 2024 Presidential Election with *276* electoral votes compared to Trump’s *262*. This margin is closer than previous elections, such as when Biden won with 306 in 2020, or when Trump won with 304 electoral votes in 2016. A map of this victory is illustrated below. As expected, solidly Democratic and Republican states are forecasted to continue to vote for their respective candidates, while the most competitive states remain in the Rust Belt (WI, MI, PA) and the Sun Belt (NV, AZ, GA, NC).  


```
## Warning: Removed 20 rows containing missing values or values outside the scale range
## (`geom_text()`).
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

An important caveat to these projects that I must emphasize is that these results are far from my final prediction. This model, an arguably conservative estimate based on averages, assumes that Harris will underperform Biden in 2020 and outperform Clinton in 2016. While historical trends are part of the story, election outcomes are not simply products of previous results. The model in this week’s blog post is simply the start of a months-long project. In subsequent blog posts, I will be adding more variables to my model to create the most accurate prediction of the 2024 presidential election. Stay tuned!
