---
title: Blog 3
author: Nick Dominguez
date: '2024-09-23'
slug: prediction-3
---

# Prediction #3: Polling Data

**September 23, 2024** -  This week’s model focuses on one of the staples of some of today’s most widely known election forecasting models: poll data. But how can we disentangle the deluge of polls and glean insight on what the outcome of the election may be? 

Polls capture voter sentiments at a moment in time, and when polls are put together and averaged, we can get a clearer picture of how voters feel about the candidates over the course of the election. Because there are numerous pollsters that all have their own methods of sampling and measuring, we often look at polling aggregates such as FiveThirtyEight and RealClearPolitics. Forecasters like Nate Silver use polling averages in their election models, rather than just basing their models on a singular poll. Therefore, my model does the same. 

With these facts in mind, I built a prediction model based on previous (2016 and 2020) and current poll data. In our models, we want to include as many polls as possible, but there are some constraints. Not all polls are built the same. We do not want poorly fielded polls to skew our averages and thus our predictions. Therefore, when selecting the statewide poll data from 2016 and 2020 to train my model, I first subset to not include the lowest quality of polls; polls with a D rating or a numeric rating of less than 1. 

Another constraint we have to address is the limit of time frame we have for polls. Vice President Kamala Harris did not become the presumptive Democratic nominee until President Biden dropped out and endorsed her on July 21, 2024. The vast majority of  general election polling before this time was Biden vs. Trump polling. Therefore, my model only uses poll data from July 21, 2024 to September 9, 2024, the most recent polls I have in my 2024 dataset. As a result, when I selected the polling data to construct the model, I only included 2016 and 2020 poll data from this time period, relative to the election date of those respective election years. 

To predict outcomes in individual states, I calculated polling averages for each state for polls for the 2016 and 2020 elections in this late July to early September time period for the Democratic candidates to predict Democratic two-party vote share as the outcome variable. In addition to the period polling average, I added state as a factor variable and election year as a numeric variable to account for state differences and election cycle changes. I also ran regressions that did or did not include the variables `state` and `year`, but the regression that had the highest `\(r^2\)` of 0.992 and the lowest RMSE of 1.567 was the model that included both state and year in its construction. In summary, my model explains an extremely large proportion of the outcome variability (good in-sample fit) but does not overfit and therefore has good predictive power, as evidenced by the RMSE from the 1000 repetition cross validation. 

As for the test data, I use 2024 state polling averages from late July to early September to predict Harris’ two party vote share this November. Unfortunately, although the model has 2016 and 2020 averages for every state, there are some states that do not have any polls during this time period in 2024 (see my data and RealClearPolitics). As a result, the model can only predict the 2024 Democrat two-party vote share for 34 states. The results of this poll data model forecast are shown below. 


























<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" width="672" />

This model predicts that Democrat Vice President Kamala Harris will win with *303* electoral votes compared to Republican former President Donald Trump’s *130* electoral votes. But, this is not a full prediction, as there is insufficient data to predict outcomes for 16 states and the District of Columbia. However, it makes sense that these states have no polls, as they all are either consistently Republican or Democratic on the presidential level, as evidenced by the graph below. Even if we did have 2024 poll data for these states, it is unlikely that any of these states would flip.  


<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" width="672" />


To incorporate these states into a final prediction for this week, I will assume that these states will vote in line with how they voted in 2016 and 2020. Summing these results, we get a final prediction of this week of *349* electoral votes for Harris, and *189* for Trump; representing a large Democratic victory.

Although my model predicts a Harris win for the 34 states I was able to predict outcomes for, these are mere estimates and regressions contain error. As seen below, when we add error bars for a 95% confidence interval for predicted Democrat two-party vote share, there are  numerous Harris states in whose intervals straddle the 50% line, meaning that it is possible for Trump to carry some, if not all, of the closest states that Harris is predicted to win, including Pennsylvania, Wisconsin, Arizona, Florida, Georgia, North Carolina. Similarly, the intervals demonstrate that it is possible for Harris to win Texas, Ohio, and Iowa. Running my model with more polls and polls closer to the election on November 5th could change this, but for now, the election is still anyone’s game. 


<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-15-1.png" width="672" />
