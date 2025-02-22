Lab 06 - Ugly charts and Simpson’s paradox
================
Lilly McClendon
02-21-2025

### Load packages and data

``` r
library(tidyverse) 
library(dsbox)
library(mosaicData) 
```

``` r
data(Whickham)

?Whickham

#library(performance)
#performance::compare_performance()
```

### Exercise 1

I think these data came from an observational study because it is
described as surveying participants between 1972 and 1974 and then
following up 20 years later. Observational studies frequently involve
researchers monitoring participants over time. Unlike in an experimental
study, in this study, there was no manipulation of variables by the
researcher or a control group. Observational studies are commonly used
in public health or epidemiology research where the goal is to
understand a potential association between an exposure and outcome.
Further I think that this is an observational study because it would not
be ethical to manipulate smoking knowing the risks to see the potential
disease outcomes.

### Exercise 2

``` r
view(Whickham)
nrow(Whickham)
```

    ## [1] 1314

``` r
ncol(Whickham)
```

    ## [1] 3

There are 1,314 observations in this data set. Each observation
represents a subset of the survey sample and included UK women who
either reported never smoked or currently smoked.

### Exercise 3

There are 3 variables in the data set. Smoking (dichotomous,
categorical) and age (continuous) can be considered explanatory
variables and outcome can be considered a response variable
(dichotomous, categorical). In this study, smoking is observed to see if
there is an association with mortality.

``` r
library(ggplot2)
ggplot(Whickham, aes(x = smoker)) + 
  geom_bar() + 
  labs(title = "Smoking Status")
```

![](lab-06_files/figure-gfm/smoking-visualizations-1.png)<!-- -->

``` r
library(ggplot2)
ggplot(Whickham, aes(x = outcome)) + 
  geom_bar() + 
  labs(title = "Mortality Outcomes")
```

![](lab-06_files/figure-gfm/mortality-visualizations-1.png)<!-- -->

``` r
library(ggplot2)
ggplot(Whickham, aes(x=age)) + 
  geom_histogram(binwidth = 2, color = "navy", fill = "grey") + 
  labs(title = "Age of Participants", x = "Age (years)") + 
  theme_minimal()
```

![](lab-06_files/figure-gfm/age-visualization-1.png)<!-- -->

### Exercise 4

I would expect the relationship between smoking status and health
outcome to be that more individuals who smoked have died.

### Exercise 5

``` r
Whickham %>%
  count(smoker, outcome) %>%
  group_by(smoker) %>%
  mutate(prop_outcome = n / sum(n))
```

    ## # A tibble: 4 × 4
    ## # Groups:   smoker [2]
    ##   smoker outcome     n prop_outcome
    ##   <fct>  <fct>   <int>        <dbl>
    ## 1 No     Alive     502        0.686
    ## 2 No     Dead      230        0.314
    ## 3 Yes    Alive     443        0.761
    ## 4 Yes    Dead      139        0.239

``` r
ggplot(Whickham, aes(y = smoker, fill = outcome)) + 
  geom_bar(position = "fill") + 
  labs(title = "Outcome by Smoking Status", 
       y = NULL, x = NULL)
```

![](lab-06_files/figure-gfm/smoking-outcome-visualization-1.png)<!-- -->

The relationship between smoking status and health outcome did not meet
my expectation that individuals who smoked were more likely to have died
compared to individuals that did not smoke. The probability of being
alive was highst for individuals who smoked, and individuals who didn’t
smoke had a higher probability of being deceased.

### Exercise 6

``` r
library(dplyr)
Whickham <- Whickham %>% 
  mutate(
    age_cat = dplyr::case_when(
      age <= 44 ~ "18-44", 
      age > 44 & age <= 64 ~ "45-64",
      age > 64 ~ "65+"
  )
)
```

### Exercise 7

``` r
ggplot(Whickham, aes(y = smoker, fill = outcome)) + 
  geom_bar(position = "fill") + 
  facet_wrap(. ~ age_cat) +
  labs(title = "Outcome by Smoking Status and Age Category", 
       y = NULL, x = NULL)
```

![](lab-06_files/figure-gfm/re-created-visualization-1.png)<!-- -->

``` r
Whickham %>%
  count(smoker, age_cat, outcome) %>%
  group_by() %>%
  mutate(prop_outcome = n / sum(n))
```

    ## # A tibble: 12 × 5
    ##    smoker age_cat outcome     n prop_outcome
    ##    <fct>  <chr>   <fct>   <int>        <dbl>
    ##  1 No     18-44   Alive     327      0.249  
    ##  2 No     18-44   Dead       12      0.00913
    ##  3 No     45-64   Alive     147      0.112  
    ##  4 No     45-64   Dead       53      0.0403 
    ##  5 No     65+     Alive      28      0.0213 
    ##  6 No     65+     Dead      165      0.126  
    ##  7 Yes    18-44   Alive     270      0.205  
    ##  8 Yes    18-44   Dead       15      0.0114 
    ##  9 Yes    45-64   Alive     167      0.127  
    ## 10 Yes    45-64   Dead       80      0.0609 
    ## 11 Yes    65+     Alive       6      0.00457
    ## 12 Yes    65+     Dead       44      0.0335

When looking at the recreated visualization showing the relationship
between smoking status and health outcome faceted by age category, among
all groups, it appears that across all age groups when individuals
smoke, more of them are deceased. It appears to be particularly
pronounced in the 45-64 age range which may be as that is when diseases
from smoking may arise and lead to early mortality. It may be that when
you examine the 65+ category there is not as dramatic a difference in
mortality outcome between smokers and non-smokers as other health
problems tend to arise in older adults potentially leading to their
death.
