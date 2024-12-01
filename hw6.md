HW6
================
Sarahy Martinez
2024-12-01

``` r
library(tidyverse)
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

``` r
set.seed(1)
```

\#Problem 1

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(id, USW00094728 = "CentralPark_NY"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

# Problem 2

## Problem 2A

``` r
#reading in Washington post data 

homicide_raw_data = read.csv("./homicide-data.csv")

homicide = homicide_raw_data %>% 
  unite(city_state, city, state, sep = ",") %>% 
  mutate(disposition_case = case_when( disposition == "Closed without arrest"~ "unresolved",
                                  disposition == "Closed by arrest" ~ "resolved",
                                  disposition == "Open/No arrest" ~ "unresolved")) %>%
 filter(!city_state %in% c("Dallas,TX", "Phoenix,AZ", "Kansas City,MO", "Tulsa,AL")) %>% 
  filter(victim_race %in% c("White", "Black")) %>% 
  mutate(victim_age = as.numeric(victim_age))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `victim_age = as.numeric(victim_age)`.
    ## Caused by warning:
    ## ! NAs introduced by coercion

## Problem 2B

Using these data, we can fit a logistic regression for the binary
“resolved” outcome and victim demographics as predictors. This uses the
glm function with the family specified to account for the non-Gaussian
outcome distribution.

``` r
baltimore_df = 
  homicide %>% 
  filter(city_state == "Baltimore,MD") %>% 
  mutate(
    resolved = as.numeric(disposition_case == "resolved"),
    victim_age = as.numeric(victim_age),
    victim_race = fct_relevel(victim_race, "White")) %>% 
  select(resolved,victim_age, victim_race, victim_sex)

# applying the glm function
fit_logistic = 
  baltimore_df %>% 
  glm(resolved ~ victim_age + victim_race + victim_sex, data = ., family = binomial()) 

#using the broom tidy function and estimate the odds ratio and confidence interval

Balt_fit_logistic = 
  broom::tidy(fit_logistic, conf.int = TRUE) %>% #conf.int function used to calculate the CI
  mutate(
    OR = exp(estimate),
    OR_conf.low = exp(conf.low),
    OR_conf.high = exp(conf.high)
  ) %>%
  select (OR, OR_conf.low, OR_conf.high) %>% 
  rename(low = "OR_conf.low",
          high = "OR_conf.high") 

Balt_fit_logistic %>% knitr::kable(digits = 3)
```

|    OR |   low |  high |
|------:|------:|------:|
| 3.164 | 1.998 | 5.057 |
| 0.993 | 0.987 | 1.000 |
| 0.431 | 0.305 | 0.606 |
| 0.426 | 0.324 | 0.558 |

# Problem 3
