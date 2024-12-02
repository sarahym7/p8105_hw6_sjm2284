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
library(broom)
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
  mutate(victim_age = as.numeric(victim_age)) %>% 
  mutate(resolved = as.numeric(disposition_case == "resolved"))
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

Balt_fit_logistic %>% knitr::kable(digits = 2)
```

|   OR |  low | high |
|-----:|-----:|-----:|
| 3.16 | 2.00 | 5.06 |
| 0.99 | 0.99 | 1.00 |
| 0.43 | 0.31 | 0.61 |
| 0.43 | 0.32 | 0.56 |

## Problem 2C

``` r
# taking df homicide and nesting data by city
homicide_data = homicide %>%
  group_by(city_state) %>%
  nest()

# create a function for the logistic regression and extract OR and CI
logistic = function(data) {
  fit = glm(resolved ~ victim_age + victim_race + victim_sex, data = data, family = binomial())
  tidy_fit = tidy(fit, conf.int = TRUE) %>%
    mutate(
      OR = exp(estimate),
      OR_conf.low = exp(conf.low),
      OR_conf.high = exp(conf.high)
    ) %>%
    filter(term == "victim_sexMale") %>%
    select(term, log_OR = estimate, OR, p.value, OR_conf.low, OR_conf.high)
  return(tidy_fit)
}

# Apply the function to each city's data and unnest the results to return values
city_results =
  homicide_data %>%
  mutate(model = purrr::map(data, logistic)) %>%
  unnest(model) %>% 
    select (OR, OR_conf.low, OR_conf.high) %>% 
  rename(Low = "OR_conf.low",
          High = "OR_conf.high") 
```

    ## Warning: There were 45 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `model = purrr::map(data, logistic)`.
    ## ℹ In group 1: `city_state = "Albuquerque,NM"`.
    ## Caused by warning:
    ## ! glm.fit: fitted probabilities numerically 0 or 1 occurred
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 44 remaining warnings.

    ## Adding missing grouping variables: `city_state`

``` r
# Display the results
city_results %>% knitr::kable(digits = 2)
```

| city_state        |   OR |  Low | High |
|:------------------|-----:|-----:|-----:|
| Albuquerque,NM    | 1.77 | 0.82 | 3.76 |
| Atlanta,GA        | 1.00 | 0.68 | 1.46 |
| Baltimore,MD      | 0.43 | 0.32 | 0.56 |
| Baton Rouge,LA    | 0.38 | 0.20 | 0.68 |
| Birmingham,AL     | 0.87 | 0.57 | 1.31 |
| Boston,MA         | 0.67 | 0.35 | 1.28 |
| Buffalo,NY        | 0.52 | 0.29 | 0.94 |
| Charlotte,NC      | 0.88 | 0.55 | 1.39 |
| Chicago,IL        | 0.41 | 0.34 | 0.50 |
| Cincinnati,OH     | 0.40 | 0.23 | 0.67 |
| Columbus,OH       | 0.53 | 0.38 | 0.75 |
| Denver,CO         | 0.48 | 0.23 | 0.96 |
| Detroit,MI        | 0.58 | 0.46 | 0.73 |
| Durham,NC         | 0.81 | 0.38 | 1.66 |
| Fort Worth,TX     | 0.67 | 0.39 | 1.12 |
| Fresno,CA         | 1.34 | 0.57 | 3.05 |
| Houston,TX        | 0.71 | 0.56 | 0.91 |
| Indianapolis,IN   | 0.92 | 0.68 | 1.24 |
| Jacksonville,FL   | 0.72 | 0.54 | 0.97 |
| Las Vegas,NV      | 0.84 | 0.61 | 1.15 |
| Long Beach,CA     | 0.41 | 0.14 | 1.02 |
| Los Angeles,CA    | 0.66 | 0.46 | 0.95 |
| Louisville,KY     | 0.49 | 0.30 | 0.78 |
| Memphis,TN        | 0.72 | 0.53 | 0.98 |
| Miami,FL          | 0.52 | 0.30 | 0.87 |
| Milwaukee,wI      | 0.73 | 0.50 | 1.05 |
| Minneapolis,MN    | 0.95 | 0.48 | 1.88 |
| Nashville,TN      | 1.03 | 0.68 | 1.56 |
| New Orleans,LA    | 0.58 | 0.42 | 0.81 |
| New York,NY       | 0.26 | 0.13 | 0.49 |
| Oakland,CA        | 0.56 | 0.36 | 0.87 |
| Oklahoma City,OK  | 0.97 | 0.62 | 1.52 |
| Omaha,NE          | 0.38 | 0.20 | 0.71 |
| Philadelphia,PA   | 0.50 | 0.38 | 0.65 |
| Pittsburgh,PA     | 0.43 | 0.26 | 0.70 |
| Richmond,VA       | 1.01 | 0.48 | 1.99 |
| San Antonio,TX    | 0.70 | 0.39 | 1.24 |
| Sacramento,CA     | 0.67 | 0.33 | 1.31 |
| Savannah,GA       | 0.87 | 0.42 | 1.78 |
| San Bernardino,CA | 0.50 | 0.17 | 1.46 |
| San Diego,CA      | 0.41 | 0.19 | 0.83 |
| San Francisco,CA  | 0.61 | 0.31 | 1.16 |
| St. Louis,MO      | 0.70 | 0.53 | 0.93 |
| Stockton,CA       | 1.35 | 0.63 | 2.99 |
| Tampa,FL          | 0.81 | 0.34 | 1.86 |
| Tulsa,OK          | 0.98 | 0.61 | 1.54 |
| Washington,DC     | 0.69 | 0.47 | 1.01 |

## Problem 2D

``` r
# plotting the estimated ORs and CIs for each city according to estimated OR 

city_estimated = ggplot(city_results, aes(x = reorder(city_state, OR), y =OR))+
                          geom_point()+
                          geom_line()+
                          geom_errorbar(aes(ymin = Low, ymax = High), width = 0.2)+
                          labs(title = "Estimated Odds Ratio and CI for Homcides Across Cities",
                                                       x = "City",
                                                        y = "Odds Ratio")+
                              theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

Based on the plot we can see that New Mexico has the largest odds ratio
and confidence interval in comparison to New York who has the smallest
OR and CI. This is interesting given that it is common knowledge that
Chicago, IL has been considered the city and state with the highest
homicide rate yet its OR is less than New York and Baton Rogue, LA.

# Problem 3
