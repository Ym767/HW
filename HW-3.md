HW3
================
Yeming Wei
10/5/2021

# What time of day should you fly if you want to avoid delays as much as possible?

``` r
library(nycflights13)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)

flights %>%
  group_by(hour) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE),dep_delay=mean(dep_delay, na.rm = TRUE))
```

    ## # A tibble: 20 x 3
    ##     hour arr_delay dep_delay
    ##    <dbl>     <dbl>     <dbl>
    ##  1     1   NaN       NaN    
    ##  2     5    -4.80      0.688
    ##  3     6    -3.38      1.64 
    ##  4     7    -5.30      1.91 
    ##  5     8    -1.11      4.13 
    ##  6     9    -1.45      4.58 
    ##  7    10     0.954     6.50 
    ##  8    11     1.48      7.19 
    ##  9    12     3.49      8.61 
    ## 10    13     6.54     11.4  
    ## 11    14     9.20     13.8  
    ## 12    15    12.3      16.9  
    ## 13    16    12.6      18.8  
    ## 14    17    16.0      21.1  
    ## 15    18    14.8      21.1  
    ## 16    19    16.7      24.8  
    ## 17    20    16.7      24.3  
    ## 18    21    18.4      24.2  
    ## 19    22    16.0      18.8  
    ## 20    23    11.8      14.0

### I think it is much less likely to be delayed on a morning flight.

# Depend on Season

``` r
S_flights <- flights %>%
  mutate(season = ifelse(month >= 3 & month <= 5, 'spring', 
                         ifelse(month>=6 & month <= 8, "Summer",
                                ifelse(month>=9&month <=11, "Autumn","winter"))))%>%
  group_by(season)%>%
  summarise(dep_delay=mean(dep_delay, na.rm = TRUE))%>%
  arrange(dep_delay)
  
ggplot(S_flights, aes(x=season, y=dep_delay))+
  geom_bar(stat="identity")+
  labs(
    x="season",
    y="Dep_delay")
```

![](HW-3_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
S_flights <- flights %>%
  mutate(season = ifelse(month >= 3 & month <= 5, 'spring', 
                         ifelse(month>=6 & month <= 8, "Summer",
                                ifelse(month>=9&month <=11, "Autumn","winter"))))%>%
  
group_by(season)%>%
  summarise(arr_delay=mean(arr_delay, na.rm = TRUE))%>%
  arrange(arr_delay)
  
ggplot(S_flights, aes(x=season, y=arr_delay))+
  geom_bar(stat="identity")+
  labs(
    x="season",
    y="Arr_delay")
```

![](HW-3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### The two graphs show the average delay time in different seasons, and summer flights have the most delay of departure or arrival.

# Depend on Airlines

``` r
A_delay <- flights %>%
  group_by(carrier) %>%
  summarise(dep_delay = mean(dep_delay,na.rm = T))%>%
  arrange(dep_delay)

ggplot(A_delay, aes(x=carrier, y=dep_delay))+
  geom_bar(stat = "identity") +
  labs(x=" carrier",
       y="Dep_delay time")
```

![](HW-3_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
A_delay <- flights %>%
  group_by(carrier) %>%
  summarise(arr_delay = mean(arr_delay,na.rm = T))%>%
  arrange(arr_delay)

ggplot(A_delay, aes(x=carrier, y=arr_delay))+
  geom_bar(stat = "identity") +
  labs(x=" carrier",
       y="Arr_delay time")
```

![](HW-3_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### The above two graphs show the relationship between delay time and airlines. The top three airlines for delays are EV, F9, and FL. However, HA, US and HA are less likely to be a delay.

# Depend on Airport

``` r
A_1 <- flights %>%
  group_by(origin) %>%
  mutate(dep_delay_lag = lag(dep_delay)) %>%
  arrange(dep_delay) 

ggplot(A_1,aes(y = dep_delay, x = origin)) +
  geom_bar(stat = "identity") +
  labs(y = " Delay time", x = "Departure Delay")
```

    ## Warning: Removed 8255 rows containing missing values (position_stack).

![](HW-3_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
A_1 <- flights %>%
  group_by(origin) %>%
  mutate(arr_delay_lag = lag(arr_delay)) %>%
  arrange(arr_delay) 

ggplot(A_1,aes(y = arr_delay, x = origin)) +
  geom_bar(stat = "identity") +
  labs(y = " Delay time", x = "Arrive Delay")
```

    ## Warning: Removed 9430 rows containing missing values (position_stack).

![](HW-3_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### The above two graphs show the relationship between delay time and Airport. The EWR has the most delay time.

<https://github.com/Ym767/HW/blob/master/HW%203.Rmd>
