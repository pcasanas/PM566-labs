Lab5
================
Patrick Casanas
2024-10-03

``` r
library(data.table)
library(dtplyr)
library (dplyr)
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
stations <- fread("https://noaa-isd-pds.s3.amazonaws.com/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

stations <- unique(stations[, list(USAF, CTRY, STATE)])

stations <- stations[!is.na(USAF)]

stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]

met <- fread("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz")
```

``` r
met_merged <- merge(
  x=met,
  y=stations,
  by.x="USAFID",
  by.y="USAF",
  all.x=TRUE,
  all.y=FALSE
)
head (met_merged)
```

    ## Key: <USAFID>
    ##    USAFID  WBAN  year month   day  hour   min   lat      lon  elev wind.dir
    ##     <int> <int> <int> <int> <int> <int> <int> <num>    <num> <int>    <int>
    ## 1: 690150 93121  2019     8     1     0    56  34.3 -116.166   696      220
    ## 2: 690150 93121  2019     8     1     1    56  34.3 -116.166   696      230
    ## 3: 690150 93121  2019     8     1     2    56  34.3 -116.166   696      230
    ## 4: 690150 93121  2019     8     1     3    56  34.3 -116.166   696      210
    ## 5: 690150 93121  2019     8     1     4    56  34.3 -116.166   696      120
    ## 6: 690150 93121  2019     8     1     5    56  34.3 -116.166   696       NA
    ##    wind.dir.qc wind.type.code wind.sp wind.sp.qc ceiling.ht ceiling.ht.qc
    ##         <char>         <char>   <num>     <char>      <int>         <int>
    ## 1:           5              N     5.7          5      22000             5
    ## 2:           5              N     8.2          5      22000             5
    ## 3:           5              N     6.7          5      22000             5
    ## 4:           5              N     5.1          5      22000             5
    ## 5:           5              N     2.1          5      22000             5
    ## 6:           9              C     0.0          5      22000             5
    ##    ceiling.ht.method sky.cond vis.dist vis.dist.qc vis.var vis.var.qc  temp
    ##               <char>   <char>    <int>      <char>  <char>     <char> <num>
    ## 1:                 9        N    16093           5       N          5  37.2
    ## 2:                 9        N    16093           5       N          5  35.6
    ## 3:                 9        N    16093           5       N          5  34.4
    ## 4:                 9        N    16093           5       N          5  33.3
    ## 5:                 9        N    16093           5       N          5  32.8
    ## 6:                 9        N    16093           5       N          5  31.1
    ##    temp.qc dew.point dew.point.qc atm.press atm.press.qc       rh   CTRY  STATE
    ##     <char>     <num>       <char>     <num>        <int>    <num> <char> <char>
    ## 1:       5      10.6            5    1009.9            5 19.88127     US     CA
    ## 2:       5      10.6            5    1010.3            5 21.76098     US     CA
    ## 3:       5       7.2            5    1010.6            5 18.48212     US     CA
    ## 4:       5       5.0            5    1011.6            5 16.88862     US     CA
    ## 5:       5       5.0            5    1012.7            5 17.38410     US     CA
    ## 6:       5       5.6            5    1012.7            5 20.01540     US     CA

``` r
median_temp <- median(met_merged$temp, na.rm = TRUE)
median_wind <- median(met_merged$wind.sp, na.rm = TRUE)
median_pressure <- median(met_merged$atm.press, na.rm = TRUE)

closest_temp_station <- met_merged[!is.na(met_merged$temp)][which.min(abs(met_merged$temp - median_temp)), ]
closest_wind_station <- met_merged[!is.na(met_merged$wind.sp)][which.min(abs(met_merged$wind.sp - median_wind)), ]
closest_pressure_station <- met_merged[!is.na(met_merged$atm.press)][which.min(abs(met_merged$atm.press - median_pressure)), ]

print("Station closest to median temperature:")
```

    ## [1] "Station closest to median temperature:"

``` r
print(closest_temp_station)
```

    ## Key: <USAFID>
    ##    USAFID  WBAN  year month   day  hour   min    lat     lon  elev wind.dir
    ##     <int> <int> <int> <int> <int> <int> <int>  <num>   <num> <int>    <int>
    ## 1: 720113 54829  2019     8     1     9    57 42.543 -83.178   222       NA
    ##    wind.dir.qc wind.type.code wind.sp wind.sp.qc ceiling.ht ceiling.ht.qc
    ##         <char>         <char>   <num>     <char>      <int>         <int>
    ## 1:           9              C       0          5      22000             5
    ##    ceiling.ht.method sky.cond vis.dist vis.dist.qc vis.var vis.var.qc  temp
    ##               <char>   <char>    <int>      <char>  <char>     <char> <num>
    ## 1:                 9        N    16093           5       N          5  15.5
    ##    temp.qc dew.point dew.point.qc atm.press atm.press.qc       rh   CTRY  STATE
    ##     <char>     <num>       <char>     <num>        <int>    <num> <char> <char>
    ## 1:       5      12.8            5        NA            9 84.08336     US     MI

``` r
print("Station closest to median wind speed:")
```

    ## [1] "Station closest to median wind speed:"

``` r
print(closest_wind_station)
```

    ## Key: <USAFID>
    ##    USAFID  WBAN  year month   day  hour   min   lat      lon  elev wind.dir
    ##     <int> <int> <int> <int> <int> <int> <int> <num>    <num> <int>    <int>
    ## 1: 690150 93121  2019     8     1     4    56  34.3 -116.166   696      120
    ##    wind.dir.qc wind.type.code wind.sp wind.sp.qc ceiling.ht ceiling.ht.qc
    ##         <char>         <char>   <num>     <char>      <int>         <int>
    ## 1:           5              N     2.1          5      22000             5
    ##    ceiling.ht.method sky.cond vis.dist vis.dist.qc vis.var vis.var.qc  temp
    ##               <char>   <char>    <int>      <char>  <char>     <char> <num>
    ## 1:                 9        N    16093           5       N          5  32.8
    ##    temp.qc dew.point dew.point.qc atm.press atm.press.qc      rh   CTRY  STATE
    ##     <char>     <num>       <char>     <num>        <int>   <num> <char> <char>
    ## 1:       5         5            5    1012.7            5 17.3841     US     CA

``` r
print("Station closest to median atmospheric pressure:")
```

    ## [1] "Station closest to median atmospheric pressure:"

``` r
print(closest_pressure_station)
```

    ## Key: <USAFID>
    ##    USAFID  WBAN  year month   day  hour   min   lat      lon  elev wind.dir
    ##     <int> <int> <int> <int> <int> <int> <int> <num>    <num> <int>    <int>
    ## 1: 690150 93121  2019     8     1    18    56  34.3 -116.166   696       NA
    ##    wind.dir.qc wind.type.code wind.sp wind.sp.qc ceiling.ht ceiling.ht.qc
    ##         <char>         <char>   <num>     <char>      <int>         <int>
    ## 1:           9              V     2.1          5      22000             5
    ##    ceiling.ht.method sky.cond vis.dist vis.dist.qc vis.var vis.var.qc  temp
    ##               <char>   <char>    <int>      <char>  <char>     <char> <num>
    ## 1:                 9        N    16093           5       N          5  37.2
    ##    temp.qc dew.point dew.point.qc atm.press atm.press.qc       rh   CTRY  STATE
    ##     <char>     <num>       <char>     <num>        <int>    <num> <char> <char>
    ## 1:       5         5            5    1014.1            5 13.51449     US     CA

The closest station to the median temperature is station 720113.

The closest station to the median wind speed is station 690150.

The closest station to the median atmospheric pressure was also 690150.
