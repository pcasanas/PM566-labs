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
library(leaflet)
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

The closest station to the median atmospheric pressure is also 690150.

**Question 2**

``` r
representative_stations <- met_merged %>%
  filter(!is.na(temp), !is.na(wind.sp), !is.na(atm.press), !is.na(elev)) %>%
  group_by(STATE) %>%
  mutate(
    med_temp = median(temp),
    med_wind = median(wind.sp),
    med_pressure = median(atm.press),
    euclidean_dist = sqrt((temp - med_temp)^2 + (wind.sp - med_wind)^2 + (atm.press - med_pressure)^2)
  ) %>%
  filter(euclidean_dist == 0) %>%
  arrange(STATE, elev) %>%
  slice(1)

print(met_merged)
```

    ## Key: <USAFID>
    ##          USAFID  WBAN  year month   day  hour   min    lat      lon  elev
    ##           <int> <int> <int> <int> <int> <int> <int>  <num>    <num> <int>
    ##       1: 690150 93121  2019     8     1     0    56 34.300 -116.166   696
    ##       2: 690150 93121  2019     8     1     1    56 34.300 -116.166   696
    ##       3: 690150 93121  2019     8     1     2    56 34.300 -116.166   696
    ##       4: 690150 93121  2019     8     1     3    56 34.300 -116.166   696
    ##       5: 690150 93121  2019     8     1     4    56 34.300 -116.166   696
    ##      ---                                                                 
    ## 2377339: 726813 94195  2019     8    31    19    56 43.650 -116.633   741
    ## 2377340: 726813 94195  2019     8    31    20    56 43.650 -116.633   741
    ## 2377341: 726813 94195  2019     8    31    21    56 43.650 -116.633   741
    ## 2377342: 726813 94195  2019     8    31    22    56 43.642 -116.636   741
    ## 2377343: 726813 94195  2019     8    31    23    56 43.642 -116.636   741
    ##          wind.dir wind.dir.qc wind.type.code wind.sp wind.sp.qc ceiling.ht
    ##             <int>      <char>         <char>   <num>     <char>      <int>
    ##       1:      220           5              N     5.7          5      22000
    ##       2:      230           5              N     8.2          5      22000
    ##       3:      230           5              N     6.7          5      22000
    ##       4:      210           5              N     5.1          5      22000
    ##       5:      120           5              N     2.1          5      22000
    ##      ---                                                                  
    ## 2377339:       70           5              N     2.1          5      22000
    ## 2377340:       NA           9              C     0.0          5      22000
    ## 2377341:       10           5              N     2.6          5      22000
    ## 2377342:       10           1              N     2.1          1      22000
    ## 2377343:       40           1              N     2.1          1      22000
    ##          ceiling.ht.qc ceiling.ht.method sky.cond vis.dist vis.dist.qc vis.var
    ##                  <int>            <char>   <char>    <int>      <char>  <char>
    ##       1:             5                 9        N    16093           5       N
    ##       2:             5                 9        N    16093           5       N
    ##       3:             5                 9        N    16093           5       N
    ##       4:             5                 9        N    16093           5       N
    ##       5:             5                 9        N    16093           5       N
    ##      ---                                                                      
    ## 2377339:             5                 9        N    16093           5       N
    ## 2377340:             5                 9        N    16093           5       N
    ## 2377341:             5                 9        N    14484           5       N
    ## 2377342:             1                 9        N    16093           1       9
    ## 2377343:             1                 9        N    16093           1       9
    ##          vis.var.qc  temp temp.qc dew.point dew.point.qc atm.press atm.press.qc
    ##              <char> <num>  <char>     <num>       <char>     <num>        <int>
    ##       1:          5  37.2       5      10.6            5    1009.9            5
    ##       2:          5  35.6       5      10.6            5    1010.3            5
    ##       3:          5  34.4       5       7.2            5    1010.6            5
    ##       4:          5  33.3       5       5.0            5    1011.6            5
    ##       5:          5  32.8       5       5.0            5    1012.7            5
    ##      ---                                                                       
    ## 2377339:          5  32.2       5      12.2            5    1012.8            5
    ## 2377340:          5  33.3       5      12.2            5    1011.6            5
    ## 2377341:          5  35.0       5       9.4            5    1010.8            5
    ## 2377342:          9  34.4       1       9.4            1    1010.1            1
    ## 2377343:          9  34.4       1       9.4            1    1009.6            1
    ##                rh   CTRY  STATE
    ##             <num> <char> <char>
    ##       1: 19.88127     US     CA
    ##       2: 21.76098     US     CA
    ##       3: 18.48212     US     CA
    ##       4: 16.88862     US     CA
    ##       5: 17.38410     US     CA
    ##      ---                       
    ## 2377339: 29.40686     US     ID
    ## 2377340: 27.60422     US     ID
    ## 2377341: 20.76325     US     ID
    ## 2377342: 21.48631     US     ID
    ## 2377343: 21.48631     US     ID

**Question 3**

``` r
midpoint_stations <- met_merged %>%
  group_by(STATE) %>%
  summarize(
    mid_lat = mean(lat, na.rm = TRUE),
    mid_lon = mean(lon, na.rm = TRUE)
  )

print(midpoint_stations)
```

    ## # A tibble: 48 × 3
    ##    STATE mid_lat mid_lon
    ##    <chr>   <dbl>   <dbl>
    ##  1 AL       32.8   -86.7
    ##  2 AR       35.2   -92.7
    ##  3 AZ       33.9  -112. 
    ##  4 CA       36.5  -120. 
    ##  5 CO       39.1  -106. 
    ##  6 CT       41.5   -72.7
    ##  7 DE       39.2   -75.5
    ##  8 FL       28.3   -82.4
    ##  9 GA       32.6   -83.3
    ## 10 IA       41.9   -93.5
    ## # ℹ 38 more rows

``` r
met_with_midpoints <- merge(met_merged, midpoint_stations, by = "STATE")

met_with_midpoints <- met_with_midpoints %>%
  mutate(
    dist_to_mid = sqrt((lat - mid_lat)^2 + (lon - mid_lon)^2)  # Use lowercase 'lat' and 'lon'
  )

print(head(met_with_midpoints))
```

    ## Key: <STATE>
    ##     STATE USAFID  WBAN  year month   day  hour   min    lat     lon  elev
    ##    <char>  <int> <int> <int> <int> <int> <int> <int>  <num>   <num> <int>
    ## 1:     AL 720265 63833  2019     8     1     0    15 32.915 -85.963   209
    ## 2:     AL 720265 63833  2019     8     1     0    35 32.915 -85.963   209
    ## 3:     AL 720265 63833  2019     8     1     0    55 32.915 -85.963   209
    ## 4:     AL 720265 63833  2019     8     1     1    15 32.915 -85.963   209
    ## 5:     AL 720265 63833  2019     8     1     1    35 32.915 -85.963   209
    ## 6:     AL 720265 63833  2019     8     1     1    55 32.915 -85.963   209
    ##    wind.dir wind.dir.qc wind.type.code wind.sp wind.sp.qc ceiling.ht
    ##       <int>      <char>         <char>   <num>     <char>      <int>
    ## 1:       NA           9              C     0.0          5      22000
    ## 2:       NA           9              C     0.0          5      22000
    ## 3:       NA           9              C     0.0          5      22000
    ## 4:       NA           9              C     0.0          5      22000
    ## 5:       NA           9              C     0.0          5      22000
    ## 6:      210           5              N     2.1          5      22000
    ##    ceiling.ht.qc ceiling.ht.method sky.cond vis.dist vis.dist.qc vis.var
    ##            <int>            <char>   <char>    <int>      <char>  <char>
    ## 1:             5                 9        N    16093           5       N
    ## 2:             5                 9        N    16093           5       N
    ## 3:             5                 9        N    16093           5       N
    ## 4:             5                 9        N    16093           5       N
    ## 5:             5                 9        N    16093           5       N
    ## 6:             5                 9        N    16093           5       N
    ##    vis.var.qc  temp temp.qc dew.point dew.point.qc atm.press atm.press.qc
    ##        <char> <num>  <char>     <num>       <char>     <num>        <int>
    ## 1:          5  29.2       5      22.0            5        NA            9
    ## 2:          5  27.7       5      22.3            5        NA            9
    ## 3:          5  27.1       5      23.0            5        NA            9
    ## 4:          5  26.2       5      22.8            5        NA            9
    ## 5:          5  26.0       5      23.0            5        NA            9
    ## 6:          5  24.6       5      22.7            5        NA            9
    ##          rh   CTRY  mid_lat   mid_lon dist_to_mid
    ##       <num> <char>    <num>     <num>       <num>
    ## 1: 65.19533     US 32.75554 -86.65318   0.7083627
    ## 2: 72.47858     US 32.75554 -86.65318   0.7083627
    ## 3: 78.33501     US 32.75554 -86.65318   0.7083627
    ## 4: 81.61330     US 32.75554 -86.65318   0.7083627
    ## 5: 83.59005     US 32.75554 -86.65318   0.7083627
    ## 6: 89.21468     US 32.75554 -86.65318   0.7083627

``` r
closest_midpoint_station <- met_with_midpoints %>%
  group_by(STATE) %>%
  filter(dist_to_mid == min(dist_to_mid)) %>%
  slice(1)

print(closest_midpoint_station)
```

    ## # A tibble: 48 × 35
    ## # Groups:   STATE [48]
    ##    STATE USAFID  WBAN  year month   day  hour   min   lat    lon  elev wind.dir
    ##    <chr>  <int> <int> <int> <int> <int> <int> <int> <dbl>  <dbl> <int>    <int>
    ##  1 AL    722300 53864  2019     8    13     0    53  33.2  -86.8   179      220
    ##  2 AR    723429 53920  2019     8    13     0    53  35.3  -93.1   123       NA
    ##  3 AZ    723745   374  2019     8     1     0    15  34.3 -111.   1572       NA
    ##  4 CA    723890 93193  2019     8     1     0     0  36.8 -120.    100      300
    ##  5 CO    726396   422  2019     8     1     0    10  39.0 -106.   3438       40
    ##  6 CT    725027 54788  2019     8     1     0    53  41.5  -72.8    32       NA
    ##  7 DE    724088 13707  2019     8     1     0    18  39.1  -75.5     9       70
    ##  8 FL    722014 12818  2019     8     1     0    53  28.5  -82.5    23      150
    ##  9 GA    722175 13860  2019     8     8    16    56  32.6  -83.6    90       NA
    ## 10 IA    725466  4938  2019     8     1     0    15  41.7  -93.6   277       NA
    ## # ℹ 38 more rows
    ## # ℹ 23 more variables: wind.dir.qc <chr>, wind.type.code <chr>, wind.sp <dbl>,
    ## #   wind.sp.qc <chr>, ceiling.ht <int>, ceiling.ht.qc <int>,
    ## #   ceiling.ht.method <chr>, sky.cond <chr>, vis.dist <int>, vis.dist.qc <chr>,
    ## #   vis.var <chr>, vis.var.qc <chr>, temp <dbl>, temp.qc <chr>,
    ## #   dew.point <dbl>, dew.point.qc <chr>, atm.press <dbl>, atm.press.qc <int>,
    ## #   rh <dbl>, CTRY <chr>, mid_lat <dbl>, mid_lon <dbl>, dist_to_mid <dbl>

``` r
combined_stations <- bind_rows(closest_midpoint_station, representative_stations)

print(combined_stations)
```

    ## # A tibble: 72 × 39
    ## # Groups:   STATE [48]
    ##    STATE USAFID  WBAN  year month   day  hour   min   lat    lon  elev wind.dir
    ##    <chr>  <int> <int> <int> <int> <int> <int> <int> <dbl>  <dbl> <int>    <int>
    ##  1 AL    722300 53864  2019     8    13     0    53  33.2  -86.8   179      220
    ##  2 AR    723429 53920  2019     8    13     0    53  35.3  -93.1   123       NA
    ##  3 AZ    723745   374  2019     8     1     0    15  34.3 -111.   1572       NA
    ##  4 CA    723890 93193  2019     8     1     0     0  36.8 -120.    100      300
    ##  5 CO    726396   422  2019     8     1     0    10  39.0 -106.   3438       40
    ##  6 CT    725027 54788  2019     8     1     0    53  41.5  -72.8    32       NA
    ##  7 DE    724088 13707  2019     8     1     0    18  39.1  -75.5     9       70
    ##  8 FL    722014 12818  2019     8     1     0    53  28.5  -82.5    23      150
    ##  9 GA    722175 13860  2019     8     8    16    56  32.6  -83.6    90       NA
    ## 10 IA    725466  4938  2019     8     1     0    15  41.7  -93.6   277       NA
    ## # ℹ 62 more rows
    ## # ℹ 27 more variables: wind.dir.qc <chr>, wind.type.code <chr>, wind.sp <dbl>,
    ## #   wind.sp.qc <chr>, ceiling.ht <int>, ceiling.ht.qc <int>,
    ## #   ceiling.ht.method <chr>, sky.cond <chr>, vis.dist <int>, vis.dist.qc <chr>,
    ## #   vis.var <chr>, vis.var.qc <chr>, temp <dbl>, temp.qc <chr>,
    ## #   dew.point <dbl>, dew.point.qc <chr>, atm.press <dbl>, atm.press.qc <int>,
    ## #   rh <dbl>, CTRY <chr>, mid_lat <dbl>, mid_lon <dbl>, dist_to_mid <dbl>, …

``` r
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = combined_stations,
                   lng = ~lon, lat = ~lat)
```

**Question 4**

``` r
state_avg_temp <- met_merged %>%
  group_by(STATE) %>%
  summarize(avg_temp = mean(temp, na.rm = TRUE))

state_avg_temp <- state_avg_temp %>%
  mutate(
    temp_level = ifelse(avg_temp < 20, "Low", 
                  ifelse(avg_temp >= 20 & avg_temp < 25, "Mid", "High"))
  )

met_with_levels <- merge(met_merged, state_avg_temp, by = "STATE")

print(head(met_with_levels))
```

    ## Key: <STATE>
    ##     STATE USAFID  WBAN  year month   day  hour   min    lat     lon  elev
    ##    <char>  <int> <int> <int> <int> <int> <int> <int>  <num>   <num> <int>
    ## 1:     AL 720265 63833  2019     8     1     0    15 32.915 -85.963   209
    ## 2:     AL 720265 63833  2019     8     1     0    35 32.915 -85.963   209
    ## 3:     AL 720265 63833  2019     8     1     0    55 32.915 -85.963   209
    ## 4:     AL 720265 63833  2019     8     1     1    15 32.915 -85.963   209
    ## 5:     AL 720265 63833  2019     8     1     1    35 32.915 -85.963   209
    ## 6:     AL 720265 63833  2019     8     1     1    55 32.915 -85.963   209
    ##    wind.dir wind.dir.qc wind.type.code wind.sp wind.sp.qc ceiling.ht
    ##       <int>      <char>         <char>   <num>     <char>      <int>
    ## 1:       NA           9              C     0.0          5      22000
    ## 2:       NA           9              C     0.0          5      22000
    ## 3:       NA           9              C     0.0          5      22000
    ## 4:       NA           9              C     0.0          5      22000
    ## 5:       NA           9              C     0.0          5      22000
    ## 6:      210           5              N     2.1          5      22000
    ##    ceiling.ht.qc ceiling.ht.method sky.cond vis.dist vis.dist.qc vis.var
    ##            <int>            <char>   <char>    <int>      <char>  <char>
    ## 1:             5                 9        N    16093           5       N
    ## 2:             5                 9        N    16093           5       N
    ## 3:             5                 9        N    16093           5       N
    ## 4:             5                 9        N    16093           5       N
    ## 5:             5                 9        N    16093           5       N
    ## 6:             5                 9        N    16093           5       N
    ##    vis.var.qc  temp temp.qc dew.point dew.point.qc atm.press atm.press.qc
    ##        <char> <num>  <char>     <num>       <char>     <num>        <int>
    ## 1:          5  29.2       5      22.0            5        NA            9
    ## 2:          5  27.7       5      22.3            5        NA            9
    ## 3:          5  27.1       5      23.0            5        NA            9
    ## 4:          5  26.2       5      22.8            5        NA            9
    ## 5:          5  26.0       5      23.0            5        NA            9
    ## 6:          5  24.6       5      22.7            5        NA            9
    ##          rh   CTRY avg_temp temp_level
    ##       <num> <char>    <num>     <char>
    ## 1: 65.19533     US 26.19799       High
    ## 2: 72.47858     US 26.19799       High
    ## 3: 78.33501     US 26.19799       High
    ## 4: 81.61330     US 26.19799       High
    ## 5: 83.59005     US 26.19799       High
    ## 6: 89.21468     US 26.19799       High

``` r
summary_table <- met_with_levels %>%
  group_by(temp_level) %>%
  summarize(
    num_records = n(),
    num_na_temp = sum(is.na(temp)),
    num_na_wind = sum(is.na(wind.sp)),
    num_na_pressure = sum(is.na(atm.press)),
    num_stations = n_distinct(USAFID),
    num_states = n_distinct(STATE),
    mean_temp = mean(temp, na.rm = TRUE),
    mean_wind = mean(wind.sp, na.rm = TRUE),
    mean_pressure = mean(atm.press, na.rm = TRUE)
  )

print(summary_table)
```

    ## # A tibble: 3 × 10
    ##   temp_level num_records num_na_temp num_na_wind num_na_pressure num_stations
    ##   <chr>            <int>       <int>       <int>           <int>        <int>
    ## 1 High            811126       23468       29837          572668          555
    ## 2 Low             430794        7369       16048          337209          259
    ## 3 Mid            1135423       29252       33808          756397          781
    ## # ℹ 4 more variables: num_states <int>, mean_temp <dbl>, mean_wind <dbl>,
    ## #   mean_pressure <dbl>
=======
