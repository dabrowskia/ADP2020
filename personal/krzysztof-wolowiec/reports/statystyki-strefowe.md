Statystyki Strefowe
================
Krzysztof Wołowiec
16 05 2020

``` r
library(raster)
library(sf)
library(reshape2)
library(cowplot)
library(dplyr)
library(ggplot2)
library(tmap)
```

``` r
DEM.Poland <- getData('alt', country='PL', mask=TRUE)
```

``` r
plot(DEM.Poland)
```

![](statystyki-strefowe_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Podstawowe statystyki dla obszaru całej Polski

``` r
summary(DEM.Poland)
```

    ##         POL_msk_alt
    ## Min.             -5
    ## 1st Qu.         100
    ## Median          142
    ## 3rd Qu.         197
    ## Max.           2023
    ## NA's              0

## Statystyki dla województw

``` r
woj <- read_sf('../data/woj/Wojewodztwa.shp')
```

Zonal przyjmuje jako drugi argument **raster**, natomiast województwa
mamy w formie **geometrii** (*shapefile*). To nie zadziała:

``` r
#zonal(x=DEM.Poland, z=woj, fun='mean', na.rm=T)
```

Skorzystałem z funkcji `extract`:

``` r
woj.el.mean <- extract(DEM.Poland, woj, fun = 'mean', na.rm=T, df=T, weights=T)
woj.el.max <- extract(DEM.Poland, woj, fun = max, na.rm=T, df=T)
woj.el.min <- extract(DEM.Poland, woj, fun = min, na.rm=T, df=T)
```

``` r
woj.el.stats <- data.frame(woj.el.mean, woj.el.max, woj.el.min) %>%
  rename(el.min = POL_msk_alt.2,
         el.max = POL_msk_alt.1,
         el.mean = POL_msk_alt) %>%
  select(-c(ID.1, ID.2))

woj <- woj %>% mutate(ID = row_number())

woj.el.stats.geom <- left_join(x = woj, 
                 y = woj.el.stats, 
                 by = 'ID')

woj.el.stats.geom %>% select(JPT_NAZWA_, el.mean, el.max, el.min)
```

    ## Simple feature collection with 16 features and 4 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: 14.12288 ymin: 49.00205 xmax: 24.14578 ymax: 54.83642
    ## proj4string:    +proj=longlat +ellps=GRS80 +no_defs 
    ## # A tibble: 16 x 5
    ##    JPT_NAZWA_    el.mean el.max el.min                                  geometry
    ##    <chr>           <dbl>  <dbl>  <dbl>               <MULTIPOLYGON [arc_degree]>
    ##  1 śląskie         317.    1280    174 (((18.91685 51.09608, 18.91677 51.0961, ~
    ##  2 opolskie        204.     733    128 (((17.81203 51.18669, 17.81209 51.1867, ~
    ##  3 wielkopolskie   101.     265     25 (((16.71447 53.29913, 16.71447 53.2992, ~
    ##  4 świętokrzysk~   243.     569    125 (((19.7043 50.75239, 19.70443 50.75248, ~
    ##  5 pomorskie       103.     308    -11 (((17.66483 54.78297, 17.66653 54.78334,~
    ##  6 kujawsko-pom~    91.5    175     10 (((17.6567 53.57115, 17.65672 53.57115, ~
    ##  7 podlaskie       145.     283     93 (((21.98108 52.97859, 21.98078 52.97861,~
    ##  8 zachodniopom~    71.6    235     -6 (((14.81157 54.04037, 14.89525 54.0557, ~
    ##  9 dolnośląskie    246.    1421     65 (((15.10534 51.4273, 15.1054 51.42732, 1~
    ## 10 podkarpackie    307.    1251    101 (((22.03538 50.80675, 22.0355 50.80681, ~
    ## 11 małopolskie     420.    2079    157 (((19.9723 50.51625, 19.97227 50.51633, ~
    ## 12 warmińsko-ma~   126.     298     -5 (((21.55932 54.3225, 21.55939 54.3225, 2~
    ## 13 łódzkie         169.     373     23 (((19.99385 51.18395, 19.9941 51.18395, ~
    ## 14 mazowieckie     127.     389     50 (((21.98576 52.97722, 21.98587 52.97708,~
    ## 15 lubelskie       193.     367    104 (((21.61554 51.61756, 21.62194 51.62192,~
    ## 16 lubuskie         81.8    219      8 (((15.10534 51.4273, 15.10521 51.42727, ~

``` r
basemap <- tm_shape(woj.el.stats.geom) + tm_borders()

map.min <- basemap + tm_fill(col = 'el.min', palette = '-BrBG')
map.mean <- basemap + tm_fill(col = 'el.mean',  palette = 'YlOrBr')
map.max <- basemap + tm_fill(col = 'el.max', palette = 'YlOrBr')

tmap_arrange(map.min, map.max, map.mean)
```

![](statystyki-strefowe_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggplot(woj.el.stats.geom, aes(x=reorder(JPT_NAZWA_, el.mean, FUN = median))) +
  geom_errorbar(aes(ymin=el.min, ymax=el.max)) +
  geom_point(aes(y=el.mean)) +
  labs(title='Podstawowe statystyki strefowe dla województw',
       subtitle='Rozstęp wysokości i średnia na podstawie Numerycznego Modelu Terenu',
       x='', y='Wysokość') +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, face = 'bold', size=12, vjust=0)) +
  coord_flip()
```

![](statystyki-strefowe_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

W dolnośląskim średnia jest niższa niż w śląskim i podkarpackim,
znacznie bliżej mu pod tym względem do świętokrzystkiego, jednak max
jest większy od dwóch wspomnianych województw. Rejon śnieżki jest
znacznie wyżej położony niż Beskid Śląski, czy Bieszczady, jednak
dolnośląskie obejmuje również nieco większe obszary terenów niżej
położonych.

``` r
woj.el.stats.geom
```

    ## Simple feature collection with 16 features and 33 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: 14.12288 ymin: 49.00205 xmax: 24.14578 ymax: 54.83642
    ## proj4string:    +proj=longlat +ellps=GRS80 +no_defs 
    ## # A tibble: 16 x 34
    ##    JPT_SJR_KO JPT_KOD_JE JPT_NAZWA_ JPT_ORGAN_ JPT_JOR_ID WERSJA_OD  WERSJA_DO 
    ##    <chr>      <chr>      <chr>      <chr>           <int> <date>     <date>    
    ##  1 WOJ        24         śląskie    <NA>                0 2017-10-10 NA        
    ##  2 WOJ        16         opolskie   <NA>                0 2017-10-10 NA        
    ##  3 WOJ        30         wielkopol~ <NA>                0 2016-05-05 NA        
    ##  4 WOJ        26         świętokrz~ <NA>                0 2012-09-26 NA        
    ##  5 WOJ        22         pomorskie  <NA>                0 2020-04-09 NA        
    ##  6 WOJ        04         kujawsko-~ <NA>                0 2012-09-26 NA        
    ##  7 WOJ        20         podlaskie  <NA>                0 2019-03-21 NA        
    ##  8 WOJ        32         zachodnio~ <NA>                0 2020-04-24 NA        
    ##  9 WOJ        02         dolnośląs~ <NA>                0 2018-09-25 NA        
    ## 10 WOJ        18         podkarpac~ <NA>                0 2017-10-10 NA        
    ## 11 WOJ        12         małopolsk~ <NA>                0 2017-10-10 NA        
    ## 12 WOJ        28         warmińsko~ <NA>                0 2019-08-13 NA        
    ## 13 WOJ        10         łódzkie    <NA>                0 2012-09-26 NA        
    ## 14 WOJ        14         mazowieck~ <NA>                0 2019-03-21 NA        
    ## 15 WOJ        06         lubelskie  <NA>                0 2012-09-26 NA        
    ## 16 WOJ        08         lubuskie   <NA>                0 2018-09-25 NA        
    ## # ... with 27 more variables: WAZNY_OD <date>, WAZNY_DO <date>,
    ## #   JPT_KOD__1 <chr>, JPT_NAZWA1 <chr>, JPT_ORGAN1 <chr>, JPT_WAZNA_ <chr>,
    ## #   ID_BUFORA_ <dbl>, ID_BUFORA1 <dbl>, ID_TECHNIC <int>, IIP_PRZEST <chr>,
    ## #   IIP_IDENTY <chr>, IIP_WERSJA <chr>, JPT_KJ_IIP <chr>, JPT_KJ_I_1 <chr>,
    ## #   JPT_KJ_I_2 <chr>, JPT_OPIS <chr>, JPT_SPS_KO <chr>, ID_BUFOR_1 <int>,
    ## #   JPT_ID <int>, JPT_KJ_I_3 <chr>, Shape_Leng <dbl>, Shape_Area <dbl>,
    ## #   geometry <MULTIPOLYGON [arc_degree]>, ID <int>, el.mean <dbl>,
    ## #   el.max <dbl>, el.min <dbl>

## Zastosowanie funckji `focal` do analizy sąsiedztwa

``` r
binaryRaster <- raster(x = matrix(sample(c(0,1), 10000, replace = T), ncol = 100, nrow = 100))
plot(focal(binaryRaster, w = matrix(1, nrow = 3, ncol = 3), fun = 'sum'))
```

![](statystyki-strefowe_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Im bardziej zielony piksel, tym więcej jedynek ma w sąsiedztwie - jakby
to porównać do gry w Sapera - wokół takich punktów jest więcej min :)
