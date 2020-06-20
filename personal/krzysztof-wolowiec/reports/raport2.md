Zajęcia2
================
Krzysztof Wołowiec
4 04 2020

## Biblioteki

``` r
library(sf)
library(raster)
library(spData)
library(dplyr)
library(ggplot2)
```

# World dataset tasks

**Wyświetlcie mapę krajów Europejskich wg liczby ludności**

``` r
world %>% 
  filter(continent == 'Europe') %>%
  select(pop) %>%
  plot()
```

![](raport2_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

**Który kraj posiada najmniejszą liczbę ludności i ile wynosi?**

``` r
world %>%
  filter(pop == min(world$pop, na.rm = TRUE)) %>%
  select(name_long, pop)
```

    ## Simple feature collection with 1 feature and 2 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: -73.297 ymin: 60.03676 xmax: -12.20855 ymax: 83.64513
    ## CRS:            EPSG:4326
    ## # A tibble: 1 x 3
    ##   name_long   pop                                                           geom
    ##   <chr>     <dbl>                                    <MULTIPOLYGON [arc_degree]>
    ## 1 Greenland 56295 (((-46.76379 82.62796, -43.40644 83.22516, -39.89753 83.18018~

**Ile krajów znajduje się w Azji?**

``` r
world %>%
  filter(continent=='Asia') %>%
  count()
```

    ## Simple feature collection with 1 feature and 1 field
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: 26.04335 ymin: -10.35999 xmax: 145.5431 ymax: 55.38525
    ## CRS:            EPSG:4326
    ## # A tibble: 1 x 2
    ##       n                                                                     geom
    ##   <int>                                              <MULTIPOLYGON [arc_degree]>
    ## 1    47 (((120.295 -10.25865, 118.9678 -9.557969, 119.9003 -9.36134, 120.4258 -~

**Wyświetlcie histogram powierzchni wszystkich niezależnych krajów
(Sovereign Country)**

``` r
world %>%
  filter(type == "Sovereign country") %>%
  select(area_km2) -> areas

ggplot(areas, aes(x=area_km2)) +
  geom_histogram() +
  scale_x_log10()
```

![](raport2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

**Wyświetlcie wykres punktowy relacji pomiędzy lifeExp, a gdpPercap**

``` r
plot(x=world$lifeExp, y=world$gdpPercap)
```

![](raport2_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# Raster

W przypadku problemów z spDataLarge zainstalować pakiet według
instrukcji w repo:

<https://github.com/Nowosad/spDataLarge>

``` r
# install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
```

``` r
raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
new_raster = raster(raster_filepath)
new_raster
```

    ## class      : RasterLayer 
    ## dimensions : 457, 465, 212505  (nrow, ncol, ncell)
    ## resolution : 0.0008333333, 0.0008333333  (x, y)
    ## extent     : -113.2396, -112.8521, 37.13208, 37.51292  (xmin, xmax, ymin, ymax)
    ## crs        : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
    ## source     : C:/Users/Krzysztof/Documents/R/win-library/3.6/spDataLarge/raster/srtm.tif 
    ## names      : srtm 
    ## values     : 1024, 2892  (min, max)

``` r
# new_raster[] <- shows the matrix
plot(new_raster)
```

![](raport2_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

**Jaka jest maksymalna wysokość w danym obrazie rastrowym?**

``` r
max(new_raster[], na.rm = TRUE)
```

    ## [1] 2892

**Jaki jest rozkład wartości obrazu rastroweg**

``` r
hist(new_raster, maxpixels=250000)
```

![](raport2_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
