Raport3
================
Krzysztof Wołowiec
3 05 2020

``` r
library(sf)
library(magrittr)
library(ggplot2)
library(spData)
```

# Zadanie 1

Utworzyć punkt na terenie Polski i wyświetlenie go na mapie świata.

Przykładowa wizualizacja na podstawie zbioru:

``` r
plot(world['continent'])
```

![](raport3_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Układ współrzędnych zbioru `world`:

``` r
st_crs(world)
```

    ## Coordinate Reference System:
    ##   User input: EPSG:4326 
    ##   wkt:
    ## GEOGCS["WGS 84",
    ##     DATUM["WGS_1984",
    ##         SPHEROID["WGS 84",6378137,298.257223563,
    ##             AUTHORITY["EPSG","7030"]],
    ##         AUTHORITY["EPSG","6326"]],
    ##     PRIMEM["Greenwich",0,
    ##         AUTHORITY["EPSG","8901"]],
    ##     UNIT["degree",0.0174532925199433,
    ##         AUTHORITY["EPSG","9122"]],
    ##     AUTHORITY["EPSG","4326"]]

**Finalne rozwiązanie: dodanie punktu do mapy:**

``` r
point_poland <- st_point(c(17, 52)) %>%
  st_sfc(crs = st_crs(world)) %>%
  st_sf()

ggplot() +
  geom_sf(data = world) +
  geom_sf(data = point_poland, size=4, col='red')
```

![](raport3_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

# Zadanie 2 - Zadania o Polsce

``` r
# Zadanie 2.1
poland <- world[world$name_long == 'Poland',]
bbox_pl <- st_bbox(poland)

square_pl <-  list(rbind(
                   c(bbox_pl$xmin, bbox_pl$ymin),
                   c(bbox_pl$xmax, bbox_pl$ymin),
                   c(bbox_pl$xmax, bbox_pl$ymax),
                   c(bbox_pl$xmin, bbox_pl$ymax),
                   c(bbox_pl$xmin, bbox_pl$ymin)
               )) %>%
  st_polygon() %>%
  st_sfc(crs = st_crs(poland))


# Zadanie 2.2
vistula <- rbind(c(18.9, 54.4), # Ujście rzeki
                 c(18.8, 53.5), # Grudziądz
                 c(18.5, 53), # Toruń
                 c(19.8, 52.5), # Płock
                 c(21, 52.1), # Warszawa
                 c(21.9, 51.4), # Puławy
                 c(21.7, 50.6), # Sandomierz
                 c(21, 50.2), # Szczucin
                 c(20, 50), # Kraków
                 c(19, 49.5)) %>% # Źródło rzeki
  st_linestring() %>% 
  st_sfc(crs = st_crs(poland))


# Zadanie 2.3
ggplot() +
  geom_sf(data = poland, fill = 'darkgreen', alpha = 0.5) +
  geom_sf(data = square_pl, alpha = 0.2, fill = 'red') +
  geom_sf(data = vistula, size = 2, color = 'blue') +
  theme_light()
```

![](raport3_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Zadania 2.4 i 2.5

Rzeczywiste dane nieco się różnią od poniższych z uwagi na poziom
generalizacji. Przyjmuję jednak, że chodziło o statystyki na podstawie
poligonów.

``` r
c("Powierzchnia Polski [km2]" = poland$area_km2, 
  "Granica Polski [km]" = st_length(poland) / 1000)
```

    ## Powierzchnia Polski [km2]       Granica Polski [km] 
    ##                310402.333                  2384.913
