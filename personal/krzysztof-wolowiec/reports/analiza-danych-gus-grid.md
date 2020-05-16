Analiza danych GUS - grid
================
Krzysztof Wołowiec
3 05 2020

``` r
library(tidyverse)
library(sf)
library(xlsx)
```

# 1\. IMPORT DANYCH

### 1.1 Wczytanie danych liczbowych z pliku `.xls`

``` r
df <- read.xlsx('../data/gus-data/dane_siatka_miasta_zalacznik_nr_3.xls', sheetIndex = 1)
head(df)
```

    ##        New_ID ID_GRID KOD_MIASTA U_L_00_14 U_L_15_64 U_L_65 U_Feminiz
    ## 1 76079PL003C   76079     PL003C     37.50     62.50   0.00        78
    ## 2 76080PL003C   76080     PL003C      9.09     54.55  36.36       120
    ## 3 76081PL003C   76081     PL003C     17.99     74.82   7.19        99
    ## 4 76881PL003C   76881     PL003C     22.47     73.03   4.49       112
    ## 5 76882PL003C   76882     PL003C     18.57     68.57  12.86       130
    ## 6 76883PL003C   76883     PL003C     24.14     72.41   3.45       107
    ##   Mediana_Wi Indeks_Chu W_Wspar_85 W_ObcDemD W_ObcDemS Indeks_St  Moran_Med
    ## 1       32.5     0.0000       0.00     60.00      0.00      0.00 nieistotny
    ## 2       47.0     0.2182         NA     16.67     66.67    400.00         HL
    ## 3       34.0     0.0216       0.00     24.04      9.62     40.00 nieistotny
    ## 4       31.0     0.0157       0.00     30.77      6.15     20.00 nieistotny
    ## 5       34.5     0.0479      10.71     27.08     18.75     69.23 nieistotny
    ## 6       32.0     0.0241       0.00     33.33      4.76     14.29 nieistotny
    ##    Moran_Chu  Moran_Wsp  Moran_Dem  Moran_Sta Moran_Syn U_Prac_O U_Em_Re_O
    ## 1         LH nieistotny         LH nieistotny         0    43.75      3.13
    ## 2         HL nieistotny         HL         HL         0    54.55     36.36
    ## 3 nieistotny nieistotny nieistotny nieistotny         0    41.73     14.39
    ## 4 nieistotny nieistotny nieistotny nieistotny         0    48.31      6.74
    ## 5 nieistotny nieistotny nieistotny nieistotny         0    45.71     18.57
    ## 6         LL nieistotny         LL nieistotny         0    34.48     13.79
    ##   U_PracPopr                        Lq U_Em_Rent U_Bezrobot U_Socj_Spo
    ## 1         NA                pozostaĹ‚e     15.00       0.00       0.00
    ## 2          0           emerytura/renta     50.00       0.00       0.00
    ## 3          0                pozostaĹ‚e     16.85       1.12       1.44
    ## 4          0             praca najemna      6.56       0.00       1.12
    ## 5         16 praca na rachunek wĹ‚asny     25.88       0.00       0.71
    ## 6         50             praca najemna     21.05       0.00       3.45

``` r
str(df)
```

    ## 'data.frame':    8626 obs. of  26 variables:
    ##  $ New_ID    : Factor w/ 8626 levels "100067PL010C",..: 6714 6715 6716 6717 6718 6719 6720 6721 6722 6723 ...
    ##  $ ID_GRID   : num  76079 76080 76081 76881 76882 ...
    ##  $ KOD_MIASTA: Factor w/ 18 levels "PL001C","PL002C",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ U_L_00_14 : num  37.5 9.09 17.99 22.47 18.57 ...
    ##  $ U_L_15_64 : num  62.5 54.5 74.8 73 68.6 ...
    ##  $ U_L_65    : num  0 36.36 7.19 4.49 12.86 ...
    ##  $ U_Feminiz : num  78 120 99 112 130 107 105 124 72 101 ...
    ##  $ Mediana_Wi: num  32.5 47 34 31 34.5 32 34 35 28 36 ...
    ##  $ Indeks_Chu: num  0 0.2182 0.0216 0.0157 0.0479 ...
    ##  $ W_Wspar_85: num  0 NA 0 0 10.7 ...
    ##  $ W_ObcDemD : num  60 16.7 24 30.8 27.1 ...
    ##  $ W_ObcDemS : num  0 66.67 9.62 6.15 18.75 ...
    ##  $ Indeks_St : num  0 400 40 20 69.2 ...
    ##  $ Moran_Med : Factor w/ 5 levels "HH","HL","LH",..: 5 2 5 5 5 5 5 5 4 5 ...
    ##  $ Moran_Chu : Factor w/ 5 levels "HH","HL","LH",..: 3 2 5 5 5 4 5 5 5 5 ...
    ##  $ Moran_Wsp : Factor w/ 5 levels "HH","HL","LH",..: 5 5 5 5 5 5 5 5 5 5 ...
    ##  $ Moran_Dem : Factor w/ 5 levels "HH","HL","LH",..: 3 2 5 5 5 4 5 5 5 5 ...
    ##  $ Moran_Sta : Factor w/ 5 levels "HH","HL","LH",..: 5 2 5 5 5 5 5 5 5 5 ...
    ##  $ Moran_Syn : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ U_Prac_O  : num  43.8 54.5 41.7 48.3 45.7 ...
    ##  $ U_Em_Re_O : num  3.13 36.36 14.39 6.74 18.57 ...
    ##  $ U_PracPopr: num  NA 0 0 0 16 ...
    ##  $ Lq        : Factor w/ 4 levels "emerytura/renta",..: 2 1 2 4 3 4 3 2 2 4 ...
    ##  $ U_Em_Rent : num  15 50 16.85 6.56 25.88 ...
    ##  $ U_Bezrobot: num  0 0 1.12 0 0 0 0 0.56 0.82 0 ...
    ##  $ U_Socj_Spo: num  0 0 1.44 1.12 0.71 3.45 1.68 2.2 3.11 1.27 ...

Dane obejmują wskaźniki społeczno-gospodarcze dla segmentów siatki
wybranych miast Polski. Zmienne można podzielić na 3 grupy:

  - wskaźniki demograficzne, m.in.: udział różnych grup wiekowych w
    populacji, współczynnik feminizacji, mediana wieku, w. wsparcia osób
    najstarszycg, indeks starości),

  - wskaźniki związane z zatrudnieniem: udział pracujących, m.in.:
    udział emerytór i rencistów, u. pracujących w wieku poprodukcyjnym,
    u. bezrobotnych

  - kategorie na podstawie statystyk korelacji przestrzennej Morana.

<!-- end list -->

``` r
summary(df)
```

    ##           New_ID        ID_GRID          KOD_MIASTA     U_L_00_14    
    ##  100067PL010C:   1   Min.   :  76079   PL001C :1504   Min.   : 0.00  
    ##  100068PL010C:   1   1st Qu.: 296114   PL003C : 937   1st Qu.:11.06  
    ##  100069PL010C:   1   Median : 647475   PL002C : 879   Median :13.53  
    ##  100070PL010C:   1   Mean   : 573698   PL004C : 639   Mean   :14.75  
    ##  100071PL010C:   1   3rd Qu.: 731448   PL005C : 634   3rd Qu.:17.46  
    ##  100072PL010C:   1   Max.   :1222118   PL006C : 579   Max.   :58.33  
    ##  (Other)     :8620                     (Other):3454                  
    ##    U_L_15_64          U_L_65        U_Feminiz       Mediana_Wi   
    ##  Min.   : 40.00   Min.   : 0.00   Min.   : 25.0   Min.   :10.00  
    ##  1st Qu.: 68.42   1st Qu.: 8.16   1st Qu.:100.0   1st Qu.:35.00  
    ##  Median : 72.25   Median :12.50   Median :110.0   Median :39.00  
    ##  Mean   : 72.14   Mean   :13.11   Mean   :110.8   Mean   :38.95  
    ##  3rd Qu.: 75.86   3rd Qu.:17.24   3rd Qu.:120.0   3rd Qu.:43.00  
    ##  Max.   :100.00   Max.   :50.00   Max.   :450.0   Max.   :63.00  
    ##                                                                  
    ##    Indeks_Chu        W_Wspar_85        W_ObcDemD        W_ObcDemS     
    ##  Min.   :0.00000   Min.   :  0.000   Min.   :  0.00   Min.   :  0.00  
    ##  1st Qu.:0.03130   1st Qu.:  1.220   1st Qu.: 15.38   1st Qu.: 10.81  
    ##  Median :0.05180   Median :  5.260   Median : 18.56   Median : 17.09  
    ##  Mean   :0.05653   Mean   :  7.011   Mean   : 20.89   Mean   : 18.89  
    ##  3rd Qu.:0.07587   3rd Qu.:  9.775   3rd Qu.: 24.44   3rd Qu.: 24.80  
    ##  Max.   :0.29170   Max.   :200.000   Max.   :140.00   Max.   :114.29  
    ##                    NA's   :51                                         
    ##    Indeks_St            Moran_Med         Moran_Chu         Moran_Wsp   
    ##  Min.   :   0.00   HH        : 729   HH        : 935   HH        : 359  
    ##  1st Qu.:  50.00   HL        : 106   HL        :  64   HL        :  84  
    ##  Median :  90.03   LH        :  91   LH        :  76   LH        :  71  
    ##  Mean   : 110.74   LL        : 491   LL        : 528   LL        :  15  
    ##  3rd Qu.: 144.90   nieistotny:7209   nieistotny:7023   nieistotny:8097  
    ##  Max.   :1400.00                                                        
    ##  NA's   :97                                                             
    ##       Moran_Dem         Moran_Sta      Moran_Syn       U_Prac_O    
    ##  HH        : 861   HH        : 739   Min.   :0.00   Min.   : 0.00  
    ##  HL        :  65   HL        :  62   1st Qu.:0.00   1st Qu.:40.74  
    ##  LH        :  77   LH        :  65   Median :0.00   Median :45.22  
    ##  LL        : 434   LL        : 232   Mean   :0.42   Mean   :45.14  
    ##  nieistotny:7189   nieistotny:7528   3rd Qu.:0.00   3rd Qu.:49.93  
    ##                                      Max.   :5.00   Max.   :92.86  
    ##                                                                    
    ##    U_Em_Re_O       U_PracPopr                             Lq      
    ##  Min.   : 0.00   Min.   :  0.00   emerytura/renta          :1772  
    ##  1st Qu.:12.62   1st Qu.:  7.14   pozostaĹ‚e               :2709  
    ##  Median :17.55   Median : 13.67   praca na rachunek wĹ‚asny:2426  
    ##  Mean   :17.52   Mean   : 14.78   praca najemna            :1719  
    ##  3rd Qu.:22.23   3rd Qu.: 20.00                                   
    ##  Max.   :60.00   Max.   :100.00                                   
    ##                  NA's   :143                                      
    ##    U_Em_Rent        U_Bezrobot        U_Socj_Spo    
    ##  Min.   :  0.00   Min.   : 0.0000   Min.   : 0.000  
    ##  1st Qu.: 16.03   1st Qu.: 0.0000   1st Qu.: 0.540  
    ##  Median : 23.86   Median : 0.5100   Median : 1.390  
    ##  Mean   : 25.02   Mean   : 0.7355   Mean   : 2.077  
    ##  3rd Qu.: 32.11   3rd Qu.: 1.0100   3rd Qu.: 2.640  
    ##  Max.   :133.33   Max.   :25.0000   Max.   :50.000  
    ## 

### 1.2. Wczytanie warstwy wektorowej z pliku `shp`

``` r
grid <- read_sf('../data/gus-data/siatka_miasta załącznik nr 1.shp')
str(grid)
```

    ## tibble [15,965 x 4] (S3: sf/tbl_df/tbl/data.frame)
    ##  $ New_ID    : chr [1:15965] "660450PL001C" "660451PL001C" "660452PL001C" "660453PL001C" ...
    ##  $ ID_GRID500: num [1:15965] 660450 660451 660452 660453 660454 ...
    ##  $ Nr_LUZu   : chr [1:15965] "PL001C" "PL001C" "PL001C" "PL001C" ...
    ##  $ geometry  :sfc_POLYGON of length 15965; first list element: List of 1
    ##   ..$ : num [1:5, 1:2] 5070000 5070000 5070500 5070500 5070000 ...
    ##   ..- attr(*, "class")= chr [1:3] "XY" "POLYGON" "sfg"
    ##  - attr(*, "sf_column")= chr "geometry"
    ##  - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA
    ##   ..- attr(*, "names")= chr [1:3] "New_ID" "ID_GRID500" "Nr_LUZu"

# 2\. INTEGRACJA DANYCH

Identyfikacja kodu dla Poznania, filtrowanie danych.

``` r
code.pz <- 'PL005C'
grid.pz <- grid %>% filter(Nr_LUZu == code.pz)
df.pz <- df %>% filter(KOD_MIASTA == code.pz )
```

Przyłączenie danych numerycznych do siatki kwadratów

``` r
grid.feats.pz <- left_join(x = grid.pz, 
                           y = df.pz, 
                           by = c('ID_GRID500' = 'ID_GRID'))
```

# 3\. WIZUALIZACJA

``` r
ggplot() +
  geom_sf(data=grid.feats.pz, aes(fill=U_L_00_14)) +
  labs(title = "Udział ludności w wieku 0-14 lat w liczbie ludności ogółem",
      caption = 'Źródło: NSP 2011',
      fill = 'W %:') +
  theme_void()
```

![](analiza-danych-gus_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
