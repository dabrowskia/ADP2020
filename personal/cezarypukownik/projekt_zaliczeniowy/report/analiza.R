
#==============================================================================================
#   1. Projekt zaliczeniowy
# •	W parach:
#   –	Wejdźcie na stronę Banku Danych Lokalnych [OK]
# –	Wybierzcie interesujące Was zagadnienie (spis ludności, handel i gastronomia, kultura i sztuka, etc.) [OK]
# –	Pobierzcie dane dla interesującego Was podziału (gminy, powiaty, województwa), dla określnego przez Was przedziału czasowego (może być jeden rok, ale więcej = ciekawsza analiza) [OK]
# –	Wykorzystajcie dane z Państwowego Rejestru Granic do złączenia z danymi tabelarycznymi. [OK]
# –	Wykonajcie eksplorację i wizualizację danych. [OK]
# –	wybierzcie 2 zagadnienia i sprawdźcie, czy istnieje korelacja. [OK]
# 
# 
# II. 
# 1.	Stwórzcie plik markdown “Statystyki strefowe”.
# 2.	Pobieżcie za pomocą getData plik 'alt’ dla Polski.
# 3.	Wczytajcie plik .shp z granicami województw (Państwowy Rejestr Granic). [OK}
# 4.	Obliczcie podstawowe statystyki dla obszaru całej Polski.
# 5.	Obliczcie podstawowe statystyki dla poszczególnych województw w Polsce.
# 6.	Zwizualizujcie uzyskane wyniki.
# 2.	Utwórzcie prostą mapę poligonów oraz centroidów dla danych do projektu z BDL.
# 3.	Aby utworzyć centroidy poligonów użyjcie funkcji st_centroid()
# 4.	Dodajcie również warstwę rastra wysokości nad poziomem morza (cyfrowy model wysokościowy– Digital Elevation Model)
# 5.	Utwórzcie 2 kartogramy (mapy pokolorowane względem zmiennych) dla danych z projektu BDL.
# 6.	Wyświetlcie je obok siebie za pomocą tm_arrange.
#==============================================================================================

#install.packages('remotes')
#
#library(remotes)
#install_github("mtennekes/tmaptools")
#install_github("mtennekes/tmap")

#==============================================================================================
# Biblioteki
#==============================================================================================
library(dplyr)
library(sf)
library(raster)
library(spData)
library(spDataLarge)
library(ggplot2)
library(readxl)
library(tmap)


#==============================================================================================
# Ustaw jako wd katalog z tym plikiem
# Wczytanie danych z różnych źródeł
#==============================================================================================

powiaty <- st_read('../data/mapy/Powiaty.shp') #wczytaj powiaty
wojewodztwa <- st_read('../data/mapy/Województwa.shp') #wczytaj wojewódzwta

powiaty['JPT_KOD_JE'] <-  paste0(powiaty$JPT_KOD_JE, '000') # dodaj 000 na końcu id aby połączyć z danymi w xlsx

powiaty <- powiaty %>% 
  dplyr::select(Kod=JPT_KOD_JE, Nazwa=JPT_NAZWA_)


# weź tylko informacje o kodzie i o nazwie wojewodzwtwa, usuń geometrię
wojewodztwa_df <- wojewodztwa %>% 
  dplyr::select(Kod=JPT_KOD_JE, Nazwa=JPT_NAZWA_)

st_geometry(wojewodztwa_df) <- NULL

wojewodztwa_df

powiaty <- powiaty %>%
  mutate(Kod_woj=Kod) %>%
  mutate_at('Kod_woj', substr, 1,2) %>%
  left_join(wojewodztwa_df, by=c('Kod_woj'='Kod'))%>%
  dplyr::select(Kod, Powiat=Nazwa.x, Wojewodztwo=Nazwa.y)


feminizacja = readxl::read_excel('../data/Feminizacja_Gminy.xlsx', sheet = 'TABLICA')
ludnosc = readxl::read_excel('../data/Ludność.xlsx', sheet = 'TABLICA') 
malzenstwa = readxl::read_excel('../data/Małżeństwa_Gminy.xlsx', sheet = 'TABLICA') # ok
bezrobocie = readxl::read_excel('../data/Bezrobocie_rejestrowne.xlsx', sheet = 'TABLICA') # ok

ludnosc <- ludnosc %>% 
  dplyr::select(Kod, ludnosc=ogółem, ludn.m=...4, ludn.k=...5)


DEM.Poland <- getData('alt', country = 'pol', mask = TRUE)
DEM.Poland

#==============================================================================================
# Połączenie danych do jednego czystego df
#==============================================================================================
df <- powiaty %>%
  left_join(feminizacja, by=c('Kod'='Kod')) %>% 
  left_join(malzenstwa, by=c('Kod'='Kod')) %>% 
  left_join(bezrobocie, by=c('Kod'='Kod')) %>% 
  left_join(ludnosc, by=c('Kod'='Kod')) %>% 
  dplyr::select(Kod, Powiat, Wojewodztwo, feminizacja=`ogółem.x`, `małżeństwa`=`ogółem.y`, bezrobocie.m = `mężczyźni`, bezrobocie.k = kobiety, ludnosc, ludn.m, ludn.k) %>% 
  mutate_at('feminizacja', as.numeric) %>% 
  mutate_at('małżeństwa', as.numeric) %>%
  
  mutate_at('ludnosc', as.numeric) %>% 
  mutate_at('ludn.m', as.numeric) %>% 
  mutate_at('ludn.k', as.numeric) %>% 
  
  mutate_at('bezrobocie.m', as.numeric) %>% 
  mutate_at('bezrobocie.k', as.numeric)

df %>% 
  as_tibble() %>% 
  head() 

#==============================================================================================
# Analiza - czy liczba małżeństw ma pływ na bezrobocie u kobiet?
#==============================================================================================

# aby wykluczyć wpływ wielkości ludności na bezrobociem, obliczmy wspołczynnik małżeństw i bezrobocia, jako 
# stosunek miary do ludności.

df <- df %>%
  mutate(wsp.małż = małżeństwa/ludnosc,
         wsp.bezr.k = bezrobocie.k/ludn.k,
         wsp.bezr.m = bezrobocie.m/ludn.m)


# wsp małżeństw
df %>% 
  ggplot(aes(fill=`wsp.bezr.k`, geometry=geometry)) +
  geom_sf() +
  labs(title = 'Wspołczynnik procentowy bezrobocia kobiet w Polsce') +
  theme_void()


# w ten sposób otrzymamy informacje w jakim poawiecie/województwie jest najwioęszyt odsetek małżenstw/odsetek bezrobotnych

df %>%
  arrange(-wsp.bezr.k) %>%
  head(10) %>%
  ggplot(aes(x=reorder(Powiat, wsp.bezr.k), y=wsp.bezr.k, fill=Wojewodztwo)) +
  geom_col() +
  coord_flip()

df %>%
  arrange(-wsp.bezr.k) %>%
  head(10)

# największy wspołczynnik bezrobocia kobiet, jest w powiecie szydłowieckim w woj. mazowieckim i wynosi ponad 8%

df %>%
  arrange(-wsp.małż) %>%
  head(10) %>%
  ggplot(aes(x=reorder(Powiat, wsp.małż), y=wsp.małż, fill=Wojewodztwo)) +
  geom_col() +
  coord_flip()


# czy liczba małżęństw wpływa na bezrobocie kobiet?

df %>%
  ggplot(aes(x=wsp.małż, y=wsp.bezr.k, color=Wojewodztwo)) +
  geom_point()+
  # geom_smooth(method='lm') +
  scale_x_log10()+
  scale_y_log10()+
  labs(title='Wspołczynnik zawartych małżeństw do wspołczynnika bezrobocia w Polsce',
       subtitle = 'z podziałem na powiaty',
       x='Współczynnik zawartych małżeństw',
       y='Wspołczynnik bezrobocia kobiet')

# z wykresu nie widać korelacji między tymi dwoma zagadnieniami.
# wygląda na to, że liczba małżeństw, nie ma wpływu na bezrobocie kobiet w Polsce.


cor.test(df$wsp.małż, df$wsp.bezr.k)

# z interpretacji wpsołczynnika korelacji wnioskujemy że, że nie występuje isototna korelacja między dwoma
# analizowanymi cechami.

#t_map dla powiaty + punkty centroidy
library(tmap)    # for static and interactive maps

dem <- getData('alt', country = "PL")
powiaty2<- transform(powiaty, 4326)
m1 <- tm_shape(dem) + tm_raster()
m2 <- m1 + tm_shape(powiaty) + tm_borders()
centroidy <- st_centroid(powiaty2)
m3 <- m2 + tm_shape(centroidy) + tm_dots()


tm_shape(dem) + 
  tm_raster()+ #warstwa rastra wysokoĹ›ci nad poziomem morza
  tm_shape(powiaty) + 
  tm_borders() + 
  tm_shape(st_centroid(powiaty2)) + 
  tm_layout(legend.outside=TRUE, 
            legend.outside.position = 'right', 
            legend.title.size = 0.1, 
            frame = FALSE)  +
  tm_legend(title = 'Ukształtowanie terenu')+
  tm_dots()













