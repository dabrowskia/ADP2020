library(spData)
library(sf)
library(dplyr)
library(ggplot2)

dane1 <- st_read(dsn = 'J:/Desktop/ADP2020/ne_10m_populated_places.shp')

## Wczytujemy dane wektorowe dla powiatow
powiaty <- st_read(dsn = 'J:/Desktop/ADP2020/Jednostki_Administracyjne/Powiaty.shp')

## Wczytujemy dane z pliku excel
wynagrodzenia <- readxl::read_xlsx('J:/Desktop/ADP2020/Wynagrodzenia.xlsx', sheet = 2)

## Dodajemy do kodu w powiatach dodatkowe 0, zeby sie dlugosc znakow zgadzala z kodami z wynagrodzen
powiaty$JPT_KOD_JE <- paste0(powiaty$JPT_KOD_JE, '000')

## Polaczenia DO POWIATOW dodajemy dane NIGDY ODWROTNIE !!!
polaczona <- powiaty %>% left_join(wynagrodzenia, 
                            by = c('JPT_KOD_JE'= 'Kod'))

ggplot(polaczona, aes(fill=wynagrodzenia$Wartosc)) +
  geom_sf()

