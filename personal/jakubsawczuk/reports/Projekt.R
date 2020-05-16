library(tidyverse)
library(sf)
library(dplyr)
library(ggplot2)
library(raster)
library(spData)
library(spDataLarge)
library(caret)

powiaty <- st_read(dsn = 'J:/Desktop/ADP2020/Jednostki_Administracyjne/Powiaty.shp')
powiaty
powiaty$JPT_KOD_JE <- paste0(powiaty$JPT_KOD_JE, '000')

wynagrodzenia <- readxl::read_xlsx('J:/Desktop/ADP2020/Wynagrodzenia.xlsx', sheet = 2)
wynagrodzenia %>% mutate(wynagrodzenia_ogolem = Ogolem) -> nowewynagrodzenia
nowewynagrodzenia

malzenstwa <- readxl::read_xlsx('J:/Desktop/ADP2020/Malzenstwa.xlsx', sheet = 2)
malzenstwa %>% mutate(malzenstwa_ogolem = ogółem) -> nowemalzenstwa
nowemalzenstwa

rozwody  <- readxl::read_xlsx('J:/Desktop/ADP2020/Rozwody.xlsx', sheet = 2)
rozwody %>% mutate(rozwody_ogolem = ogółem) -> nowerozwody
nowerozwody

skolaryzacja  <- readxl::read_xlsx('J:/Desktop/ADP2020/Skolaryzacja.xlsx', sheet = 2)
skolaryzacja

tab1 <- powiaty %>% left_join(nowemalzenstwa,by = c('JPT_KOD_JE' = 'Kod'))
tab2 <- tab1 %>% left_join(nowerozwody,by = c('JPT_KOD_JE' = 'Kod'))
tab3 <- tab2 %>% left_join(skolaryzacja, by = c('JPT_KOD_JE' = 'Kod'))
polaczona <- tab3 %>% left_join(nowewynagrodzenia,by = c('JPT_KOD_JE' = 'Kod'))

polaczona

wykres <- ggplot(polaczona, aes(fill = polaczona$`brutto szkoły podstawowe`)) + 
  geom_sf()

######  Modelowanie:

polaczona.df <- st_set_geometry(polaczona,NULL)
summary(polaczona.df)

cvCtrl <- trainControl(method = 'repeatedcv',number = 10, repeats = 2)

## Robimy model

Model <- train(data = polaczona.df,
               malzenstwa_ogolem ~ .,
                     method = "glm", 
                     metric = "RMSE",
                     tuneLength = 5, trControl = cvCtrl)

summary(Model)
