library(sf)
library(raster)
library(spData)
library(spDataLarge)
library(dplyr)
library(ggplot2)

View(world)

world %>% select(name_long,pop) %>% arrange(pop)%>% slice(0:1)

world %>% filter(continent == 'Asia') %>% count()

world %>% filter( type == 'sovereign country')

