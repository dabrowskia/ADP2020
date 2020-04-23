library(sf)
library(raster)
library(spData)
library(spDataLarge)
library(dplyr)
library(ggplot2)

View(world)

qplot(world[world$type == "Sovereign country","area_km2"], geom = "histogram")


world %>% filter( type == "Sovereign country") %>% select(name_long, area_km2) %>% 
  ggplot(mapping = aes(x = name_long, y = area_km2))+geom_bar(stat = "identity", fill = "lightblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


world %>% select(lifeExp, gdpPercap ) %>% ggplot(mapping = aes(x = lifeExp, y = gdpPercap)) + geom_point()
