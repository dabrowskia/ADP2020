install.packages("sf") 
install.packages("raster")
install.packages("spData")
devtools::install_github("Nowosad/spDataLarge")



library(sf)
library(raster)
library(spData)
library(spDataLarge)
library(dplyr)

head(world)

View(world)
      
world %>%
select(pop,continent) %>%
filter( continent == 'Europe')%>%
plot()

plot(world[world$continent == "Europe", "pop"])


plot(world)
summary(world["lifeExp"]) 
world_mini = world[1:2, 1:3]

