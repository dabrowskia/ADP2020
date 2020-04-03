library("sf")
library("raster")
library("spData")
library("spDataLarge")
library(dplyr)
library(ggplot2)

names(world)
class(world)
plot(world)

summary(world)
summary(world["lifeExp"]) 

world_mini <- world[1:2,1:3]
world_mini


# Wy�wietlcie map� kraj�w Europejskich  wg liczby ludno�ci
world %>%
  filter(continent == "Europe") %>%
  select(pop) %>%
  plot()


world %>%
  filter(continent == "Europe") %>%
  select(pop) -> europe_pop

plot(europe_pop)


# Kt�ry kraj posiada najmniejsz� liczb� ludno�ci i ile wynosi?
world %>%
  arrange(pop) %>%
  select(name_long,pop) %>%
  top_n(-10,wt=pop)


# Ile kraj�w znajduje si� w Azji?
world %>%
  filter(continent == "Asia") %>%
  count() 

# Wy�wietlcie histogram powierzchni  wszystkich niezale�nych kraj�w (Sovereign Country)
world %>%
  filter(type == "Sovereign country") %>%
  ggplot(aes(area_km2)) +
  geom_histogram(bins = 150)

# Wy�wietlcie wykres punktowy relacji pomi�dzy lifeExp, a gdpPercap

world %>%
  ggplot(aes(x = lifeExp, y = gdpPercap)) +
  geom_smooth()



