library(sf)
library(raster)
library(spData)
library(spDataLarge)
library(dplyr)
library(ggplot2)

names(world)
class(world)
plot(world)

summary(world)
summary(world["lifeExp"]) 

world_mini <- world[1:2,1:3]
world_mini


# Wyświetlcie mapę krajów Europejskich  wg liczby ludności
world %>%
  filter(continent == "Europe") %>%
  select(pop) %>%
  plot()


world %>%
  filter(continent == "Europe") %>%
  select(pop) -> europe_pop

plot(europe_pop)


# Który kraj posiada najmniejszą liczbę ludności i ile wynosi?
world %>%
  arrange(pop) %>%
  select(name_long, pop) %>%
  top_n(-10, wt=pop)


# Ile krajów znajduje się w Azji?
world %>%
  filter(continent == "Asia") %>%
  count() 

# Wyświetlcie histogram powierzchni  wszystkich niezależnych krajów (Sovereign Country)
world %>%
  filter(type == "Sovereign country") %>%
  ggplot(aes(area_km2)) +
  geom_histogram(bins = 150)

# Wyświetlcie wykres punktowy relacji pomiędzy lifeExp, a gdpPercap

world %>%
  ggplot(aes(x = lifeExp, y = gdpPercap)) +
  geom_smooth()



