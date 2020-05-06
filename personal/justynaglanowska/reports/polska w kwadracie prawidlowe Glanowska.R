---
title: "polska"
author: "justyna glanowska"
date: "1 05 2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
```{r}
library(spData)
library(sf)
library(dplyr)
library(ggplot2)
```

```{r}
polygon_list = list(rbind(c(13.9, 55), c(24.2, 55), c(24.2, 48.9), c(14, 48.9), c(13.9, 55)))

kwadrat <- st_polygon(polygon_list) %>%
  st_sfc(crs= 4326) %>%
  st_sf()
```

```{r}
linestring_matrix = rbind(c(13, 52), c(14, 49))

line <- st_linestring(linestring_matrix) %>%
  st_sfc(crs=4326)%>%
  st_sf()    
```
```{r}
Polska <-world %>%
  filter(name_long =="Poland")

ggplot() +
  geom_sf(data = kwadrat) +
  geom_sf (data = Polska) +
  geom_sf(data = line)
```

