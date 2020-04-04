library("sf")
library("raster")
library("spData")
library("spDataLarge")
library(dplyr)
library(ggplot2)

raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
new_raster = raster(raster_filepath)
new_raster
new_raster[]
plot(new_raster)

max(new_raster[], na.rm = TRUE)
mean(new_raster[], na.rm = TRUE)

hist(new_raster, maxpixels=250000)
