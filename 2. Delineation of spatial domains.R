#' ---
#' title: "Delineation of spatial domains - Rwanda"
#' author: "Frédéric Baudron"
#' date: "May 26th, 2025"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list = ls())


# INSTALLING NECESSARY PACKAGES-------------------------------------------------

if (!require('terra')) install.packages("terra")
if (!require('geodata')) install.packages("geodata")
if (!require('ade4')) install.packages("ade4")
if (!require('factoextra')) install.packages("factoextra")
if (!require('randomForest')) install.packages("randomForest")


# LOADING NECESSARY PACKAGES----------------------------------------------------

library(terra)
library(geodata)
library(ade4)
library(factoextra)
library(randomForest)


# SETTING UP THE DIRECTORY & THE PATH TO LOAD THE RASTERS-----------------------

setwd("D:\\Mes Donnees\\1. Cirad\\UM6P\\Training\\")

path = 'D:\\Mes Donnees\\1. Cirad\\UM6P\\Training\\Data\\'


# LOADING RASTERS---------------------------------------------------------------

# Country shapefile

rwa0 = gadm(country = 'RWA', level = 0, path = path)
rwa1 = gadm(country = 'RWA', level = 1, path = path)
rwa2 = gadm(country = 'RWA', level = 2, path = path)
rwa3 = gadm(country = 'RWA', level = 3, path = path)

plot(rwa0)
plot(rwa1)
plot(rwa2)
plot(rwa3)


# Cropland (will be used to mask out what is not cropland)

cropland = geodata::cropland(source = "WorldCover", year = 2019, path = path)
plot(cropland)

cropland = ifel(cropland$cropland > 0, 1, NA) # raster of presence
plot(cropland)

cropland = crop(cropland, rwa0) # crop to Rwanda's cropland
cropland = mask(cropland, rwa0)
plot(cropland)


png("Maps\\cropland_presence.png", units = "in", width = 7, height = 6, res = 1000)
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,1), cex.main = 1.95, cex.axis = 1.6)
plot(rwa0, col='lightgrey', main = 'Cropland presence', panel.first=grid(col="gray", lty="solid"), pax=list(sides=1:2, cex.axis=1.5))
plot(cropland, col = "forestgreen",
     legend = FALSE, axes = F, add = T)
plot(rwa1, axes = F, add = T)
points(30.0606, -1.9536, pch = 21, bg = 'orangered', cex = 2)
dev.off()

writeRaster(cropland, "Data\\1. Rasters\\cropland_presence.tiff", overwrite = T)


# Elevation

elevation = geodata::elevation_30s(country = 'RWA', path = path)
elevation = crop(elevation, cropland)
elevation = mask(elevation, cropland)
names(elevation) = 'elevation'
png("Maps\\elevation.png", units = "in", width = 7.5, height = 6, res = 1000)
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,1), cex.main = 1.95, cex.axis = 1.6)
plot(rwa0, col='lightgrey', main = 'Elevation (m a.s.l.)', panel.first=grid(col="gray", lty="solid"), pax=list(sides=1:2, cex.axis=1.5))
plot(elevation, 
     legend = TRUE, axes = F, add = T)
plot(rwa1, axes = F, add = T)
points(30.0606, -1.9536, pch = 21, bg = 'orangered', cex = 2)
dev.off()

writeRaster(elevation, "Data\\1. Rasters\\elevation.tiff", overwrite = T)


# https://www.isric.org/projects/soil-property-maps-africa-250-m-resolution 

# Soc 

soc_5 = geodata::soil_af(var = 'SOC', depth = 5, path = path)
soc_15 = geodata::soil_af(var = 'SOC', depth = 15, path = path)
soc_30 = geodata::soil_af(var = 'SOC', depth = 30, path = path)
soc = (soc_5 * 5 + soc_15 * 10 + soc_30 * 15) / (5 + 10 + 15)
soc = crop(soc, cropland)
soc = mask(soc, cropland)
names(soc) = 'soc'
png("Maps\\soc.png", units = "in", width = 7.5, height = 6, res = 1000)
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,1), cex.main = 1.95, cex.axis = 1.6)
plot(rwa0, col='lightgrey', main = 'Soil organic carbon (g/kg 0-30 cm depth)', panel.first=grid(col="gray", lty="solid"), pax=list(sides=1:2, cex.axis=1.5))
plot(soc, 
     legend = TRUE, axes = F, add = T)
plot(rwa1, axes = F, add = T)
points(30.0606, -1.9536, pch = 21, bg = 'orangered', cex = 2)
dev.off()

writeRaster(soc, "Data\\1. Rasters\\soc.tiff", overwrite = T)


# texture

sand_5 = geodata::soil_af(var = 'sand', depth = 5, path = path)
sand_15 = geodata::soil_af(var = 'sand', depth = 15, path = path)
sand_30 = geodata::soil_af(var = 'sand', depth = 30, path = path)
sand = (sand_5 * 5 + sand_15 * 10 + sand_30 * 15) / (5 + 10 + 15)
sand = crop(sand, cropland)
sand = mask(sand, cropland)
names(sand) = 'sand'
png("Maps\\sand.png", units = "in", width = 7.5, height = 6, res = 1000)
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,1), cex.main = 1.95, cex.axis = 1.6)
plot(rwa0, col='lightgrey', main = 'Sand (%, 0-30 cm depth)', panel.first=grid(col="gray", lty="solid"), pax=list(sides=1:2, cex.axis=1.5))
plot(sand, 
     legend = TRUE, axes = F, add = T)
plot(rwa1, axes = F, add = T)
points(30.0606, -1.9536, pch = 21, bg = 'orangered', cex = 2)
dev.off()

writeRaster(sand, "Data\\1. Rasters\\sand.tiff", overwrite = T)


# CEC

cec_5 = geodata::soil_af(var = 'CEC', depth = 5, path = path)
cec_15 = geodata::soil_af(var = 'CEC', depth = 15, path = path)
cec_30 = geodata::soil_af(var = 'CEC', depth = 30, path = path)
cec = (cec_5 * 5 + cec_15 * 10 + cec_30 * 15) / (5 + 10 + 15)
cec = crop(cec, cropland)
cec = mask(cec, cropland)
names(cec) = 'cec'
png("Maps\\cec.png", units = "in", width = 7.5, height = 6, res = 1000)
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,1), cex.main = 1.95, cex.axis = 1.6)
plot(rwa0, col='lightgrey', main = 'Cation Exchange Capacity (cmol(+)/kg 0-30 cm depth)', panel.first=grid(col="gray", lty="solid"), pax=list(sides=1:2, cex.axis=1.5))
plot(cec, 
     legend = TRUE, axes = F, add = T)
plot(rwa1, axes = F, add = T)
points(30.0606, -1.9536, pch = 21, bg = 'orangered', cex = 2)
dev.off()

writeRaster(cec, "Data\\1. Rasters\\cec.tiff", overwrite = T)


# pH

ph_5 = geodata::soil_af(var = 'pH', depth = 5, path = path)
ph_15 = geodata::soil_af(var = 'pH', depth = 15, path = path)
ph_30 = geodata::soil_af(var = 'pH', depth = 30, path = path)
ph = -log10((10^-ph_5 * 5 + 10^-ph_15 * 10 + 10^-ph_30 * 15) / (5 + 10 + 15))
ph = crop(ph, cropland)
ph = mask(ph, cropland)
names(ph) = 'ph'
png("Maps\\ph.png", units = "in", width = 7.5, height = 6, res = 1000)
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,1), cex.main = 1.95, cex.axis = 1.6)
plot(rwa0, col='lightgrey', main = 'pH H20 (0-30 cm depth)', panel.first=grid(col="gray", lty = "solid"), pax = list(sides = 1:2, cex.axis = 1.5))
plot(ph, 
     legend = TRUE, axes = F, add = T)
plot(rwa1, axes = F, add = T)
points(30.0606, -1.9536, pch = 21, bg = 'orangered', cex = 2)
dev.off()

writeRaster(ph, "Data\\1. Rasters\\ph.tiff", overwrite = T)


# exchangeable acidity

acid_exch_5 = geodata::soil_af(var = 'acid-exch', depth = 5, path = path)
acid_exch_15 = geodata::soil_af(var = 'acid-exch', depth = 15, path = path)
acid_exch_30 = geodata::soil_af(var = 'acid-exch', depth = 30, path = path)
acid_exch = (acid_exch_5 * 5 + acid_exch_15 * 10 + acid_exch_30 * 15) / (5 + 10 + 15)
acid_exch = crop(acid_exch, cropland)
acid_exch = mask(acid_exch, cropland)
names(acid_exch) = 'acid_exch'
png("Maps\\acid_exch.png", units = "in", width = 7.5, height = 6, res = 1000)
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,1), cex.main = 1.95, cex.axis = 1.6)
plot(rwa0, col='lightgrey', main = 'Exchangeable acidity (cmol(+)/kg, 0-30 cm depth)', panel.first=grid(col="gray", lty = "solid"), pax = list(sides = 1:2, cex.axis = 1.5))
plot(acid_exch, 
     legend = TRUE, axes = F, add = T)
plot(rwa1, axes = F, add = T)
points(30.0606, -1.9536, pch = 21, bg = 'orangered', cex = 2)
dev.off()

writeRaster(acid_exch, "Data\\1. Rasters\\acid_exch.tiff", overwrite = T)


# precipitation in Season A and in Season B

prec = worldclim_country(country = 'RWA', var = 'prec', path = path, res = 0.5)

prec_a = prec$RWA_wc2.1_30s_prec_9 + prec$RWA_wc2.1_30s_prec_10 + prec$RWA_wc2.1_30s_prec_11 + prec$RWA_wc2.1_30s_prec_12
prec_a = crop(prec_a, cropland)
prec_a = mask(prec_a, cropland)
names(prec_a) = 'prec_a'
png("Maps\\prec_a.png", units = "in", width = 7.5, height = 6, res = 1000)
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,1), cex.main = 1.95, cex.axis = 1.6)
plot(rwa0, col='lightgrey', main = 'Precipitation in Season A (mm)', panel.first=grid(col="gray", lty = "solid"), pax = list(sides = 1:2, cex.axis = 1.5))
plot(prec_a, 
     legend = TRUE, axes = F, add = T)
plot(rwa1, axes = F, add = T)
points(30.0606, -1.9536, pch = 21, bg = 'orangered', cex = 2)
dev.off()

writeRaster(prec_a, "Data\\1. Rasters\\prec_a.tiff", overwrite = T)


prec_b = prec$RWA_wc2.1_30s_prec_2 + prec$RWA_wc2.1_30s_prec_3 + prec$RWA_wc2.1_30s_prec_4 + prec$RWA_wc2.1_30s_prec_5
prec_b = crop(prec_b, cropland)
prec_b = mask(prec_b, cropland)
names(prec_b) = 'prec_b'
png("Maps\\prec_b.png", units = "in", width = 7.5, height = 6, res = 1000)
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,1), cex.main = 1.95, cex.axis = 1.6)
plot(rwa0, col='lightgrey', main = 'Precipitation in Season B (mm)', panel.first=grid(col="gray", lty = "solid"), pax = list(sides = 1:2, cex.axis = 1.5))
plot(prec_b, 
     legend = TRUE, axes = F, add = T)
plot(rwa1, axes = F, add = T)
points(30.0606, -1.9536, pch = 21, bg = 'orangered', cex = 2)
dev.off()

writeRaster(prec_b, "Data\\1. Rasters\\prec_b.tiff", overwrite = T)


# minimum temperature in Seaason A and in Season B

tmin = worldclim_country(country = 'RWA', var = 'tmin', path = path)

tmin_a = (tmin$RWA_wc2.1_30s_tmin_9 + tmin$RWA_wc2.1_30s_tmin_10 + tmin$RWA_wc2.1_30s_tmin_11 + tmin$RWA_wc2.1_30s_tmin_12)/4
tmin_a = crop(tmin_a, cropland)
tmin_a = mask(tmin_a, cropland)
names(tmin_a) = 'tmin_a'
png("Maps\\tmin_a.png", units = "in", width = 7.5, height = 6, res = 1000)
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,1), cex.main = 1.95, cex.axis = 1.6)
plot(rwa0, col='lightgrey', main = 'Minimum temperature in Seasson A (°C)', panel.first=grid(col="gray", lty = "solid"), pax = list(sides = 1:2, cex.axis = 1.5))
plot(tmin_a, 
     legend = TRUE, axes = F, add = T)
plot(rwa1, axes = F, add = T)
points(30.0606, -1.9536, pch = 21, bg = 'orangered', cex = 2)
dev.off()

writeRaster(tmin_a, "Data\\1. Rasters\\tmin_a.tiff", overwrite = T)


tmin_b = (tmin$RWA_wc2.1_30s_tmin_2 + tmin$RWA_wc2.1_30s_tmin_3 + tmin$RWA_wc2.1_30s_tmin_4 + tmin$RWA_wc2.1_30s_tmin_5)/4
tmin_b = crop(tmin_b, cropland)
tmin_b = mask(tmin_b, cropland)
names(tmin_b) = 'tmin_b'
png("Maps\\tmin_b.png", units = "in", width = 7.5, height = 6, res = 1000)
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,1), cex.main = 1.95, cex.axis = 1.6)
plot(rwa0, col='lightgrey', main = 'Minimum temperature in Seasson A (°C)', panel.first=grid(col="gray", lty = "solid"), pax = list(sides = 1:2, cex.axis = 1.5))
plot(tmin_b, 
     legend = TRUE, axes = F, add = T)
plot(rwa1, axes = F, add = T)
points(30.0606, -1.9536, pch = 21, bg = 'orangered', cex = 2)
dev.off()

writeRaster(tmin_b, "Data\\1. Rasters\\tmin_b.tiff", overwrite = T)


# maximum temperature in Seaason A and in Season B

tmax = worldclim_country(country = 'RWA', var = 'tmax', path = path)

tmax_a = (tmax$RWA_wc2.1_30s_tmax_9 + tmax$RWA_wc2.1_30s_tmax_10 + tmax$RWA_wc2.1_30s_tmax_11 + tmax$RWA_wc2.1_30s_tmax_12)/4
tmax_a = crop(tmax_a, cropland)
tmax_a = mask(tmax_a, cropland)
names(tmax_a) = 'tmax_a'
png("Maps\\tmax_a.png", units = "in", width = 7.5, height = 6, res = 1000)
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,1), cex.main = 1.95, cex.axis = 1.6)
plot(rwa0, col='lightgrey', main = 'Maximum temperature in Seasson A (°C)', panel.first=grid(col="gray", lty = "solid"), pax = list(sides = 1:2, cex.axis = 1.5))
plot(tmax_a, 
     legend = TRUE, axes = F, add = T)
plot(rwa1, axes = F, add = T)
points(30.0606, -1.9536, pch = 21, bg = 'orangered', cex = 2)
dev.off()

writeRaster(tmax_a, "Data\\1. Rasters\\tmax_a.tiff", overwrite = T)


tmax_b = (tmax$RWA_wc2.1_30s_tmax_2 + tmax$RWA_wc2.1_30s_tmax_3 + tmax$RWA_wc2.1_30s_tmax_4 + tmax$RWA_wc2.1_30s_tmax_5)/4
tmax_b = crop(tmax_b, cropland)
tmax_b = mask(tmax_b, cropland)
names(tmax_b) = 'tmax_b'
png("Maps\\tmax_b.png", units = "in", width = 7.5, height = 6, res = 1000)
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,1), cex.main = 1.95, cex.axis = 1.6)
plot(rwa0, col='lightgrey', main = 'Maximum temperature in Seasson A (°C)', panel.first=grid(col="gray", lty = "solid"), pax = list(sides = 1:2, cex.axis = 1.5))
plot(tmax_b, 
     legend = TRUE, axes = F, add = T)
plot(rwa1, axes = F, add = T)
points(30.0606, -1.9536, pch = 21, bg = 'orangered', cex = 2)
dev.off()

writeRaster(tmax_b, "Data\\1. Rasters\\tmax_b.tiff", overwrite = T)


# average temperature in Seaason A and in Season B

tavg = worldclim_country(country = 'RWA', var = 'tavg', path = path)

tavg_a = (tavg$RWA_wc2.1_30s_tavg_9 + tavg$RWA_wc2.1_30s_tavg_10 + tavg$RWA_wc2.1_30s_tavg_11 + tavg$RWA_wc2.1_30s_tavg_12)/4
tavg_a = crop(tavg_a, cropland)
tavg_a = mask(tavg_a, cropland)
names(tavg_a) = 'tavg_a'
png("Maps\\tavg_a.png", units = "in", width = 7.5, height = 6, res = 1000)
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,1), cex.main = 1.95, cex.axis = 1.6)
plot(rwa0, col='lightgrey', main = 'Average temperature in Seasson A (°C)', panel.first=grid(col="gray", lty = "solid"), pax = list(sides = 1:2, cex.axis = 1.5))
plot(tavg_a, 
     legend = TRUE, axes = F, add = T)
plot(rwa1, axes = F, add = T)
points(30.0606, -1.9536, pch = 21, bg = 'orangered', cex = 2)
dev.off()

writeRaster(tavg_a, "Data\\1. Rasters\\tavg_a.tiff", overwrite = T)


tavg_b = (tavg$RWA_wc2.1_30s_tavg_2 + tavg$RWA_wc2.1_30s_tavg_3 + tavg$RWA_wc2.1_30s_tavg_4 + tavg$RWA_wc2.1_30s_tavg_5)/4
tavg_b = crop(tavg_b, cropland)
tavg_b = mask(tavg_b, cropland)
names(tavg_b) = 'tavg_b'
png("Maps\\tavg_b.png", units = "in", width = 7.5, height = 6, res = 1000)
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,1), cex.main = 1.95, cex.axis = 1.6)
plot(rwa0, col='lightgrey', main = 'Average temperature in Seasson A (°C)', panel.first=grid(col="gray", lty = "solid"), pax = list(sides = 1:2, cex.axis = 1.5))
plot(tavg_b, 
     legend = TRUE, axes = F, add = T)
plot(rwa1, axes = F, add = T)
points(30.0606, -1.9536, pch = 21, bg = 'orangered', cex = 2)
dev.off()

writeRaster(tavg_b, "Data\\1. Rasters\\tavg_b.tiff", overwrite = T)


# incident solar radiation

srad = worldclim_country(country = 'RWA', var = 'srad', path = path)

srad_a = (srad$RWA_wc2.1_30s_srad_9 + srad$RWA_wc2.1_30s_srad_10 + srad$RWA_wc2.1_30s_srad_11 + srad$RWA_wc2.1_30s_srad_12)/4
srad_a = crop(srad_a, cropland)
srad_a = mask(srad_a, cropland)
names(srad_a) = 'srad_a'
png("Maps\\srad_a.png", units = "in", width = 7.5, height = 6, res = 1000)
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,1), cex.main = 1.95, cex.axis = 1.6)
plot(rwa0, col='lightgrey', main = 'Incident solar radiation in Seasson A (kJ/m2/jour)', panel.first=grid(col="gray", lty = "solid"), pax = list(sides = 1:2, cex.axis = 1.5))
plot(srad_a, 
     legend = TRUE, axes = F, add = T)
plot(rwa1, axes = F, add = T)
points(30.0606, -1.9536, pch = 21, bg = 'orangered', cex = 2)
dev.off()

writeRaster(srad_a, "Data\\1. Rasters\\srad_a.tiff", overwrite = T)


srad_b = (srad$RWA_wc2.1_30s_srad_2 + srad$RWA_wc2.1_30s_srad_3 + srad$RWA_wc2.1_30s_srad_4 + srad$RWA_wc2.1_30s_srad_5)/4
srad_b = crop(srad_b, cropland)
srad_b = mask(srad_b, cropland)
names(srad_b) = 'srad_b'
png("Maps\\srad_b.png", units = "in", width = 7.5, height = 6, res = 1000)
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,1), cex.main = 1.95, cex.axis = 1.6)
plot(rwa0, col='lightgrey', main = 'Incident solar radiation in Seasson B (kJ/m2/jour)', panel.first=grid(col="gray", lty = "solid"), pax = list(sides = 1:2, cex.axis = 1.5))
plot(srad_b, 
     legend = TRUE, axes = F, add = T)
plot(rwa1, axes = F, add = T)
points(30.0606, -1.9536, pch = 21, bg = 'orangered', cex = 2)
dev.off()

writeRaster(srad_b, "Data\\1. Rasters\\srad_b.tiff", overwrite = T)


# population

pop = geodata::population(year = 2020, path = path)
pop = resample(pop, cropland)

pop
cropland

pop = crop(pop, cropland)
pop = mask(pop, cropland)

# 1 degree latitude at the equator = 111320 m
# 0.008333333 * 111320 = 927.6666

names(pop) = 'pop'
png("Maps\\pop.png", units = "in", width = 7.5, height = 6, res = 1000)
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,1), cex.main = 1.95, cex.axis = 1.6)
plot(rwa0, col='lightgrey', main = 'Population (inhab/cell of ~1 km2)', panel.first=grid(col="gray", lty="solid"), pax=list(sides=1:2, cex.axis=1.5))
plot(pop, 
     legend = TRUE, axes = F, add = T)
plot(rwa1, axes = F, add = T)
points(30.0606, -1.9536, pch = 21, bg = 'orangered', cex = 2)
dev.off()

writeRaster(pop, "Data\\1. Rasters\\pop.tiff", overwrite = T)


# travel time

travel = geodata::travel_time(to = "city", size = 2, path = path)
travel = crop(travel, cropland)
travel = mask(travel, cropland)
names(travel) = 'travel'
png("Maps\\travel.png", units = "in", width = 7.5, height = 6, res = 1000)
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,1), cex.main = 1.95, cex.axis = 1.6)
plot(rwa0, col='lightgrey', main = 'Travel time to the nearest city of > 1 M inhab (min)', panel.first=grid(col="gray", lty="solid"), pax=list(sides=1:2, cex.axis=1.5))
plot(travel, 
     legend = TRUE, axes = F, add = T)
plot(rwa1, axes = F, add = T)
points(30.0606, -1.9536, pch = 21, bg = 'orangered', cex = 2)
dev.off()

writeRaster(travel, "Data\\1. Rasters\\travel.tiff", overwrite = T)


# PCA & HCA---------------------------------------------------------------------

elevation = rast("Data\\1. Rasters\\elevation.tiff")
soc = rast("Data\\1. Rasters\\soc.tiff")
sand = rast("Data\\1. Rasters\\sand.tiff")
cec = rast("Data\\1. Rasters\\cec.tiff")
ph = rast("Data\\1. Rasters\\ph.tiff")
acid_exch = rast("Data\\1. Rasters\\acid_exch.tiff")
prec_a = rast("Data\\1. Rasters\\prec_a.tiff")
prec_b = rast("Data\\1. Rasters\\prec_b.tiff")
tmin_a = rast("Data\\1. Rasters\\tmin_a.tiff")
tmin_b = rast("Data\\1. Rasters\\tmin_b.tiff")
tmax_a = rast("Data\\1. Rasters\\tmax_a.tiff")
tmax_b = rast("Data\\1. Rasters\\tmax_b.tiff")
tavg_a = rast("Data\\1. Rasters\\tavg_a.tiff")
tavg_b = rast("Data\\1. Rasters\\tavg_b.tiff")
srad_a = rast("Data\\1. Rasters\\srad_a.tiff")
srad_b = rast("Data\\1. Rasters\\srad_b.tiff")
pop = rast("Data\\1. Rasters\\pop.tiff")
travel = rast("Data\\1. Rasters\\travel.tiff")


# stack of the rasters to be used in the PCA
stacked = c(elevation, soc, sand, cec, ph, acid_exch, prec_a, prec_b,
            tmin_a, tmin_b, tmax_a, tmax_b, tavg_a, tavg_b, srad_a, srad_b,
            pop, travel)

plot(stacked)

stacked_original = stacked  # copy of your spatial data before transformation


# remove outliers

terra::boxplot(stacked$elevation)
stacked = ifel(stacked$elevation > 3500, NA, stacked)
terra::boxplot(stacked$elevation)
terra::boxplot(stacked$sand)
terra::boxplot(stacked$cec)
terra::boxplot(stacked$ph)
terra::boxplot(stacked$acid_exch)
terra::boxplot(stacked$prec_a)
terra::boxplot(stacked$prec_b)
terra::boxplot(stacked$tmin_a)
terra::boxplot(stacked$tmin_b)
terra::boxplot(stacked$tmax_a)
terra::boxplot(stacked$tmax_b)
terra::boxplot(stacked$tavg_a)
terra::boxplot(stacked$tavg_b)
terra::boxplot(stacked$srad_a)
terra::boxplot(stacked$srad_b)
terra::boxplot(stacked$pop)
terra::boxplot(stacked$travel)


# check that the data is roughly normally distributed, log-transform otherwise

terra::hist(stacked$elevation)
terra::hist(stacked$sand)
terra::hist(stacked$cec)
terra::hist(stacked$ph)
terra::hist(stacked$acid_exch)
terra::hist(stacked$prec_a)
terra::hist(stacked$prec_b)
terra::hist(stacked$tmin_a)
terra::hist(stacked$tmin_b)
terra::hist(stacked$tmax_a)
terra::hist(stacked$tmax_b)
terra::hist(stacked$tavg_a)
terra::hist(stacked$tavg_b)
terra::hist(stacked$srad_a)
terra::hist(stacked$srad_b)
terra::hist(stacked$pop)
# stacked$pop = log10(stacked$pop + (0.5 * global(stacked$pop, min, na.rm = T)$min))
# terra::hist(stacked$pop)
terra::hist(stacked$travel)
terra::hist(stacked$travel)


stacked = scale(stacked)  # scaling is always a good idea when using variables that may be on very different scales


# sample 10,000 random grid cells

set.seed(123)  #ensure the same random numbers are generated each session (reproducibility)
sr = terra::spatSample(stacked, min(10000, ncell(stacked)), method = "random", na.rm = F, as.raster = F, as.df = T, xy = T)
sr = na.omit(sr)


# run pca 

# pca = dudi.pca(sr[, -c(1:2)])
pca = dudi.pca(sr[, -c(1:2)], scannf=F, nf=4)
(eig.val = data.frame(eigenvalue=c(pca$eig)))
(exp.var = c(cumsum(pca$eig) / sum(pca$eig)))
(loading = pca$co)
fviz_pca_var(pca, col.var="contrib") + 
  scale_color_gradient2(low = "blue", mid = "purple", high = "red", midpoint = 4.5) +
  theme_minimal() 


# temperature variables highly correlated: re-runing with tavg_a only


# pca = dudi.pca(sr[, -c(1:2, 11:14, 16)])
pca = dudi.pca(sr[, -c(1:2, 11:14, 16)], scannf=F, nf=3)
(eig.val = data.frame(eigenvalue=c(pca$eig)))
(exp.var = c(cumsum(pca$eig) / sum(pca$eig)))
(loading = pca$co)
fviz_pca_var(pca, col.var="contrib") + 
  scale_color_gradient2(low = "blue", mid = "purple", high = "red", midpoint = 7) +
  theme_minimal() 


# plotting all points on the PC1-PC2 plane and checking for possible outliers
fviz_pca_ind(pca)


# remove outlier (defined as any point more than 1.5 time the interquantile range⋅ above the third quartile or below the first quartile)
outliers = subset(pca$li, Axis1 < mean(pca$li$Axis1) - 1.5 * IQR(pca$li$Axis1) |
                    Axis1 > mean(pca$li$Axis1) + 1.5 * IQR(pca$li$Axis1) |
                    Axis2 < mean(pca$li$Axis2) - 1.5 * IQR(pca$li$Axis2) |
                    Axis2 > mean(pca$li$Axis2) + 1.5 * IQR(pca$li$Axis2))
sr$new = row.names(sr)
'%!in%' = function(x,y)!('%in%'(x,y))
sr = subset(sr, new %!in% row.names(outliers))


# re-run again pca

# pca = dudi.pca(sr[, -c(1:2, 11:14, 16, 21)])
pca = dudi.pca(sr[, -c(1:2, 11:14, 16, 21)], scannf=F, nf=4)
(eig.val = data.frame(eigenvalue=c(pca$eig)))
(exp.var = c(cumsum(pca$eig) / sum(pca$eig)))
(loading = pca$co)
fviz_pca_var(pca, col.var="contrib") + 
  scale_color_gradient2(low="blue", mid="purple", high="red", midpoint=6) +
  theme_minimal() 


fviz_pca_ind(pca)


# predict principal components from stack of rasters

unused = c("tmin_a", "tmin_b", "tmax_a", "tmax_b", "tavg_b")
stacked = subset(stacked, unused, negate = TRUE)

pc_rast = terra::predict(stacked, pca)
names(pc_rast) = c('pc1', 'pc2', 'pc3', 'pc4')
plot(pc_rast)


# applying the hierarchical clustering on the PCs

v = data.frame(terra::values(pc_rast))
v_na = na.omit(v)
hclust_r = hclust(dist(v_na), method = "ward.D")
hclust_dendo = as.dendrogram(hclust_r)
plot(hclust_dendo, type = "rectangle", ylab = "Height", leaflab = "none")
hclust_cluster = cutree(hclust_r, k = 5)


# get clusters

v_na$cluster = hclust_cluster
v_na$id = row.names(v_na)


# plot clusters on the PC1-PC2 plane

s.class(na.omit(v), fac = as.factor(hclust_cluster), col = c('wheat', 'yellowgreen', "green", 'forestgreen', 'darkgreen'))
s.label(na.omit(v), xax = 1, yax = 2)


# back to spat raster

hclust_raster = terra::rast(pc_rast, nlyr = 1)  # create an empty raster with 1 layer and with the geometry (extent, resolution, etc) of pc_rast
hclust_raster[which(!is.na(v[,1]))] = v_na$cluster # transfering cluster value to the raster (outside of NAs) 
names(hclust_raster) = 'cluster'
pc_rast = c(pc_rast, hclust_raster)


# map

png("Maps\\Predictions\\domains.png", units = "in", width = 7.5, height = 6, res = 1000)
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,1), cex.main = 1.95, cex.axis = 1.6)
pal = colorRampPalette(c('wheat', 'yellowgreen',"green", 'forestgreen', 'darkgreen'))
plot(rwa0, col='lightgrey', main='Domains', panel.first=grid(col="gray", lty="solid"), pax=list(sides=1:2, cex.axis=1.5))
plot(pc_rast$cluster, col=pal(5), legend=T, axes=F, add=T)
plot(rwa1, axes=F, add=T)
points(30.0606, -1.9536, pch = 21, bg = 'orangered', cex = 2)
dev.off()


# CLUSTER CHARACTERIZATION------------------------------------------------------

# dataframes

all = c(pc_rast, stacked_original)
all_df = terra::as.data.frame(all)


# random forest to understand what variables are the most segregating 

all_df = all_df[-c(1:4, 14:17)]
all_df = na.omit(all_df)
rf = randomForest(as.factor(cluster) ~ ., all_df)
varImpPlot(rf)


# plotting some of these variables

png("typology-interpretation.png", units = "in", width = 7.5, height = 5, res = 1000)
par(mfrow = c(2,2), xaxs = 'i', yaxs = 'i', mar = c(3,5,1,1), 
    cex.axis=1.4, cex.lab=1.6)
pal = colorRampPalette(c('wheat', 'yellowgreen', 'green', 'forestgreen', 'darkgreen'))
boxplot(all_df$prec_a ~ all_df$cluster, 
        ylab='prec A', xlab='', 
        col=pal(5))
boxplot(all_df$srad_b ~ all_df$cluster, 
        ylab='srad B', xlab='', 
        col=pal(5))
boxplot(all_df$cec ~ all_df$cluster, 
        ylab='cec', xlab='', 
        col=pal(5))
boxplot(all_df$soc ~ all_df$cluster, 
        ylab='soc', xlab='', 
        col=pal(5))
dev.off()
