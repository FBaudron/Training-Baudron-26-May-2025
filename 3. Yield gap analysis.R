#' ---
#' title: "Delineation of spatial domains - Rwanda"
#' author: "Frédéric Baudron"
#' date: "May 26th, 2025"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list = ls())


# INSTALLING NECESSARY PACKAGES-------------------------------------------------

if (!require('openxlsx')) install.packages("openxlsx")
if (!require('terra')) install.packages("terra")
if (!require('geodata')) install.packages("geodata")
if (!require('dplyr')) install.packages("dplyr")
if (!require('tidyr')) install.packages("tidyr")
if (!require('sf')) install.packages("sf")
if (!require('ggplot2')) install.packages("ggplot2")
if (!require('ggspatial')) install.packages("ggspatial")
if (!require('tidyterra')) install.packages("tidyterra")
if (!require('randomForest')) install.packages("randomForest")
if (!require('kernelshap')) install.packages("kernelshap")
if (!require('shapviz')) install.packages("shapviz")
if (!require('chillR')) install.packages("chillR")
if (!require('caret')) install.packages("caret")


# LOADING NECESSARY PACKAGES----------------------------------------------------

library(openxlsx)
library(terra)
library(geodata)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(tidyterra)
library(randomForest)
library(kernelshap)
library(shapviz)
library(chillR)
library(caret)


# SETTING UP THE DIRECTORY & LOADING THE DATA-----------------------------------

setwd("D:\\Mes Donnees\\1. Cirad\\UM6P\\Training\\")

path = 'D:\\Mes Donnees\\1. Cirad\\UM6P\\Training\\Data\\'


# LOADING THE DATA--------------------------------------------------------------

data = read.xlsx("Data\\2. Datasets\\Data for YGA.xlsx", sheet = 1)


# MAP OF SURVEYED FARMS---------------------------------------------------------

# Converting to spatial object (and jittering points so they are more visible)
data_sf = st_as_sf(data, coords = c("gpslongitudedegrees", "gpslatitudedegrees"), crs ="+proj=longlat + datum = WGS84 + no_defs")
data_sf = st_set_crs(data_sf, 4326)
data_jitt_sf = st_jitter(data_sf, factor = 0.01) # jittering slightly so all points can be visible
data_jitt_sf = as(data_jitt_sf, "Spatial")
data_jitt_sf = vect(data_jitt_sf)
data_jitt_sf = project(data_jitt_sf, "EPSG:4326")


# Country shapefile
rwa1 = gadm(country = 'RWA', level = 1, path = path)


# elevation raster
elevation = terra::rast("Data\\1. Rasters\\elevation.tiff")
elevation_df = as.data.frame(elevation, xy = TRUE)


# map
ggplot() + theme_bw() +
  geom_raster(data = na.omit(elevation_df), aes(x = x, y = y, fill = elevation)) +
  geom_spatvector(data = rwa1, fill = NA, linewidth = 1, color = "black") +
  geom_spatvector(data = data_jitt_sf, shape = 16, aes(color = season), alpha = 0.5, size = 3) +
  scale_fill_distiller(palette = "RdYlGn") +
  scale_color_manual(values = c("blue", "purple")) +
  labs(fill = "Altitude (m.a.s.l.)", color = "Season") +
  theme(plot.title = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right") +
  annotation_scale(location = "bl", width_hint = 0.5, height = unit(0.25, "cm"), 
                   text_cex = 1, pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 10))

ggsave("Maps//Map location surveyed farms.png", units = "cm", width = 25, height = 18, dpi = 320)


# LOADING CROPLAND RASTERS------------------------------------------------------

# elevation
elevation = terra::rast("Data\\1. Rasters\\elevation.tiff")

# SOC 
soc = terra::rast("Data\\1. Rasters\\soc.tiff")

# texture
sand = terra::rast("Data\\1. Rasters\\sand.tiff")

# CEC
cec = terra::rast("Data\\1. Rasters\\cec.tiff")

# pH
ph = terra::rast("Data\\1. Rasters\\ph.tiff")

# exchangeable acidity
acid_exch = terra::rast("Data\\1. Rasters\\acid_exch.tiff")

# precipitation in Season A and in Season B
prec_a = terra::rast("Data\\1. Rasters\\prec_a.tiff")
prec_b = terra::rast("Data\\1. Rasters\\prec_b.tiff")

# minimum temperature in Seaason A and in Season B
tmin_a = terra::rast("Data\\1. Rasters\\tmin_a.tiff")
tmin_b = terra::rast("Data\\1. Rasters\\tmin_b.tiff")

# maximum temperature in Seaason A and in Season B
tmax_a = terra::rast("Data\\1. Rasters\\tmax_a.tiff")
tmax_b = terra::rast("Data\\1. Rasters\\tmax_b.tiff")

# average temperature in Seaason A and in Season B
tavg_a = terra::rast("Data\\1. Rasters\\tavg_a.tiff")
tavg_b = terra::rast("Data\\1. Rasters\\tavg_b.tiff")

# incident solar radiation in Seaason A and in Season B
srad_a = terra::rast("Data\\1. Rasters\\srad_a.tiff")
srad_b = terra::rast("Data\\1. Rasters\\srad_b.tiff")

# population
pop = terra::rast("Data\\1. Rasters\\pop.tiff")

# travel time
travel = terra::rast("Data\\1. Rasters\\travel.tiff")


# EXTRACTION OF SPATIAL DATA----------------------------------------------------

stacked = c(elevation, soc, sand, cec, ph, acid_exch, prec_a, prec_b,
            tmin_a, tmin_b, tmax_a, tmax_b, tavg_a, tavg_b, srad_a, srad_b,
            pop, travel)

plot(stacked)

data = cbind(data, terra::extract(stacked, terra::vect(data_sf)))
data = data[, -c(23)]


# RANDOM FOREST-----------------------------------------------------------------

# create a training sample with 70% of the original data and a validation sample with 20% of the original data
set.seed(12345)
split = sample(c(rep(0, 0.7 * nrow(data)), rep(1, 0.3 * nrow(data))))
train = data[split == 0, ]                                          
valid = data[split == 1, ] 


train = na.omit(train)
train_rf = randomForest(harvest_kg_ha ~ ., data = train[, -c(1, 3:6)])


pred_train = predict(object = train_rf, newdata = train)
pred_valid = predict(object = train_rf, newdata = valid)


# calculate RMSEP and R2

RMSEP_train = RMSEP(pred_train, train$harvest_kg_ha, na.rm = TRUE)
R2_train = R2(pred_train, train$harvest_kg_ha, na.rm = TRUE)

par(mfrow = c(1,1), mar = c(5,5,5,1), cex.axis = 1.4, cex.lab = 1.6)
plot(x = train$harvest_kg_ha, y = pred_train, xlab = "Observed", ylab = "Predicted", main = "Cassava yield - calibration")
abline(0, 1, col = "red")
fit = lm(pred_train ~ train$harvest_kg_ha)
sfit = summary(fit)
# sfit
abline(a = fit$coefficients[1], b = fit$coefficients[2], col = "black")
mtext(paste0("R2 = ", round(R2_train, 2),"; RMSEP = ", round(RMSEP_train, 3)), side = 3)



# calculate RMSEP and R2
RMSEP_test = RMSEP(pred_valid, valid$harvest_kg_ha, na.rm = TRUE)
R2_test = R2(pred_valid, valid$harvest_kg_ha, na.rm = TRUE)

par(mfrow = c(1,1), mar = c(5,5,5,1), cex.axis = 1.4, cex.lab = 1.6)
plot(x = valid$harvest_kg_ha, y = pred_valid, xlab = "Observed", ylab = "Predicted", main = "Cassava yield - validation")
abline(0, 1, col = "red")
fit = lm(pred_valid ~ valid$harvest_kg_ha)
sfit = summary(fit)
# sfit
abline(a = fit$coefficients[1], b = fit$coefficients[2], col = "black")
mtext(paste0("R2 = ", round(R2_test, 2),"; RMSEP = ", round(RMSEP_test, 3)), side = 3)


# ANALYSIS OF SHAP VALUES-------------------------------------------------------

shap_values =  kernelshap(train_rf, na.omit(data[, -c(1, 3:6)]))

shap_values_df = as.data.frame(shap_values$S)

names(shap_values_df) = c("sv_season", "sv_field_size_estimate_ha", "sv_slope_position", "sv_slope_steepness",
                          "sv_soil_depth", "sv_terrace", "sv_fertile_degree", "sv_cycle_duration", "plant_density_pl_ha", 
                          "sv_previous_cas", "sv_previous_crl", "sv_previous_leg", "sv_fert_use", "sv_compost_manure",
                           "sv_pesticide", "sv_times_weeded", "harvest_kg_ha", "sv_elevation", "sv_soc", "sv_ph",
                          "sv_prec_b", "sv_tmin_a", "sv_tmin_b", "sv_tmax_a", "sv_tmax_b", "sv_tavg_a", "sv_tavg_b",
                          "sv_srad_a", "sv_srad_b", "sv_pop", "sv_travel")

data_with_shap = cbind(na.omit(data), shap_values_df)

write.xlsx(data_with_shap, "Data\\2. Datasets\\Data with SHAP values.xlsx", rowNames = F)
write.csv(data_with_shap, "Data\\2. Datasets\\Data with SHAP values.csv", row.names = F)


# https://www.r-bloggers.com/2022/07/shapviz-goes-h2o/
shp = shapviz(shap_values)

sv_importance(shp, show_numbers = TRUE, max_display = Inf)
sv_importance(shp, show_numbers = TRUE, kind = "bee")
ggsave("Graphs//sv_importance.png", units = "cm", width = 30, height = 20, dpi = 320)

sv_dependence(shp, "cycle_duration", "travel", alpha = 0.5)
ggsave("Graphs//sv_cycle_duration.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "travel", "cycle_duration", alpha = 0.5)
ggsave("Graphs//sv_travel.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "sand", "travel", alpha = 0.5)
ggsave("Graphs//sv_sand.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "fertile_degree", "travel", alpha = 0.5)
ggsave("Graphs//sv_fertile_degree.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "season", "travel", alpha = 0.5)
ggsave("Graphs//sv_season.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "ph", "travel", alpha = 0.5)
ggsave("Graphs//sv_ph.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "cec", "travel", alpha = 0.5)
ggsave("Graphs//sv_cec.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "acid_exch", "travel", alpha = 0.5)
ggsave("Graphs//sv_acid_exch.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "pop", "travel", alpha = 0.5)
ggsave("Graphs//sv_pop.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "prec_a", "travel", alpha = 0.5)
ggsave("Graphs//sv_prec_a.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "soc", "travel", alpha = 0.5)
ggsave("Graphs//sv_soc.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "srad_a", "travel", alpha = 0.5)
ggsave("Graphs//sv_srad_a.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "prec_b", "travel", alpha = 0.5)
ggsave("Graphs//sv_prec_b.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "elevation", "travel", alpha = 0.5)
ggsave("Graphs//sv_elevation.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "tmin_a", "travel", alpha = 0.5)
ggsave("Graphs//sv_tmin_a.png", units = "cm", width = 20, height = 15, dpi = 320)


# sample 10,000 random grid cells
set.seed(123)
sr = terra::spatSample(stacked, min(100000, ncell(stacked)), method = "random", na.rm = F, as.raster = F, as.df = T, xy = T)
sr = na.omit(sr)

names(data[, -c(1, 3:6)])


# yield prediction

grid = expand.grid(c("S1"),
                   c(0.05),
                   c("Midslope"),
                   c("Medium"),
                   c("1"),
                   c("0"),
                   c("3"),
                   c(450),
                   c(18000),
                   c("0"),
                   c("0"),
                   c("0"),
                   c("0"),
                   c("0"),
                   c("0"),
                   c("2"),
                   stringsAsFactors = F)

names(grid) = c("season", "field_size_estimate_ha", "slope_position", "slope_steepness",
                "soil_depth", "terrace", "fertile_degree", "cycle_duration", 
                "plant_density_pl_ha", "previous_cas", "previous_crl", "previous_leg",
                "fert_use", "compost_manure", "pesticide", "times_weeded")

grid = merge(grid, sr)

pred_grid = predict(object = train_rf, newdata = grid)

pred_grid_df = as.data.frame(pred_grid)

pred_grid_df = cbind(grid, pred_grid_df)

pred_grid_sf = st_as_sf(pred_grid_df, coords = c("x", "y"), crs ="+proj=longlat + datum = WGS84 + no_defs")
pred_grid_sf = as(pred_grid_sf, "Spatial")
pred_grid_sf = vect(pred_grid_sf)
pred_grid_sf = project(pred_grid_sf, "EPSG:4326")


rwa0 = gadm(country = 'RWA', level = 0, path = path)
rwa1 = gadm(country = 'RWA', level = 1, path = path)


ggplot() + theme_bw() +
  ggtitle("Cassava yield (kg/ha)") +
  geom_spatvector(data = rwa0, fill = "grey90", linewidth = 0.5, color = "black") +
  geom_spatvector(data = pred_grid_sf, shape = 16, aes(color = pred_grid), alpha = 0.5, size = 2) +
  geom_spatvector(data = rwa1, fill = NA, linewidth = 0.5, color = "black") +
  scale_color_viridis_c() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title = element_blank())

ggsave("Maps\\Predictions\\Yield.png", units = "cm", width = 20, height = 16, dpi = 320)


# prediction cycle duration

grid = expand.grid(c("S1"),
                   c(0.05),
                   c("Midslope"),
                   c("Medium"),
                   c("1"),
                   c("0"),
                   c("3"),
                   c(300, 600),
                   c(18000),
                   c("0"),
                   c("0"),
                   c("0"),
                   c("0"),
                   c("0"),
                   c("0"),
                   c("2"),
                   stringsAsFactors = F)

names(grid) = c("season", "field_size_estimate_ha", "slope_position", "slope_steepness",
                "soil_depth", "terrace", "fertile_degree", "cycle_duration", 
                "plant_density_pl_ha", "previous_cas", "previous_crl", "previous_leg",
                "fert_use", "compost_manure", "pesticide", "times_weeded")

grid = merge(grid, sr)

pred_grid = predict(object = train_rf, newdata = grid)

pred_grid_df = as.data.frame(pred_grid)

pred_grid_df = cbind(grid, pred_grid_df)

cycle300 =  pred_grid_df[which(pred_grid_df$cycle_duration == 300), ]
cycle600 =  pred_grid_df[which(pred_grid_df$cycle_duration == 600), ]

cycle_diff = merge(cycle300, cycle600, by = c("x", "y",
                                              "elevation", "soc", "sand", "cec", "ph", "acid_exch", "prec_a", "prec_b",
                                              "tmin_a", "tmin_b", "tmax_a", "tmax_b", "tavg_a", "tavg_b", "srad_a", "srad_b",
                                              "pop", "travel",
                                              "season", "field_size_estimate_ha", "slope_position", "slope_steepness",
                                              "soil_depth", "terrace", "fertile_degree",  
                                              "plant_density_pl_ha", "previous_cas", "previous_crl", "previous_leg",
                                              "fert_use", "compost_manure", "pesticide", "times_weeded"))


cycle_diff$cycle_diff = cycle_diff$pred_grid.y - cycle_diff$pred_grid.x

cycle_diff_sf = st_as_sf(cycle_diff, coords = c("x", "y"), crs ="+proj=longlat + datum = WGS84 + no_defs")
cycle_diff_sf = as(cycle_diff_sf, "Spatial")
cycle_diff_sf = vect(cycle_diff_sf)
cycle_diff_sf = project(cycle_diff_sf, "EPSG:4326")

ggplot() + theme_bw() +
  ggtitle("Additional yield with a cycle duration of 600 vs 300 days") +
  geom_spatvector(data = rwa0, fill = "grey90", linewidth = 0.5, color = "black") +
  geom_spatvector(data = cycle_diff_sf, shape = 16, aes(color = cycle_diff), alpha = 0.5, size = 2) +
  geom_spatvector(data = rwa1, fill = NA, linewidth = 0.5, color = "black") +
  scale_color_viridis_c() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title = element_blank())

ggsave("Maps\\Predictions\\Cycle.png", units = "cm", width = 20, height = 16, dpi = 320)


# prediction use of season

grid = expand.grid(c("S1", "S2"),
                   c(0.05),
                   c("Midslope"),
                   c("Medium"),
                   c("1"),
                   c("0"),
                   c("3"),
                   c(450),
                   c(18000),
                   c("0"),
                   c("0"),
                   c("0"),
                   c("0"),
                   c("0"),
                   c("0"),
                   c("2"),
                   stringsAsFactors = F)

names(grid) = c("season", "field_size_estimate_ha", "slope_position", "slope_steepness",
                "soil_depth", "terrace", "fertile_degree", "cycle_duration", 
                "plant_density_pl_ha", "previous_cas", "previous_crl", "previous_leg",
                "fert_use", "compost_manure", "pesticide", "times_weeded")

grid = merge(grid, sr)

pred_grid = predict(object = train_rf, newdata = grid)

pred_grid_df = as.data.frame(pred_grid)

pred_grid_df = cbind(grid, pred_grid_df)

seas1 =  pred_grid_df[which(pred_grid_df$season == "S1"), ]
seas2 =  pred_grid_df[which(pred_grid_df$season == "S2"), ]

seas_diff = merge(seas1, seas2, by = c("x", "y",
                                       "elevation", "soc", "sand", "cec", "ph", "acid_exch", "prec_a", "prec_b",
                                       "tmin_a", "tmin_b", "tmax_a", "tmax_b", "tavg_a", "tavg_b", "srad_a", "srad_b",
                                       "pop", "travel",
                                       "field_size_estimate_ha", "slope_position", "slope_steepness",
                                       "soil_depth", "terrace", "fertile_degree", "cycle_duration",
                                       "fert_use", "compost_manure", "pesticide", "times_weeded"))


seas_diff$seas_diff = seas_diff$pred_grid.x - seas_diff$pred_grid.y

seas_diff_sf = st_as_sf(seas_diff, coords = c("x", "y"), crs ="+proj=longlat + datum = WGS84 + no_defs")
seas_diff_sf = as(seas_diff_sf, "Spatial")
seas_diff_sf = vect(seas_diff_sf)
seas_diff_sf = project(seas_diff_sf, "EPSG:4326")

ggplot() + theme_bw() +
  ggtitle("Additional yield in Season 1 vs Season 2") +
  geom_spatvector(data = rwa0, fill = "grey90", linewidth = 0.5, color = "black") +
  geom_spatvector(data = seas_diff_sf, shape = 16, aes(color = seas_diff), alpha = 0.5, size = 2) +
  geom_spatvector(data = rwa1, fill = NA, linewidth = 0.5, color = "black") +
  scale_color_viridis_c() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title = element_blank())

ggsave("Maps\\Predictions\\Season.png", units = "cm", width = 20, height = 16, dpi = 320)


