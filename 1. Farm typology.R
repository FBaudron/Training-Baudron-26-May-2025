#' ---
#' title: "Farm typology - Northern Zimbabwe"
#' author: "Frédéric Baudron"
#' date: "May 26th, 2025"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list = ls())


# INSTALLING NECESSARY PACKAGES-------------------------------------------------

if (!require('openxlsx')) install.packages("openxlsx")
if (!require('ade4')) install.packages("ade4")
if (!require('ggplot2')) install.packages("ggplot2")
if (!require('fastcluster')) install.packages("fastcluster")
if (!require('factoextra')) install.packages("factoextra")
if (!require('randomForest')) install.packages("randomForest")
if (!require('ggplot2')) install.packages("ggplot2")


# LOADING NECESSARY PACKAGES----------------------------------------------------

library(openxlsx)
library(ade4)
library(ggplot2)
library(fastcluster)
library(factoextra)
library(randomForest)
library(ggplot2)


# SETTING UP THE DIRECTORY & LOADING THE DATA-----------------------------------

setwd("D:\\Mes Donnees\\1. Cirad\\UM6P\\Training\\")

data = read.xlsx("Data\\2. Datasets\\Data for typo.xlsx", sheet = 1)


# DATA MANIPULATION-------------------------------------------------------------

# remove outliers

boxplot(data$age_hhh)
boxplot(data$family_size)
data = data[data$family_size < 20,]
boxplot(data$family_size)
boxplot(data$cropped_area)
data = data[data$cropped_area < 15,]
boxplot(data$cropped_area)
boxplot(data$fallow)
boxplot(data$prop_non_cereals)
boxplot(data$fertilizers)
boxplot(data$Manure)
boxplot(data$Compost)
boxplot(data$cattle)
boxplot(data$small_rum)
data = data[data$small_rum < 100,]
boxplot(data$small_rum)
boxplot(data$poultry)
data = data[data$poultry < 300,]
boxplot(data$poultry)
boxplot(data$food_security)
boxplot(data$hdds)
boxplot(data$tot_div)
boxplot(data$cereals)
boxplot(data$offtake)
data = data[data$offtake < 6,]
boxplot(data$offtake)
boxplot(data$eq_value)
data = data[data$eq_value < 8000,]
boxplot(data$eq_value)


# check that the data is roughly normally distributed, log-transform otherwise

typo = data # copy of your original data before transformation

hist(typo$age_hhh)
hist(typo$family_size)
hist(typo$cropped_area)
# typo$cropped_area = log10(typo$cropped_area+(0.5 * min(typo$cropped_area[typo$cropped_area > 0])))
hist(typo$cropped_area)
hist(typo$fallow)
# typo$fallow = log10(typo$fallow+(0.5 * min(typo$fallow[typo$fallow > 0])))
hist(typo$fallow)
hist(typo$prop_non_cereals)
hist(typo$fertilizers)
# typo$fertilizers = log10(typo$fertilizers+(0.5 * min(typo$fertilizers[typo$fertilizers > 0])))
hist(typo$fertilizers)
hist(typo$Manure)
# typo$Manure = log10(typo$Manure+(0.5 * min(typo$Manure[typo$Manure > 0])))
hist(typo$Manure)
hist(typo$Compost)
# typo$Compost = log10(typo$Compost+(0.5 * min(typo$Compost[typo$Compost > 0])))
hist(typo$Compost)
hist(typo$cattle)
# typo$cattle = log10(typo$cattle+(0.5 * min(typo$cattle[typo$cattle > 0])))
hist(typo$cattle)
hist(typo$small_rum)
# typo$small_rum = log10(typo$small_rum+(0.5 * min(typo$small_rum[typo$small_rum > 0])))
hist(typo$small_rum)
hist(typo$poultry)
# typo$poultry = log10(typo$poultry+(0.5 * min(typo$poultry[typo$poultry > 0])))
hist(typo$poultry)
hist(typo$food_security)
hist(typo$hdds)
hist(typo$tot_div)
hist(typo$cereals)
# typo$cereals = log10(typo$cereals+(0.5 * min(typo$cereals[typo$cereals > 0])))
hist(typo$cereals)
hist(typo$offtake)
# typo$offtake = log10(typo$offtake+(0.5 * min(typo$offtake[typo$offtake > 0])))
hist(typo$offtake)
hist(typo$eq_value)
# typo$eq_value = log10(typo$eq_value+(0.5 * min(typo$eq_value[typo$eq_value > 0])))
hist(typo$eq_value)


# PCA (REDUCTION OF THE DIMENSIONALITY OF THE DATASET) & HCA--------------------

typo[, -c(1)] = scale(typo[, -c(1)]) # scaling is always a good idea when using variables that may be on very different scales

delta.pca = dudi.pca(typo[, -c(1)])
delta.pca = dudi.pca(df = typo[, -c(1)], scannf = FALSE, nf = 5)

delta.pca$eig
cumsum(delta.pca$eig) / sum(delta.pca$eig)   # cumulated percentage of variability explained by the PC
delta.pca$co  # correlation coefficients between the PCs and the variables


fviz_pca_var(delta.pca, col.var = "contrib") + 
  scale_color_gradient2(low = "blue", mid = "purple", high = "red", midpoint = 5) +
  theme_minimal()

# plotting all points on the PC1-PC2 plane and checking for possible outliers
s.label(delta.pca$li, xax = 1, yax = 2)
scatter(delta.pca)


# applying the hierarchical clustering on the PCs
delta.cah = hclust(dist(delta.pca$li), method = "ward.D")
hcd = as.dendrogram(delta.cah)
plot(hcd, type = "rectangle", ylab = "Height", leaflab = "none")


# identifying types 
delta.type = cutree(delta.cah, k = 4)


# plot clusters on the PC1-PC2 plane
s.class(delta.pca$li, fac = as.factor(delta.type), col = c("black", "darkred", "orangered", "orange"))


# INTERPRETATION OF THE TYPOLOGY------------------------------------------------

# adding types to the dataset
data$Type = delta.type
data$Type = as.factor(data$Type)


# random forest to understand what variables are the most segregating 

rf = randomForest(as.factor(Type) ~ ., data[ ,-c(1)])
varImpPlot(rf)


# Plotting differences between types (e.g., age)

theme_set(theme_bw(base_size = 18))

ggplot(data, aes(x = Type, y = age_hhh)) + 
  geom_point(shape = 21, size = 3, position = position_jitter(width = 0.2, height = 0.2), color = "black", fill = "grey")+ 
  geom_boxplot(fill = "white", size = 0.9, width = 0.4, alpha = 0.5, outlier.shape = NA) + 
  theme(axis.text = element_text(size = 10, face = "bold"), axis.title = element_text(size = 10, face  = "bold")) + 
  ylab("Age of the head of the household") + xlab("Types") +
  scale_x_discrete(labels = c('T1', 'T2', 'T3', 'T4'))


# distribution of farm types

summary(data$Type)

