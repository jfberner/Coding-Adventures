# --------------------------------------------------#
# South America Endangered Species Richness Map
# 
# In this script we'll make a map of endangered or threathened 
# species in South America.
# 
# First version: 26.Sep.2023
# Author: João Frederico Berner
# --------------------------------------------------#

# Libs #####
library(tidyverse)
library(spData)
library(sf)
library(terra)

# Data #####
sa_poly <- st_read('data/raw/shapefiles/SouthAmerica_Adm2.shp') %>% st_transform(crs = st_crs(4326))

# Species shapefiles downloaded from iucnredlist.org
# Advanced Search>South America>EX+EW+CR+EN+VU>Terrestrial habitats only
species <- read_sf('data/raw/shapefiles/southAmerica_IUCNspecies/data_0.shp') %>% 
  st_transform(crs = st_crs(4326)) # %>% sf::st_make_valid() # %>% st_crop(sa_poly)

# Some geometries are "broken" -i.e. cross themselves- so exclude them
valid_geometries <- st_is_valid(species)
valid_species <- species[valid_geometries, ] #drop non-valid species' geometries


# Create an empty raster with ~500x500m resolution
rast <- terra::rast(crs = 'EPSG:4326', extent = ext(sa_poly), resolution = 0.003898745, vals = 0) 


# Endangered Species Density Map ####
# Create a list to store individual species rasters
species_rasters <- list()

# Loop through each species geometry and rasterize it
for (i in 1:dim(species)[1]) {
species_raster <- rasterize(valid_species[i,], rast, background = 0)
  species_rasters[[i]] <- species_raster
}

# Create an empty raster with the same dimensions as the template
sum_raster <- rast
values(sum_raster) <- 0

# Loop through the list of species rasters and summarize them
for (i in 1:length(species_rasters)) {
sum_raster <- sum_raster + species_rasters[[i]]
}

beepr::beep(8)

if(!dir.exists('data/processed/rasters/endangered_species/')) dir.create('data/processed/rasters/endangered_species/',recursive = T)
terra::writeRaster(sum_raster, 'data/processed/rasters/endangered_species/endangered_species_southAmerica.tif')

beepr::beep(8)