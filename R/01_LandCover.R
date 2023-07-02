# --------------------------------------------------#
# Zonal Statistics with R
# Following Milos Makes Maps TUtorial
# 
# First version: 18.jun.2023
# Author: João Frederico Berner
# --------------------------------------------------#
# Set up the new proj
source('~/Documents/Rstats/SetupProj.R')

# Create shapefiles folder
if (!dir.exists('data/raw/shapefiles')) dir.create('data/raw/shapefiles')

# Create rasters folder
if (!dir.exists('data/raw/rasters')) dir.create('data/raw/rasters')
# The entirety of this folder won't be uploaded to github. 
# Download time was 2h15min, so you may guess why.

# Libs #####
# remotes::install_github('ropensci/MODIStsp') # MODIS data in raster or df
# remotes::install_github('dickoa/rgeoboundaries') # country boundaries
# remotes::install_github('isciences/exactextractr') # zonal stats
library(MODIStsp)
library(rgeoboundaries)
library(exactextractr)
library(tidyverse)
library(sf)
library(terra)
library(httr)
library(viridis)
# Data #####
## Geometries ####
### South America Political #### 
sa_poly <- rgeoboundaries::gb_adm2(
  c('Argentina','Bolivia','Brazil','Chile',"Colombia","Ecuador",'Guyana','Paraguay','Peru','Suriname','Uruguay','Venezuela'), 
  type = 'SSCGS') # regional boundaries (level 2)

# Save the shapefile
sf::st_write(sa_poly, 'data/raw/shapefiles/SouthAmerica_Adm2.shp')

#### Geometry Manipulation #####
# Create an ID column, assigning an ordered ID number to each geometry
sa_poly$id <- 1:max(nrow(sa_poly))

### South America Ecoregions #####
# Get the shapefile for south american ecoregions. Source: http://ecologicalregions.info/htm/sa_eco.htm
download.file('http://ecologicalregions.info/data/sa/sa_eco_l3.zip',destfile = 'data/raw/shapefiles/ecoRegions.zip')

unzip('data/raw/shapefiles/ecoRegions.zip', exdir = 'data/raw/shapefiles/ecoRegions')

sa_eco_poly <- read_sf('data/raw/shapefiles/ecoRegions/sa_eco_l3.shp') %>% # read it
               group_by(LEVEL2) %>% 
               summarize(geometry = st_union(geometry)) %>% st_transform(crs = 4326)

sa_eco_poly$LEVEL2 <- as_factor(sa_eco_poly$LEVEL2)

sa_eco_poly <- sf::st_crop(sa_eco_poly,sa_poly)

## MODIS Land Cover Data ####
# Pick your layers at https://modis.gsfc.nasa.gov/data/dataprod/
# We're using this one: https://modis.gsfc.nasa.gov/data/dataprod/mod12.php
# And this version of it: https://lpdaac.usgs.gov/products/mcd12q1v006/
# And the LC_Type1 layer

# Checking the documentation, we can see what the names of the classes are: 
# https://ladsweb.modaps.eosdis.nasa.gov/filespec/MODIS/6/MCD12Q1

# Milos' tutorial will use httr to scrape the documentation page to get the
# classes names - which is awesome. For that, registering on the MODIS website
# is necessary.

# Get the product and layer names from te code in the second to last link above
MODIStsp::MODIStsp_get_prodlayers("MCD12Q1")

# Get the rasters
MODIStsp(gui = F,
         out_folder = "data/raw/rasters",
         out_folder_mod = "data/raw/rasters",
         selprod = "LandCover_Type_Yearly_500m (MCD12Q1)", # from running the last command
         bandsel = "LC1",
         user = "jfberner",
         password = password, # I created this outside of the script - provide your own :)
         start_date = "2020.01.01", # latest at this date
         end_date = '2020.12.31',
         spatmeth = 'file',
         spafile = 'data/raw/shapefiles/SouthAmerica_Adm2.shp', # where you saved the shp
         out_format = "GTiff")

# Load raster to env
sa_lc <- terra::rast('data/raw/rasters/SouthAmerica_Adm2/LandCover_Type_Yearly_500m_v6/LC1/MCD12Q1_LC1_2020_001.tif',) %>% terra::project("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Zonal Statistics - Single Class #####
# Get the raster data for each polygon in a tibble

## Main Land Use #####
# Get the category of the main land cover of each geometry, using mode function
sa_lc_mode <- exact_extract(sa_lc, sa_poly, "mode", append_cols = 'id') %>% 
  inner_join(sa_poly, by = 'id') %>% 
  st_as_sf()

map_mode <- ggplot() +
  geom_sf(data = sa_lc_mode,
          aes(fill = factor(mode)),
          lwd = 0) +
  guides(
    fill = guide_legend(
      direction = "vertical",
      keywidth = unit(5, "mm"),
      keyheight = unit(5, "mm"),
      title.position = "top",
      label.position = "right",
      title.hjust = .5,
      label.hjust = 0,
      ncol = 1,
      byrow = F
    )
  ) +
  labs(
    title = "Land cover - South America",
    caption = "Data: MODIS Land Cover Type Annual L3 Global 500m",
    x = "",
    y = ""
  ) +
  theme_void() +
  theme(
    legend.position = "left",
    plot.title = element_text(
      size = 22, color = "grey10",
      hjust = .5, vjust = 1 
    ),
    plot.caption = element_text(
      size = 10, color = "grey60",
      hjust = .5, vjust = -3 
    ),
    legend.title = element_text(
      size = 12, color = "grey10"
    ),
    legend.text = element_text(
      size = 11, color = "grey10"
    ),
    plot.margin = unit(c(t = 0, b = 0, r = 0, l = 5),
                       "lines"
    )
  )

print(map_mode)
# plotly::ggplotly(map_mode)


## Percentage Land Cover #####
# What's the percentage of cropland in each polygon?
sa_cropland <- exact_extract(sa_lc, #raster
                             sa_poly, #polygons
                             function(value, fraction){ 
                              100 * sum(fraction[value == 12]) / sum(fraction) # get the percentage
                             }
                             )

# Which outputs a vector, so we'll have to manipulate the data before joining it
# with the sf (polygon) object.
sa_cropland_sf <- as.data.frame(t(sa_cropland)) %>% # First, turn it into a df
                  pivot_longer(cols = 1:length(sa_cropland), names_to = 'id', values_to = 'value') %>% # wide to long format
                  cbind(sa_poly) %>% st_as_sf()

# And Plot it!
map_pctg <- ggplot() +
  geom_sf(data = sa_cropland_sf,
          aes(fill = value),
          lwd = 0) +
  scale_fill_viridis(
    name = "% of fractional cover area",
    limits = c(0, 100),
    discrete = F) +
  guides(fill = guide_legend(
         direction = "horizontal",
         keywidth = unit(10, "mm"),
         keyheight = unit(2.5, "mm"),
         title.position = "top",
         label.position = "bottom",
         title.hjust = .5,
         label.hjust = .5,
         nrow = 1,
         byrow = T)
        ) +
  geom_sf(data = sa_eco_poly, 
          aes(fill = LEVEL1, alpha = 1), 
          fill = NA, 
          color = 'white', 
          size = 0.0125,
          show.legend = F) +
  labs(title = "Croplands - South America",
       caption = "Data: MODIS Land Cover Type Annual L3 Global 500m \n Ecoregions: Ecological Classification of the Western Hemisphere (Griffith et al., 1998)",
       x = "",
       y = "") +
  theme_void() +
  theme(legend.position = "top",
        plot.title = element_text(size = 22, color = "grey10",
                                  hjust = .5, vjust = 1),
        plot.caption = element_text(size = 10, color = "grey60",
                                    hjust = .5, vjust = 10),
        legend.title = element_text(size = 12, color = "grey10"),
        legend.text = element_text(size = 11, color = "grey10"),
        plot.margin = unit(c(t = 1, b = 0, r = 0, l = 0),
                           "lines")
        )

plot(map_pctg)

 #  #####
legend_plot <- get_legend(map_pctg)

# Remove the legend from the main plot
map_pctg <- map_pctg + theme(legend.position = "none")

# Combine the main plot and the legend plot
combined_plot <- plot_grid(legend_plot, map_pctg, ncol = 2, align = "v", rel_widths = c(0.2, 0.8))

# Set the title alignment to the left
combined_plot <- ggdraw(combined_plot) +
  draw_label("Croplands - South America",
             x = 0.05, y = 0.98, hjust = 0, vjust = 1,
             fontface = "bold", size = 14)
