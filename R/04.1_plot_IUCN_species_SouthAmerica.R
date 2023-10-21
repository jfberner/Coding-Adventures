# --------------------------------------------------#
# Rayshaded Map of Endangered Species in South America
# 
# 
# In this script we'll take the raster built in script 04.0
# and plot it as a relief map using rayshader, except the 
# height will be defined by the number of IUCN listed species
# in each cell.
# 
# First version: 30.Sep.2023
# Author: João Frederico Berner
# --------------------------------------------------#

# Libs #####
library(tidyverse) # %>%
library(terra) # rast mask crop ext
library(sf) # read_sf 
library(tidyterra) # geom_spatraster aes
# library(rayshader) # Didn't work out
library(MetBrewer) # met.brewer
library(colorspace) # swatchplot
library(ggtext) # element_markdown geom_richtext
library(png) # readPNG
library(cowplot) 


# Data #####
raster <- terra::rast('data/processed/rasters/endangered_species/endangered_species_southAmerica.tif')

sa_poly <- sf::read_sf('data/raw/shapefiles/SouthAmerica_Adm2.shp')

raster_sa <- raster %>% terra::mask(sa_poly, touches = T)

raster_sa_landOnly <- raster_sa %>% crop(ext(-95.097656,-30.058594,-56.656226,14.774883))

# create color palette

c1 <- met.brewer("Hokusai1", type = "continuous", direction = -1)
swatchplot(c1)

texture <- grDevices::colorRampPalette(c1, bias = 1.5)(256)
swatchplot(texture)


# Plot #####
## GGplot approach ####
ggplot() +
  geom_spatraster(data = raster_sa, maxcell = 90000000,
                  na.rm = T
                  ) + 
  theme_void()+
  # scale_fill_whitebox_c(
  #   palette = "viridi", direction = 1,
  #   n.breaks = 5) +
  scale_fill_gradientn(colors = texture, na.value = 'black')+
  labs(fill = '',
       title = "",
       caption = 'Author: João Frederico Berner \n Source: IUCN Red List - on Sptember 27th, 2023',
       tag = "**iucn**<span style = 'color:tomato'>**RedListed**</span>**species**",
       subtitle = 'In South America') + 
  theme(text = element_text(family = 'Engebrechtre'),
        legend.position = c(0.82, 0.03),
        legend.direction = 'horizontal',
        legend.title.align = 0.45,
        plot.caption = element_text(family = 'Arial',face = 'plain', size = 40, color = "grey75", hjust = 0.5, vjust = -40),
        plot.margin = unit(c(t = 0, r = 0, b = 0, l = 0), "lines"),
        plot.background = element_rect(fill = "black", color = NA),
        panel.background = element_rect(fill = "black", color = NA),
        legend.text = element_text(colour = 'white', size = 80), 
        legend.title = element_text(colour = 'white', size = 80),
        plot.tag.position = c(0.7,0.8),
        plot.tag = element_markdown(colour = 'white', size = 175, face = 'plain',
                                    vjust = -3, hjust = 0.448),
        plot.subtitle = element_text(colour = 'white', size = 80, face = 'plain',
                                     vjust = 0, hjust = 0.935)) +
  guides(fill = guide_colorbar(
    label = T,
    label.theme = element_text(color = 'white', face = 'plain', size = 40),
    label.hjust = 0.5, 
    title = 'Number of specieS',
    title.position = 'top',
    barwidth = unit(600,'points'),
    barheight = unit(100,'points'),
    draw.ulim = T, 
    draw.llim = T,
    ticks = T, 
    ticks.colour = 'white',
    ticks.linewidth = 3)) +
  annotation_raster(readPNG('figs/input_iucn_categories.png'), 
                    xmin = -50.05,
                    xmax = -31.95,
                    ymin = -52.05,
                    ymax = -49.26) + 
  geom_richtext(aes(x = -41.25, y = -47.5,
                    label = 'Included categorieS'),
                colour = 'white',
                size = 28,
                label.colour = NA,
                fill = NA,
                family = 'Engebrechtre', 
                fontface = 'plain',
                show.legend = F
                
           ) -> gg
    

# gg
# Save the plot
tictoc::tic() ; ggsave('figs/04.1_IUCN_RedListed_SouthAmerica_90Mcell.svg',
                       plot = gg,
                       units = 'cm',
                       scale = 1,
                       width = 100,
                       height = 140,
                       limitsize = F,
                       dpi = 300) ; tictoc::toc() ; beepr::beep(8) ; gc()
# ## Rayshader 3d view #####
# # define aspect ratio based on bounding box
# 
# bb <- st_bbox(raster_sa)
# 
# bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |> 
#   st_sfc(crs = st_crs(raster_sa))
# 
# bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |> 
#   st_sfc(crs = st_crs(raster_sa))
# 
# 
# # check by plotting points
# # 
# # raster_sa |> 
# #   ggplot() +
# #   geom_sf() +
# #   geom_sf(data = bottom_left) +
# #   geom_sf(data = bottom_right, color = "red")
# 
# width <- st_distance(bottom_left, bottom_right)
# 
# top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |> 
#   st_sfc(crs = st_crs(raster_sa))
# 
# height <- st_distance(bottom_left, top_left)
# 
# # handle conditions of width or height being the longer side
# 
# if (width > height) {
#   w_ratio <- 1
#   h_ratio <- height / width
# } else {
#   h_ratio <- 1
#   w_ratio <- width / height
# }
# 
# # convert to matrix
# 
# mat <- matrix(raster_sa$lyr.1, 
#               nrow = nrow(raster_sa),
#               ncol = ncol(raster_sa))
# 
# 
# 
# # plot that 3d thing!
# 
# rgl::rgl.close()
# 
# mat |> 
#   height_shade(texture = texture) |> 
#   plot_3d(heightmap = mat,
#           zscale = 100 / 5,
#           solid = FALSE,
#           shadowdepth = 0)
# 
# render_camera(theta = -20, phi = 45, zoom = .8)
# 
# outfile <- "figs/IUCN_spDensity_SouthAmerica.png"
# 
# {
#   start_time <- Sys.time()
#   cat(crayon::cyan(start_time), "\n")
#   if (!file.exists(outfile)) {
#     png::writePNG(matrix(1), target = outfile)
#   }
#   render_highquality(
#     filename = outfile,
#     interactive = FALSE,
#     lightdirection = 280,
#     lightaltitude = c(20, 80),
#     lightcolor = c(c1[2], "white"),
#     lightintensity = c(600, 100),
#     samples = 450,
#     width = 60,
#     height = 60
#   )
#   end_time <- Sys.time()
#   diff <- end_time - start_time
#   cat(crayon::cyan(diff), "\n")
# }
