# --------------------------------------------------#
# River Synonyms Map of Brazil
# 
# Partially built following Milos' Tutorial: 
# https://www.youtube.com/watch?v=HugGwjogPv0
# 
# In this script we'll build a map highlighting the rivers in Brazil that have
# homonyms - just out of sheer curiosity :)
# 
# First version: Sep 20 2023
# Author: João Frederico Berner
# --------------------------------------------------#


# Libs #####
library(tidyverse)
library(sf)
library(giscoR)
# Data #####

# Brazil National Boundaries
br_poly <- giscoR::gisco_get_countries(
  resolution = '3',
  country = 'BR'
)

# River Shapefile Data
# From https://metadados.snirh.gov.br/geonetwork/srv/api/records/a01764d3-4742-4f7d-b867-01bf544dde6d

river_poly <- sf::read_sf('data/raw/shapefiles/Brazil_rivers/GEOFT_BHO_REF_RIO/GEOFT_BHO_REF_RIO.shp') %>% 
  sf::st_transform(crs = st_crs(br_poly)) %>% 
  sf::st_intersection(br_poly)

# Count the number of times each name appears:
river_data <- river_poly %>%
  group_by(NORIOCOMP) %>%
  mutate(repeat_count = n()) %>%
  ungroup()

# Plot #####

# Define custom color scale


# Create a variable for line width based on color category
river_data$line_width <- ifelse(
  river_data$repeat_count < 10, 0.1,
  ifelse(river_data$repeat_count < 50, 0.1,
         ifelse(river_data$repeat_count < 80, 1,
                ifelse(river_data$repeat_count < 115, 1.5,
                       ifelse(river_data$repeat_count < max(river_data$repeat_count), 3, 0.1)))))

# Get top 10 river names
top_ten_rivers <- river_data %>%
  filter(NORIOCOMP != 'NA') %>% 
  distinct(NORIOCOMP, .keep_all = TRUE) %>%
  group_by(NORIOCOMP) %>%
  arrange(desc(repeat_count)) %>%
  head(10)

# The actual Plot
ggplot()+
  geom_sf(data = river_data, color = "grey20", size = 0.1) + # Grey lines
  geom_sf(data = river_data %>% filter(NORIOCOMP != 'NA'), 
          mapping = aes(color=repeat_count, size = line_width)) +
  scale_color_gradientn(name = "Homonym Count",
                        colours = c("grey20", "green", "yellow", "red"),
                        breaks = c(0, 50, 80, 115, 158),
                        limits = c(0, 158)) +
  scale_size_continuous(guide = "none") + # Remove legend for line width
  # scale_alpha(range = c(.01, .7)) +
  theme_void() +
  theme(legend.position = c(0.8,0.18),
        # legend.justification = 'bottom',
        plot.caption = element_text(size = 9, color = "grey60",
                                    hjust = .1, vjust = 10),
        plot.margin = unit(c(t = 0, 
                             r = 0,
                             b = 0, 
                             l = 0),
                           "lines"),
        plot.background = element_rect(fill = "black",
                                       color = NA),
        panel.background = element_rect(fill = "black",
                                        color = NA)) +
  labs(title = "",
       x = "",
       y = "",
     caption = "Source: Agência Nacional de Águas (ANA) #a01764d3-4742-4f7d-b867-01bf544dde6d "
  ) +
  guides(color = guide_colorbar(
            title = "Homonym Number",
            title.theme = element_text(color = 'white', family = 'Engebrechtre', face = 'bold'), 
            title.vjust = 1.1,
            barwidth = grid::unit(10, 'points'), 
            title.position = "top", 
            label = T,
            label.position = "right",
            label.theme = element_text(color = 'white', family = 'Engebrechtre'),
            label.hjust = 1.2, 
            draw.ulim = T, 
            draw.llim = T,
            ticks = T, 
            ticks.color = 'white',
            ticks.linewidth = 1,
            direction = "vertical")) +
  geom_text(
    data = top_ten_rivers,
    aes(x = -70, y = -20:-29, 
        label = paste0(NORIOCOMP, " (", repeat_count, ")")),
    size = 5,
    color = "white",
    hjust = 0,  # Horizontal justification (left-aligned)
    vjust = 1.2,  # Vertical justification (adjust to your preference)
    family = 'Engebrechtre')+
  geom_text(
    aes(x = -70, y = -18.7, label = 'Top Ten River Names'),
    size = 6.5,
    color = "white",
    hjust = 0,  # Horizontal justification (left-aligned)
    vjust = 1.2,  # Vertical justification (adjust to your preference)
    family = 'Engebrechtre',
    fontface = 'bold') + 
  geom_text(
    aes(x = -40, y = 6, label = 'Homonym Rivers \n of Brazil'), 
    size  = 10,
    color = 'white',
    hjust = 0.5,
    vjust = 2.2,
    family = 'Engebrechtre',
    fontface = 'bold'
  ) -> p
  

ggsave(p, filename = 'figs/river_homonyms.png', width = 3200, height = 3200, units = 'px')

