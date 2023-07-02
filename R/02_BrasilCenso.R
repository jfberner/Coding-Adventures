# --------------------------------------------------#
# Fun with Brazil Census 2022
# 
# Explore and Visualize Brazil Census 2022
# 
# First version: Jul.2.2023
# Author: João Frederico Berner
# --------------------------------------------------#

# Libs #####
library(tidyverse)
library(geobr)
library(sf)
library(colorspace)
library(scales)
# Data #####
data <- read_csv('data/raw/@gzanlorenssi_populacao_censo_desde_1872 - pop_municipios_wide.csv') %>% 
  rename("code_muni" = codigo_ibge7)

city <- geobr::read_municipality(code_muni = "all") %>% 
  rename("municipio" =  name_muni)

states <- geobr::read_state(code_state = 'all')

dataset <- full_join(data, city, by = c("code_muni", "municipio")) 

# Map Fun :) #####
## Population Difference 2022 - 2010 
d_popDiff <- dataset %>% 
  mutate(popDiff = as.numeric(`2022`) - as.numeric(`2010`)) %>% 
  # filter(popDiff != "NA") %>% 
  st_as_sf()

estado <- "PR"

map_2022_2010_diff <- ggplot(d_popDiff %>% filter(uf == estado)) +
  geom_sf(aes(fill = popDiff),
          lwd = 0) +
  scale_fill_continuous_diverging(palette = "Berlin", 
                       na.value = "grey1", 
                       mid = seq(min(subset(d_popDiff, uf == estado, select = popDiff)[[1]], na.rm = T),
                                 max(subset(d_popDiff, uf == estado, select = popDiff)[[1]], na.rm = T),
                                 length.out = 7)[[2]],
                       breaks = seq(min(subset(d_popDiff, uf == estado, select = popDiff)[[1]], na.rm = T),
                                    max(subset(d_popDiff, uf == estado, select = popDiff)[[1]], na.rm = T),
                                    length.out = 7),
                       labels = comma) + 
  geom_sf(data = states %>% filter(abbrev_state == estado),
          aes(color = abbrev_state),
          fill = NA,
          color = 'white',
          show.legend = F) +
  guides(fill = guide_legend(
    direction = "horizontal",
    keywidth = unit(15, "mm"),
    keyheight = unit(2.5, "mm"),
    title.position = "top",
    label.position = "bottom",
    # title.hjust = .3,
    # label.hjust = .5,
    nrow = 1,
    byrow = T)
  ) +
  labs(title = "Diferença Populacional",
       subtitle = paste("Municípios de", estado, "entre 2022 e 2010"),
       caption = "Dados compilados por: @gzanlorenssi\n Mapa por: @bernerfrederico",
       fill = "Valores absolutos",
       x = "",
       y = "") +
  theme_void() +
  theme(legend.position = "top",
        plot.title = element_text(size = 22, color = "grey90",
                                  hjust = .5, vjust = 1),
        plot.subtitle = element_text(size = 15, color = "grey90",
                                  hjust = .5, vjust = 1),
        plot.caption = element_text(size = 10, color = "grey40",
                                    hjust = .5, vjust = 10),
        legend.title = element_text(size = 10, color = "grey90", hjust = 0.5),
        legend.text = element_text(size = 9, color = "grey90", hjust = 0.5),
        plot.margin = unit(c(t = 1, b = 0, r = 0, l = 0),
                           "lines"),
        plot.background = element_rect(fill = "black")
  )

map_2022_2010_diff

