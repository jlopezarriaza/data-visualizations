# Description: Taking a look at the trees planted in SF from DataSF.
# Date: November 10, 2019
# Author: Juan Lopez Arriaza
library(gganimate)
library(ggplot2)
`%>%` <- magrittr::`%>%`

# Source: https://data.sfgov.org/City-Infrastructure/Street-Tree-List/tkzw-k3nq
sf.tree.data.location <- 'https://data.sfgov.org/resource/tkzw-k3nq.json'
sf.tree.data.df <- RSocrata::read.socrata(sf.tree.data.location)
# Cleaning some of the data, the longitude and latitude columns come as strings, turning them into 
#   numeric values, getting the planting year, and dropping any columns that don't have a latitude,
#   longitude, and planting year associated with them.
filtered.tree.data <- sf.tree.data.df %>%
  dplyr::mutate(latitude = as.numeric(latitude),
                longitude = as.numeric(longitude),
                year = as.integer(lubridate::year(plantdate))) %>%
  dplyr::filter(!is.na(latitude), !is.na(longitude), !is.na(plantdate)) %>%
  dplyr::filter(latitude < 40, latitude > 37.65)
# --Creating the visualizations--
# Defining a base map onto which we can overlay our other visualizatoins.
bbox <- ggmap::make_bbox(lat = latitude,
                         lon =  longitude,
                         data = filtered.tree.data)
base_map <- ggmap::get_map(location = bbox,
                           source = "stamen",
                           crop = FALSE,
                           maptype = 'toner',
                           force = TRUE)

#  Plotting all of the trees on a single map.
all_trees_plot <- ggmap::ggmap(base_map) + 
  geom_point(aes(x = longitude, y = latitude),
             color = 'forestgreen',
             alpha = 0.1,
             size = 0.1,
             data = filtered.tree.data) + 
  labs(title= 'All Tress') +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 
# Creating an animation that plots the trees planted in a given year. The result is at the end of 
#  the animation we end up with a cumulative map of all trees planted
anim = ggmap::ggmap(base_map) +
  geom_point(aes(x = longitude,
                 y = latitude,
                 group = seq_along(year)),
             alpha = 0.25,
             size = 0.1,
             color = 'forestgreen',
             data = filtered.tree.data) +
  labs(title = 'Year: {frame_along}') +
  transition_reveal(year) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Save the animation

anim_save('results/sf_tree_planting/planted_trees.gif' , anim, fps = 5, start_pause = 1, end_pause = 25)
