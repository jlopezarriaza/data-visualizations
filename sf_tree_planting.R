library(gganimate)
library(ggplot2)
`%>%` <- magrittr::`%>%`

sf.tree.data.location <- 'https://data.sfgov.org/resource/337t-q2b4.json'
sf.tree.data.df <- RSocrata::read.socrata(sf.tree.data.location)

filtered.tree.data <- sf.tree.data.df %>%
  dplyr::mutate(latitude = as.numeric(latitude),
                longitude = as.numeric(longitude),
                year = as.integer(lubridate::year(plantdate))) %>%
  dplyr::filter(!is.na(latitude), !is.na(longitude), !is.na(plantdate)) %>%
  dplyr::filter(latitude < 40, latitude > 37.65)


filtered.tree.data %>%
  ggplot() +
  geom_point(aes(x = longitude, y = latitude),
             color = 'forestgreen',
             alpha = 0.1) + 
  theme(legend.position = "none")  +
  coord_quickmap()


bbox <- ggmap::make_bbox(lat = latitude,
                         lon =  longitude,
                         data = filtered.tree.data)
base_map <- ggmap::get_map(location = bbox,
                           source = "stamen",
                           crop = FALSE,
                           maptype = 'toner',
                           force = TRUE)
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
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


anim_save('~/Desktop/test-animation.gif' , anim, fps = 5, start_pause = 1, end_pause = 25)
