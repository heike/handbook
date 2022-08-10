#Load the packages
library(tidyverse)
library(ggplot2)
library(gganimate)
# for maps
library(geojsonio)
library(RColorBrewer)
library(sp)
library(broom)
library(rgeos)


hexmap <- "https://raw.githubusercontent.com/holtzy/D3-graph-gallery/master/DATA/us_states_hexgrid.geojson.json"

#Import hexbins
hex <- geojson_read(hexmap, what = "sp") #where "sp" is spatial class

saveRDS(hex,"hex-map.rds")

#Reformat the 'google_name' field
#This will remove the (United States) from each value
#E.g., Vermont (United States) will be changed to Vermont
hex@data = hex@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))

#Fortify the data to create a data format output
#This format is needed to plot the map using the ggplot2 package
hex_fortify <- tidy(hex, region = "google_name")

hex_one <- hex_fortify %>% group_by(id, piece, hole, group) %>% tidyr::nest() %>%
  rename(hexagon = data) %>% ungroup()

hex_one <- hex_one %>% mutate(
   group = sprintf("%s.%03d", id, piece)
)

# get map of states

theme_set(theme_minimal())
#devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr) # For map

states <- states %>% mutate(
  group = sprintf("%s.%03d", state_name, piece)
)
#states_sf <- get_urbn_map(map = "states", sf = TRUE)

states_one <- states %>% group_by(state_name, state_abbv, state_fips, piece, hole, group) %>% tidyr::nest() %>%
  rename(polygon = data) %>% ungroup()

statesmaps <- states_one %>% left_join(hex_one %>% select(id, hexagon), by=c("state_name"="id"))

statesmaps <- statesmaps %>%
  dplyr::select(state_name, state_abbv, state_fips, piece, hole, group, polygon, hexagon)

statesmaps <- statesmaps %>%
  mutate(
    hexagon_labels = hexagon %>% purrr::map(.f = function(d) {
      data.frame(long = mean(d$long[1:6]), lat = mean(d$lat[1:6]))
    })
  )

statesmaps <- statesmaps %>%
  mutate(
    piece = as.numeric(piece),
    group = factor(group),
    group = reorder(group, piece, FUN = function(x) mean(x)),
    group = factor(group, levels = rev(levels(group)))
  )


age10 <- get_decennial(geography = "state",
                       variables = "P013001",
                       year = 2010, geometry = TRUE)

age10 <- get_decennial(geography = "state",
                       variables = "P013001",
                       year = 2010, geometry = TRUE)
crs <- st_crs(age10)

age10 <- age10 %>%
  tigris::shift_geometry()

age10 <- age10 %>% st_transform(crs = crs)

library(sf)
age10$polygon_labels <- st_centroid(age10$geometry, of_largest_polygon = TRUE)


coords <- data.frame(state_name = age10$NAME, st_coordinates(age10$polygon_labels))
names(coords) <- c("state_name", "long", "lat")

coords_nest <- coords %>% nest(data = c(long, lat))
names(coords_nest) <- c("state_name", "polygon_labels")

statesmaps <- statesmaps %>% select(-polygon_labels)
statesmaps <- statesmaps %>% left_join(coords_nest, by = c("state_name"))


usethis::use_data(statesmaps, overwrite=TRUE)
#sf::st_centroid(states_sf$geometry)


mytheme <- theme(
  plot.caption = ggtext::element_textbox_simple(margin = ggplot2::margin(20, 0, 0, 0), size = rel(0.6), hjust = 0, color = "grey60"),
  plot.title = ggtext::element_textbox_simple(size = rel(1.12), margin = ggplot2::margin(5, 0, 5, 0)),
  plot.subtitle = ggtext::element_textbox_simple(size = rel(0.79), margin = ggplot2::margin(5, 20, 0, 0)),
  legend.title = element_text(size = rel(.95)),
  legend.text = element_text(size = rel(.75)))




# get data

library(tidycensus)
library(tidyverse)
census_key <- "7f784587c3918611ad6ca67188d9b269b3558dd4"
census_api_key(census_key, install=TRUE, overwrite = TRUE)

age10 <- get_decennial(geography = "state",
                       variables = "P013001",
                       year = 2010)
# saveRDS(age10, "age10.rds")
map_values <- statesmaps %>% left_join(age10 %>% select(-geometry,-polygon_labels), by = c("state_name" = "NAME"))

# library(tidycensus)
# library(tidyverse)
# options(tigris_use_cache = TRUE)
#
#
# age10 <- get_decennial(geography = "state",
#                        variables = "P013001",
#                        year = 2010, geometry = TRUE, shift_geo = TRUE)
# age10 %>% ggplot(aes(fill=variable)) + geom_sf()

map_values %>% unnest(col=hexagon) %>%
  ggplot(aes( x = long, y = lat, group = group, fill=value)) +
  geom_polygon() +
  #  geom_text (aes(label=id)) +
  theme_void () +
  coord_map () +
  scale_fill_gradient2("Median Age", midpoint=median(age10$value)) +
  geom_text(aes(label = state_abbv), data = map_values %>% unnest(col=hexagon_labels),
            hjust = 0.5, vjust = 0.5)

plotly::ggplotly()

map_values %>% unnest(col=polygon) %>%
  ggplot(aes(x = long, y = lat, group = group, fill=value, label=state_name)) +
  geom_polygon(colour="grey70") +
  #  geom_text (aes(label=id)) +
  theme_void () +
  scale_fill_gradient2("Median Age", midpoint=median(age10$value)) +
  coord_map() +
  geom_text(aes(label = state_abbv), data = map_values %>% filter(piece == 1) %>%
              unnest(col=polygon_labels), hjust = 0.5, vjust = 0.5)


plotly::ggplotly()

long_map <- map_values %>%
  pivot_longer(polygon:hexagon, values_to="data", names_to = "type") %>%
  tidyr::unnest(col = data)


p <- long_map %>%
  ggplot(aes(x = long, y = lat, group = group, fill=value)) +
  #  geom_polygon(colour="grey70") +
  geom_polygon(colour="grey70") +
  #  geom_text (aes(label=id)) +
  theme_void () +
  scale_fill_gradient2("Median Age", midpoint=median(age10$value)) +
  coord_map()

p <- p +
  geom_polygon(colour="grey70", data = long_map %>% filter(piece==1))


library(gganimate)

anim <- p +
  transition_states(type,
                    transition_length = 2,
                    state_length = 1)

anim

