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

#Reformat the 'google_name' field
#This will remove the (United States) from each value
#E.g., Vermont (United States) will be changed to Vermont
hex@data = hex@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))

#Fortify the data to create a data format output
#This format is needed to plot the map using the ggplot2 package
hex_fortify <- tidy(hex, region = "google_name")

hex_one <- hex_fortify %>% group_by(id, piece, hole, group) %>% tidyr::nest() %>%
  rename(hexagon = data) %>% ungroup()



# get map of states

theme_set(theme_minimal())
#devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr) # For map

states <- states %>% mutate(
  group = paste(state_name, piece, sep=".")
)
#states_sf <- get_urbn_map(map = "states", sf = TRUE)

states_one <- states %>% group_by(state_name, state_abbv, state_fips, piece, hole, group) %>% tidyr::nest() %>%
  rename(polygon = data) %>% ungroup()

statesmaps <- states_one %>% left_join(hex_one %>% select(id, hexagon), by=c("state_name"="id"))

statesmaps <- statesmaps %>%
  dplyr::select(state_name, state_abbv, state_fips, piece, hole, group, polygon, hexagon)

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
census_api_key(census_key, install=TRUE)

age10 <- get_decennial(geography = "state",
                       variables = "P013001",
                       year = 2010)
map_values <- statesmaps %>% left_join(age10, by = c("state_name" = "NAME"))




map_values %>% unnest(col=hexagon) %>%
  ggplot(aes( x = long, y = lat, group = group, fill=value)) +
  geom_polygon() +
  #  geom_text (aes(label=id)) +
  theme_void () +
  coord_map () +
  scale_fill_gradient2("Median Age", midpoint=median(age10$value))

plotly::ggplotly()

map_values %>% unnest(col=polygon) %>%
  ggplot(aes(x = long, y = lat, group = group, fill=value, label=state_name)) +
  geom_polygon(colour="grey70") +
  #  geom_text (aes(label=id)) +
  theme_void () +
  scale_fill_gradient2("Median Age", midpoint=median(age10$value)) +
  coord_map()

plotly::ggplotly()


map_values %>% pivot_longer(polygon:hexagon, values_to="data", names_to = "type") %>%
  tidyr::unnest(col = data) %>%
  ggplot(aes(x = long, y = lat, group = group, fill=value)) +
  #  geom_polygon(colour="grey70") +
  geom_polygon(colour="grey70") +
  #  geom_text (aes(label=id)) +
  theme_void () +
  scale_fill_gradient2("Median Age", midpoint=median(age10$value)) +
  coord_map()

