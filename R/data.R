#' Maps of states: polygons and hexagons
#'
#' @format A data frame of 51 states and 8 variables:
#' \describe{
#'   \item{state_name}{Name of the state}
#'   \item{state_abbv}{two-letter abbrevation of state}
#'   \item{state_fips}{character value of the fips code}
#'   \item{piece}{integer value enumerating different pieces of a state's area}
#'   \item{hole}{TRUE/FALSE - does the polygon describe a hole?}
#'   \item{group}{id for the corresponding polygon piece}
#'   \item{polygon}{list of data frames with polygon points}
#'   \item{hexagon}{list of data frames with hexagon points}
#'   \item{hexagon_labels}{list of data frames with centroids of the hexagons}
#'   \item{polygon_labels}{list of data frames with centroids of the polygons}
#' }
#' @source
#'   hexbin map: \url{https://raw.githubusercontent.com/holtzy/D3-graph-gallery/master/DATA/us_states_hexgrid.geojson.json};
#'   states map from \url{https://github.com/UrbanInstitute/urbnmapr}
#' @examples
#' library(ggplot2)
#' library(mapproj)
#' library(tidyr)
#' statesmaps %>% unnest(col=hexagon) %>%
#'   ggplot(aes( x = long, y = lat, group = group, fill=state_name)) +
#'     geom_polygon() +
#'     theme_void () +
#'     coord_map () +
#'     theme(legend.position="none")
#'
#' statesmaps %>% unnest(col=polygon) %>%
#'   ggplot(aes( x = long, y = lat, group = group, fill=state_name)) +
#'     geom_polygon() +
#'     theme_void () +
#'     coord_map () +
#'     theme(legend.position="none")
"statesmaps"


#' Average age of population in each US state (2010)
#'
#' This data is downloaded from the census API for the 2010 census.
#' @format A data frame of 52 states and 4 variables:
#' \describe{
#'   \item{GEOID}{Fips id of the state.}
#'   \item{NAME}{name of the state.}
#'   \item{variable}{character string of the variable in the census.}
#'   \item{value}{numeric value (in years) of the average age of a state's population.}
#' }
#' @source
#' Use the tidycensus package to retrieve the age10 data set:
#' age10 <- get_decennial(geography = "state",
#'                        variables = "P013001",
#'                        year = 2010)
"age10"


