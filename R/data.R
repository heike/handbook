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


#' List of NCES color palettes
#'
#' @format A list of sequential and divergent color schemes
#' @name nces_pals
"nces_pals"
