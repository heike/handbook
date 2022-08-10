#' Animated choropleth map of the US
#'
#' Create an animated choropleth map of the US that flips between hexagons and the geographic regions for each state.
#' The statesmaps dataset is used to provide the polygonal structure.
#' @param data data frame with values by state. By default, it is assumed that these variables are named `value` and `state`
#' @param value name of the variable in `data` containing the values for each state
#' @param state name of the variable in `data` containing the state names
#' @param value_name defaults to `value`. Regular expressions can be used to distinguish between multiple instances.
#' @importFrom dplyr left_join between
#' @importFrom assertthat assert_that has_name
#' @importFrom tidyr unnest pivot_longer
#' @import ggplot2
#' @importFrom stats median
#' @importFrom gganimate transition_states
#' @export
#' @examples
#' # # if tidycensus is installed:
#' # age10 <- tidycensus::get_decennial(geography = "state",
#' #                        variables = "P013001", year = 2010)
#' p <- animate_map(age10, state = "NAME", value_name="Median Age")
#' # Rendering p will create an animation in form of a gif with multiple frames.
#' # Creating the gif takes some time (order of minutes)
animate_map <- function(data,  value = "value", state = "state", value_name=value) {
  assertthat::assert_that(is.character(value))
  assertthat::assert_that(is.character(state))
  assertthat::has_name(data, value)
  assertthat::has_name(data, state)
  data <- data[,c(state, value)]
  names(data) <- c("state_name", "value")
  #browser()

  statesmaps <- get0("statesmaps", envir = as.environment("package:handbook"))

  map_values <- statesmaps %>% left_join(data, by = "state_name")
  long_map <- map_values %>%
    pivot_longer("polygon":"hexagon", values_to="data", names_to = "type") %>%
    tidyr::unnest(col = data)

  p <- long_map %>%
    ggplot(aes(x = .data$long, y = .data$lat, group = .data$group, fill=.data$value)) +
    geom_polygon(colour="grey70") + # include first piece so that the scaling for the colors is scaled correctly
    geom_polygon(colour="grey70", data = long_map %>% dplyr::filter(.data$piece==1)) +  # plot first piece on top
    scale_fill_gradient2("Median age", midpoint=median(data$value)) +
    theme_void() +
    theme(legend.position = "bottom",
          legend.key.width= unit(.1, 'snpc')) +
    coord_map()


  p + gganimate::transition_states(.data$type,
                                   transition_length = 2,
                                   state_length = 1)
}
