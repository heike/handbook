% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/animate_map.R
\name{animate_map}
\alias{animate_map}
\title{Animated choropleth map of the US}
\usage{
animate_map(data, value = "value", state = "state", value_name = value)
}
\arguments{
\item{data}{data frame with values by state. By default, it is assumed that these variables are named \code{value} and \code{state}}

\item{value}{name of the variable in \code{data} containing the values for each state}

\item{state}{name of the variable in \code{data} containing the state names}

\item{value_name}{defaults to \code{value}. Regular expressions can be used to distinguish between multiple instances.}
}
\description{
Create an animated choropleth map of the US that flips between hexagons and the geographic regions for each state.
The statesmaps dataset is used to provide the polygonal structure.
}
\examples{
# # if tidycensus is installed:
# age10 <- tidycensus::get_decennial(geography = "state",
#                        variables = "P013001", year = 2010)
p <- animate_map(age10, state = "NAME", value_name="Median Age")
# Rendering p will create an animation in form of a gif with multiple frames.
# Creating the gif takes some time (order of minutes)
}
