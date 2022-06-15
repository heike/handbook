#' Scales using IES colors
#'
#' @rdname scale_nces
#' @export
#' @inheritParams ggplot2::scale_colour_brewer
scale_colour_nces <- function(..., type = "seq", palette = 1, direction = 1, aesthetics = "colour") {
  ggplot2::discrete_scale(aesthetics, "nces", nces_palette(type, palette, direction), ...)
}

#' @rdname scale_nces
#' @export
scale_color_nces <- scale_colour_nces


#' Scales using IES colors
#'
#' @rdname scale_nces
#' @export
#' @inheritParams ggplot2::scale_fill_brewer
#' @examples
#' library(ggplot2)
#' dframe <- data.frame(x = sample(2,100, replace=TRUE))
#' ggplot(data = dframe, aes(x = x, fill=factor(x))) +
#'   geom_bar() +
#'   scale_fill_nces() +
#'   theme_bw()
scale_fill_nces <- function(..., type = "seq", palette = 1, direction = -1, aesthetics = "fill") {
  ggplot2::discrete_scale(aesthetics, "nces", nces_palette(type, palette, direction), ...)
}




