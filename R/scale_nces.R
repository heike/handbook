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



#' Discretize numeric variable
#'
#' @param x numeric variable
#' @param group discrete variable
#' @param n number of bins for variable x (by group)
#' @param type quantile or cut
#' @importFrom stats quantile
#' @importFrom dplyr group_by summarize `%>%`
#' @export
cut_by_n <- function(x, group, n, type) {
  # assume x is numeric, cut into vector of
  dframe <- data.frame(x, group) %>% group_by(group) %>% summarize(x = mean(x))
  br <- NULL
  if (type == "quantile") {
    br <- quantile(dframe$x,  seq(0,1, length.out=n+1))
    if (length(br) != length(unique(br))) {
      warning("Ties in quantiles affect the color assignment. Consider changing the number of colors <n>")
      br <- unique(br)
    }
  }
  if (type == "cut") {
    br <- seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length.out = n+1)
  }

  cut(x, breaks = br, include.lowest=TRUE)
}





