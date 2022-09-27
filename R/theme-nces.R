#' Theme for NCES
#'
#' Themes set the general aspect of the plot such as the colour of the
#' background, gridlines, the size and colour of fonts.
#' \code{theme_nces} provides access to the regular ggplot2 theme that is been
#' fine-tuned for NCES.
#'
#' @rdname theme_nces
#' @inheritParams ggplot2::theme_minimal
#' @import ggplot2
#' @importFrom ggtext element_textbox_simple
#' @examples
#' library(ggplot2)
#'
#' ggplot(mpg, aes(class)) +
#'   geom_bar() +
#'   theme_nces()
#'
#' @export

theme_nces <- function (base_size = 11, base_family = "",
                        base_line_size = base_size / 22,
                        base_rect_size = base_size / 22)
{
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      axis.ticks        = element_blank(),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      strip.background  = element_blank(),
      plot.background   = element_blank(),
      plot.caption      = ggtext::element_textbox_simple(size = rel(0.6), hjust = 0, color = "grey60", margin = ggplot2::margin(20, 0, 0, 0)),
      plot.title        = ggtext::element_textbox_simple(size = rel(1.17), margin = ggplot2::margin(5, 0, 5, 0)),
      plot.subtitle     = ggtext::element_textbox_simple(size = rel(0.79), margin = ggplot2::margin(5, 20, 0, 0)),
      legend.title      = element_text(size = rel(.95)),
      legend.text       = element_text(size = rel(.7))
    )
}
