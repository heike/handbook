#' Light or dark color selection
#'
#' @param colors vector of background colors
#' @param light hexcode for light text option
#' @param dark hexcode for dark text option
#' @param type YIQ or Luminance
#'
#' @importFrom farver decode_colour
#' @return vector of darks and lights
#'
#' @examples
#' colors <- c("#ef4444", "#faa31b", "#fff000", "#82c341",
#'             "#009f75", "#88c6ed", "#394ba0", "#d54799")
#'
#' text_color(colors)
#' @export
text_color <- function(colors, light = "#FFFFFF", dark = "#000000", type = c("YIQ", "Luminance")) {

  type <- rlang::arg_match(type)

  if(type == "YIQ") {
    yiq_contrast_threshold <- 128

    colors <- grDevices::col2rgb(colors)
    score <- colSums(colors * c(299, 587, 144))/1000

    ifelse(score >= yiq_contrast_threshold, dark, light)
  } else {
    hcl <- farver::decode_colour(colors, "rgb", "hcl")

    ifelse(hcl[, "l"] > 50, dark, light)
  }
}




#' Automatic color selection for text for optimal contrast
#'
#' These geoms are based on \code{\link[ggplot2]{geom_text}} and
#' \code{\link[ggplot2]{geom_label}}. See the documentation for those
#' functions for more details. Differences from those functions are noted
#' here.
#'
#' Text with black or white text ...
#'
#' @inheritParams ggplot2::geom_text
#' @inheritParams text_color
#' @importFrom rlang list2
#' @importFrom cli cli_abort
#' @examples
#' library(ggplot2)
#'
#' ggplot(mpg, aes(x= class, fill = class)) +
#'   geom_bar() +
#'   geom_text_contrast(aes(label = class, y = 1), hjust = 0) +
#'   scale_fill_nces(palette=1) +
#'   theme_nces() +
#'   theme(legend.position = "none") +
#'   coord_flip()
#'
#'   ggplot(mpg, aes(x= class, fill = class)) +
#'   geom_bar() +
#'   geom_text_contrast(aes(label = class, y = 1), type = "Luminance", hjust = 0) +
#'   scale_fill_nces(palette=1) +
#'   theme_nces() +
#'   theme(legend.position = "none") +
#'   coord_flip()
#'
#'
#' ggplot(mpg, aes(x= class, fill = class)) +
#'   geom_bar() +
#'   geom_text_contrast(aes(label = class, y = 1), hjust = 0) +
#'   scale_fill_manual(values = c("#d54799", "#394ba0", "#88c6ed",
#'             "#009f75", "#82c341", "#fff000", "#faa31b")) +
#'   theme_nces() +
#'   theme(legend.position = "none") +
#'   coord_flip()
#'
#' ggplot(mpg, aes(x= class, fill = class)) +
#'   geom_bar() +
#'   geom_text_contrast(aes(label = class, y = 1), type = "Luminance", hjust = 0) +
#'   scale_fill_manual(values = c("#d54799", "#394ba0", "#88c6ed",
#'             "#009f75", "#82c341", "#fff000", "#faa31b")) +
#'   theme_nces() +
#'   theme(legend.position = "none") +
#'   coord_flip()
#'
#' @export
geom_text_contrast <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           parse = FALSE,
                           nudge_x = 0,
                           nudge_y = 0,
                           check_overlap = FALSE,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           light = "#FFFFFF", dark = "#000000", type = "YIQ")
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      cli::cli_abort(c(
        "both {.arg position} and {.arg nudge_x}/{.arg nudge_y} are supplied",
        "i" = "Only use one approach to alter the position"
      ))
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextContrast,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      light = light,
      dark = dark,
      type = type,
      ...
    )
  )

}



#' @rdname geom_text_contrast
#' @format NULL
#' @usage NULL
#' @export
GeomTextContrast <- ggplot2::ggproto("GeomTextContrast", ggplot2::GeomText,
                    # required_aes = c("x", "y", "label"),
                    #
                    # default_aes = aes(
                    #   colour = "black", fill = NULL, size = 3.88, angle = 0, hjust = 0.5,
                    #   vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
                    # ),

                    draw_panel = function(data, panel_params, coord, parse = FALSE,
                                          na.rm = FALSE, check_overlap = FALSE,
                                          light = "#FFFFFF", dark = "#000000", type = "YIQ") {

                      lab <- data$label
                      if (parse) {
                        lab <- parse_safe(as.character(lab))
                      }

                      data <- coord$transform(data, panel_params)

                      if (is.character(data$vjust)) {
                        data$vjust <- ggplot2:::compute_just(data$vjust, data$y, data$x, data$angle)
                      }
                      if (is.character(data$hjust)) {
                        data$hjust <- ggplot2:::compute_just(data$hjust, data$x, data$y, data$angle)
                      }

                      if (!is.null(data$fill)) {
                        data$colour <- text_color(data$fill, light = light, dark = dark, type = type)
                      }

                      grid::textGrob(
                        lab,
                        data$x, data$y, default.units = "native",
                        hjust = data$hjust, vjust = data$vjust,
                        rot = data$angle,
                        gp = grid::gpar(
                          col = alpha(data$colour, data$alpha),
                          fontsize = data$size * .pt,
                          fontfamily = data$family,
                          fontface = data$fontface,
                          lineheight = data$lineheight
                        ),
                        check.overlap = check_overlap
                      )
                    },

                    draw_key = ggplot2::draw_key_text
)


