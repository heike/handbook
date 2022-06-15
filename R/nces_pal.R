choose_n <- function(colors, n, type) {
  pal <- colors
  if (n > length(colors)) {
    warning(sprintf("chosen color palette only has %d colors.", length(colors)))
    return(pal)
  }

  # map color steps to equidistant points on [0,1]
  # then convert to indices
  idx <- round(seq(1, length(colors), length=n))


  pal[idx]
}

#' Helper function
#'
#' Provide access to one of the pre-defined nces color schemes
#' @param type type of color scheme, one of 'seq' (sequential) or 'div' (divergent)
#' @param palette integer specifying the palette, number of 1:6 for sequential colors and 1:3 for divergent
#' @param direction order of mapping of variable values to color values, either positive or negative value
#' @export
#' @examples
#' nces_palette()
nces_palette <- function (type = "seq", palette = 1, direction = 1)
{
  colors <- nces_pals[[type]][[palette]]

  if (direction < 0) colors <- rev(colors) # color scheme is reversed
  function(n) {
    choose_n(colors, n, type)
  }
}


