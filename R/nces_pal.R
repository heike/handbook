
ies_blues <- rev(c('#071d49', '#062a6a', '#00378c', '#114ea4', '#1e5db0',
                   '#2666b9', '#3273c2', '#3d7fcc', '#5698dc', '#70b2e9',
                   '#80beee', '#8ecbf4', '#b0e5fa', '#d6f5fa', '#eaf9fd', '#f1f7f9'))


color_classes <- function(pal, n){
  stopifnot(between(n, 2, 9))

  if (n == 2) return(pal[c(4,9)])
  if (n == 3) return(pal[c(3,7,11)])
  if (n == 4) return(pal[c(2,5,9,13)])
  if (n == 5) return(pal[c(2,5,8,11,14)])
  if (n == 6) return(pal[c(2,4,7,9,12,14)])
  if (n == 7) return(pal[c(2,4,6,8,10,13,15)])
  if (n == 8) return(pal[c(2,4,6,8,9,12,14,16)])
  if (n == 9) return(pal[c(1,3,4,6,8,10,13,14,16)])
}



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
#' nces_palette(palette = "ies_blues")(2)
nces_palette <- function (type = "seq", palette = 1, direction = 1)
{
  colors <- nces_pals[[type]][[palette]]
  if (palette == "ies_blues") {
    f <- function(n) {
      color_classes(ies_blues, n)
    }
    return(f)
  }

  if (direction < 0) colors <- rev(colors) # color scheme is reversed
  function(n) {
    choose_n(colors, n, type)
  }
}


