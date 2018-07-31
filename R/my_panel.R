#' my_panel
#'
#' Auxliary function for lattice level plot.
#' @param x Input x.
#' @param y Input y.
#' @param z Matrix data.
#' @return None.
#' @export

my_panel <- function(x, y, z, ...) {
  panel.levelplot(x,y,z,...)
  panel.text(x, y, round(z,1))
}
