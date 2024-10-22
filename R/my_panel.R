#' my_panel
#'
#' Auxliary function for lattice level plot.
#' @param x Input x.
#' @param y Input y.
#' @param z Matrix data.
#' @return None.
#' @export

my_panel <- function(x, y, z, cex = 0.8, ...) {
  panel.levelplot(x,y,z,...)
  #panel.text(x, y, round(z,1), cex = cex)
  panel.text(x, y, sprintf("%.1f", z), cex = cex)
}
