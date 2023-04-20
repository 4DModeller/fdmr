#' Plot a mesh
#'
#' @param mesh Mesh data
#' @param point_data Points data
#' @param point_colour Colour for points
#' @param cex Point size magnifier
#'
#' @return NULL
#' @export
plot_mesh <- function(mesh, point_data, point_colour = "blue", cex = 0.1) {
  terra::plot(mesh, main = "")
  terra::points(point_data, col = point_colour, cex = cex)
}
