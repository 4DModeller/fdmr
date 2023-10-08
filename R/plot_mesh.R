#' Plot a mesh
#'
#' @param mesh Mesh data
#' @param point_data Points data
#' @param point_colour Colour for points
#' @param cex Point size magnifier
#'
#' @return terra::plot
#' @export
plot_mesh <- function(mesh, point_data, point_colour = "blue", cex = 0.1) {
  terra::plot(mesh, main = "")
  terra::points(point_data, col = point_colour, cex = cex)
}


#' Plot a mesh on an interactive Leaflet map
#'
#' @param mesh Mesh data
#'
#' @return leaflet::leaflet
#' @export
plot_mesh_interactive <- function(mesh) {
  spatial_mesh <- fdmr::mesh_to_spatial(mesh)

  read_crs <- sp::proj4string(mesh)

  if (!("+proj=longlat +datum=WGS84" %in% read_crs)) {
    stop("Mesh must be in latlong WGS84 projection")
  }

  # Plotting on leaflet without just using WGS84 is tricky
  crs <- leaflet::leafletCRS(
    code = "EPSG:32632",
    proj4def = "+proj=utm +zone=32"
  )

  leaflet::leaflet(options = leaflet::leafletOptions(crs = crs)) %>%
    leaflet::addTiles(group = "OSM") %>%
    leaflet::addPolygons(data = spatial_mesh, weight = 0.5, fillOpacity = 0.2, fillColor = "#5252ca", group = "Mesh")
}
