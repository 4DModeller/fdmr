# #' Plot a mesh
# #'
# #' @param mesh Mesh data
# #' @param point_data Points data
# #' @param point_colour Colour for points
# #' @param cex Point size magnifier
# #'
# #' @return terra::plot
# #' @export
# plot_mesh <- function(mesh, point_data, point_colour = "blue", cex = 0.1) {
#   terra::plot(mesh, main = "")
#   terra::points(point_data, col = point_colour, cex = cex)
# }


#' Plot a mesh on an interactive Leaflet map
#'
#' @param mesh Mesh data
#'
#' @return leaflet::leaflet
#' @export
plot_mesh <- function(mesh) {
  expected_crs <- "+proj=longlat +datum=WGS84"
  crs_string <- fmesher::fm_proj4string(mesh)

  if (is.na(crs_string)) {
    warning("Cannot read CRS from mesh, assuming WGS84")
  } else if (!(expected_crs %in% crs_string)) {
    warning("Transforming mesh from ", crs_string, " to ", expected_crs)
    mesh <- fmesher::fm_transform(
      x = mesh,
      crs = sp::CRS(expected_crs)
    )
  }

  spatial_mesh <- fdmr::mesh_to_spatial(mesh = mesh, crs = expected_crs)

  leaflet::leaflet() %>%
    leaflet::addTiles(group = "OSM") %>%
    leaflet::addPolygons(data = spatial_mesh, weight = 0.5, fillOpacity = 0.2, fillColor = "#5252ca", group = "Mesh")
}
