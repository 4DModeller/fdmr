#' Plot a mesh on a Leaflet map
#'
#' @param mesh Mesh data
#' @param spatial_data Spatial data, either a SpatialPolygonsDataFrame, SpatialPointsDataFrame or an object
#' that can be converted to a data.frame with longitude and latitude columns
#' @param markers Markers to display on map. A named list with latitude, longitude and label names must be given.
#' Expects longitude name to be longitude, latitude name to be latitude, label name to be label.
#' @param longitude_column Longitude column in spatial_data
#' @param latitude_column Latitude column in spatial_data name
#'
#' @return leaflet::leaflet
#' @export
plot_mesh <- function(mesh, spatial_data = NULL, markers = NULL, longitude_column = "LONG", latitude_column = "LAT") {
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

  plot_polygons <- FALSE
  plot_points <- FALSE
  if (!is.null(spatial_data)) {
    coords_only <- spatial_data[, c(longitude_column, latitude_column)]
    names(coords_only) <- c("LONG", "LAT")
    # We may not use this
    spatial_points <- NULL
    # Do some checks to see what kind of data we have
    # If we have a SpatialPolygonsDataFrame, we can plot the polygons
    # otherwise if we just have SpatialPoints we can plot the points
    # otherwise we don't plot anything
    if (is(spatial_data, "SpatialPolygonsDataFrame")) {
      plot_polygons <- TRUE
    } else {
      plot_points <- TRUE

      if (!is.data.frame(spatial_data)) {
        warning("Attempting to convert to a data.frame")
        spatial_data <- as.data.frame(spatial_data)
      }

      spatial_points <- sp::SpatialPointsDataFrame(
        coords = coords_only,
        data = spatial_data,
        proj4string = sp::CRS(expected_crs)
      )
    }
  }

  overlay_groups <- c("Mesh")

  m <- leaflet::leaflet()
  m <- leaflet::addTiles(m, group = "OSM")
  m <- leaflet::addPolygons(m, data = spatial_mesh, weight = 0.5, fillOpacity = 0.2, fillColor = "#5252ca", group = "Mesh")

  if (plot_polygons) {
    m <- leaflet::addPolygons(m, data = spatial_data, fillColor = "#d66363", color = "green", weight = 1, group = "Spatial")
    overlay_groups <- append(overlay_groups, "Spatial")
  } else if (plot_points) {
    m <- leaflet::addCircles(m, data = spatial_points, group = "Spatial", fillColor = "#b9220b", color = "#b9220b")
    overlay_groups <- append(overlay_groups, "Spatial")
  }

  if (!is.null(markers)) {
    m <- leaflet::addMarkers(m, lng = markers$longitude, lat = markers$latitude, label = markers$label, group = "Markers")
    overlay_groups <- append(overlay_groups, "Markers")
  }
  
  m <- leaflet::addLayersControl(m,
    position = "topright",
    baseGroups = c("OSM"),
    overlayGroups = overlay_groups,
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )
  m
}
