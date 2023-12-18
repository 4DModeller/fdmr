#' Create a simple map from data using either the leaflet or mapview packages
#'
#' NOTE that the mapview backend is only intended for quick viewing of data,
#' most of the customisation arguments are not available.
#'
#' @param polygon_data Polygon data
#' @param raster_data Raster datas
#' @param domain Domain data to be passed to leaflet::colorNumeric and leaflet::addLegend
#' @param markers Markers to display on map. A named list with latitude, longitude and label names must be given.
#' @param palette Palette to be used for colours, defaults to viridis
#' @param legend_title Title for legend
#' @param add_scale_bar Add scale bar if TRUE
#' @param polygon_fill_opacity Leaflet polygon fill opacity, float from 0 to 1.0, passed to fillOpacity of leaflet::addPolygons
#' @param polygon_fill_colour Polygon fill colour
#' @param polygon_line_colour Polygon surrounding line colour
#' @param polygon_line_weight Polygon surrounding line weight
#' @param reverse Reverse the colour palette if TRUE
#' @param wrapping Split polygons along the antimeridian (-180/180 boundary) if TRUE
#' @param backend Backend package to use for plotting, either "leaflet" or "mapview"
#'
#' @return leaflet::leaflet or mapview::mapview
#'
#' @export
plot_map <- function(polygon_data = NULL,
                     raster_data = NULL,
                     domain = NULL,
                     markers = NULL,
                     palette = "viridis",
                     legend_title = NULL,
                     add_scale_bar = FALSE,
                     polygon_fill_colour = "#E4572E",
                     polygon_line_colour = "grey",
                     polygon_line_weight = 1,
                     polygon_fill_opacity = 0.6,
                     reverse = FALSE,
                     wrapping = FALSE,
                     backend = "leaflet") {
  if (backend == "leaflet") {
    plot_map_leaflet(
      polygon_data = polygon_data,
      raster_data = raster_data,
      domain = domain,
      markers = markers,
      palette = palette,
      legend_title = legend_title,
      add_scale_bar = add_scale_bar,
      polygon_fill_colour = polygon_fill_colour,
      polygon_line_colour = polygon_line_colour,
      polygon_line_weight = polygon_line_weight,
      polygon_fill_opacity = polygon_fill_opacity,
      reverse = reverse,
      wrapping = wrapping
    )
  } else if (backend == "mapview") {
    plot_map_mapview(
      polygon_data = polygon_data,
      raster_data = raster_data
    )
  } else {
    stop("Invalid backend given, must be either 'leaflet' or 'mapview'.")
  }
}


#' Create a simple Leaflet map from data
#'
#' @param polygon_data Polygon data
#' @param raster_data Raster data
#' @param domain Domain data to be passed to leaflet::colorNumeric and leaflet::addLegend
#' @param markers Markers to display on map. A named list with latitude, longitude and label names must be given.
#' @param palette Palette to be used for colours, defaults to viridis
#' @param legend_title Title for legend
#' @param add_scale_bar Add scale bar if TRUE
#' @param polygon_fill_opacity Leaflet polygon fill opacity, float from 0 to 1.0, passed to fillOpacity of leaflet::addPolygons
#' @param polygon_fill_colour Polygon fill colour
#' @param polygon_line_colour Polygon surrounding line colour
#' @param polygon_line_weight Polygon surrounding line weight
#' @param reverse Reverse the colour palette if TRUE
#' @param wrapping Split polygons along the antimeridian (-180/180 boundary) if TRUE
#'
#' @return leaflet::leaflet
#' @keywords internal
plot_map_leaflet <- function(polygon_data = NULL,
                             raster_data = NULL,
                             domain = NULL,
                             markers = NULL,
                             palette = "viridis",
                             legend_title = NULL,
                             add_scale_bar = FALSE,
                             polygon_fill_colour = "#E4572E",
                             polygon_line_colour = "grey",
                             polygon_line_weight = 1,
                             polygon_fill_opacity = 0.6,
                             reverse = FALSE,
                             wrapping = FALSE) {
  if (is.null(polygon_data) && is.null(raster_data)) {
    stop("Polygon or raster data must be given.")
  }
  library(leaflet)
  m <- leaflet::leaflet()
  m <- leaflet::addTiles(m)
  m <- leaflet::addProviderTiles(m, leaflet::providers$Openstreetmap, group = "Satellite")
  m <- leafem::addMouseCoordinates(m, native.crs = TRUE)

  # Store a vector of layers we add to the map,
  # used later to create the layers control object
  layers <- c()

  if (!is.null(polygon_data)) {
    if (isTRUE(wrapping)) {
      polygon_data <- fdmr::antimeridian_wrapping(polygon_data, crs = "+proj=longlat +datum=WGS84", unique_inst = TRUE, to_sp = FALSE)
    }
    if (!is.null(domain)) {
      colours <- leaflet::colorNumeric(palette = palette, domain = domain, reverse = reverse)
      polygon_fill_colour <- ~ colours(domain)
      m <- leaflet::addLegend(m,
        pal = colours,
        values = domain,
        opacity = 0.8,
        title = legend_title
      )
    }

    m <- leaflet::addPolygons(m,
      data = polygon_data,
      fillColor = polygon_fill_colour,
      color = polygon_line_colour,
      weight = polygon_line_weight,
      fillOpacity = polygon_fill_opacity,
      group = "Poly"
    )
    layers <- append(layers, "Poly")
  }

  if (!is.null(raster_data)) {
    colours <- leaflet::colorNumeric(palette = palette, domain = raster::values(raster_data), na.color = rgb(0, 0, 0, 0), reverse = reverse)
    m <- leaflet::addRasterImage(m,
      x = raster_data,
      opacity = 0.75,
      group = "Raster",
      layerId = "raster",
      colors = colours,
    )
    m <- leaflet::addLegend(m,
      pal = colours,
      values = raster::values(raster_data),
      opacity = 0.75,
      title = legend_title,
      na.label = ""
    )
    layers <- append(layers, "Raster")
  }

  if (!is.null(markers)) {
    m <- leaflet::addMarkers(m, lng = markers$longitude, lat = markers$latitude, label = markers$label, group = "Markers")
    layers <- append(layers, "Markers")
  }

  m <- leaflet::addLayersControl(m,
    position = "topright",
    baseGroups = c("OSM", "Satellite"),
    overlayGroups = layers,
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )

  if (add_scale_bar) {
    m <- leaflet::addScaleBar(m, position = "bottomleft")
  }

  m <- leaflet::addMeasure(m, position = "bottomleft", primaryLengthUnit = "kilometers", primaryAreaUnit = "sqmeters")

  return(m)
}


#' A simple map plotter using mapview. This is only intended for very quick viewing of data.
#'
#' @param polygon_data Spatial data to plot
#' @param raster_data Raster data to plot
#'
#' @return mapview::mapview
#' @keywords internal
plot_map_mapview <- function(polygon_data = NULL, raster_data = NULL) {
  if (is.null(polygon_data) && is.null(raster_data)) {
    stop("Spatial or raster data must be given.")
  }

  map_types <- c("OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap")

  m <- mapview::mapview(map.types = map_types)
  if (!is.null(polygon_data)) {
    m <- m + mapview::mapview(polygon_data)
  }
  if (!is.null(raster_data)) {
    m <- m + mapview::mapview(raster_data)
  }
  return(m)
}
