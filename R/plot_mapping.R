#' Create a simple Leaflet map from data
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
#'
#' @return leaflet::leaflet
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
                     polygon_fill_opacity = 0.6) {
  if (is.null(polygon_data) && is.null(raster_data)) {
    stop("Polygon or raster data must be given.")
  }
  library(leaflet)
  m <- leaflet::leaflet()
  m <- leaflet::addTiles(m)
  m <- leaflet::addProviderTiles(m, leaflet::providers$Esri.WorldImagery, group = "Satellite")

  # Store a vector of layers we add to the map,
  # used later to create the layers control object
  layers <- c()

  if (!is.null(polygon_data)) {
    if (!is.null(domain)) {
      colours <- leaflet::colorNumeric(palette = palette, domain = domain, reverse = FALSE)
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
    m <- leaflet::addRasterImage(m,
      x = raster_data,
      opacity = 0.75,
      group = "Raster",
      layerId = "raster",
      colors = palette,
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

  return(m)
}
