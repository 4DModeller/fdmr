#' Create a simple Leaflet map from data
#'
#' @param polygon_data Polygon data
#' @param raster_data Raster datas
#' @param legend_values Values for legend
#' @param legend_title Title for legend
#' @param add_scale_bar Add scale bar if TRUE
#' @param polygon_fill_opacity Leaflet polygon fill opacity, float from 0 to 1.0, passed to fillOpacity of leaflet::addPolygons
#' @param fill_colour_weight Polygon colour weight, float from 0 to 1.0, Passed to the weight argument of addPolygons
#' @param polygon_fill_colour 
#' @param polygon_line_colour 
#'
#' @return leaflet::leaflet
#' @export
plot_map <- function(polygon_data = NULL,
                     raster_data = NULL,
                     domain = NULL,
                     #  palette = "YlOrRd",
                     #  colour = "grey",
                     legend_values = NULL,
                     legend_title = NULL,
                     add_scale_bar = FALSE,
                     polygon_fill_colour = "red",
                     polygon_line_colour = "blue",
                     polygon_fill_opacity = 0.75,
                     fill_colour_weight = 1.0) {
  require_packages(packages = "leaflet")

  if (is.null(polygon_data) && is.null(raster_data)) {
    stop("Polygon or raster data must be given.")
  }

  # if(is.null(colours) && is.null(domain)) {
  #   colours <- leaflet::colorNumeric(palette = palette, domain = domain, reverse = FALSE)
  # }

  m <- leaflet::leaflet()
  m <- leaflet::addTiles(m)
  m <- leaflet::addProviderTiles(m, leaflet::providers$Esri.WorldImagery, group = "Satellite")

  layers <- c()

  if (!is.null(polygon_data)) {
    m <- leaflet::addPolygons(m, data = polygon_data, fillColor = polygon_fill_colour, color = polygon_line_colour, group = "Poly")
    layers <- append(layers, "Poly")
  }

  if (!is.null(raster_data)) {
    m <- leaflet::addRasterImage(m,
      x = raster_data,
      opacity = 0.75,
      group = "Raster",
      layerId = "raster",
      colors = "viridis",
    )
    layers <- append(layers, "Raster")
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

  if (!is.null(legend_values)) {
    m <- leaflet::addLegend(m,
      pal = "viridis",
      values = legend_values,
      opacity = 0.8,
      title = legend_title
    )
  }

  return(m)
}
