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
                     palette = "YlOrRd",
                     legend_title = NULL,
                     add_scale_bar = FALSE,
                     polygon_fill_colour = "red",
                     polygon_line_colour = "grey",
                     polygon_line_weight = 1,
                     polygon_fill_opacity = 0.75,
                     fill_colour_weight = 1.0) {
  require_packages(packages = "leaflet")

  if (is.null(polygon_data) && is.null(raster_data)) {
    stop("Polygon or raster data must be given.")
  }
  library(leaflet)
  m <- leaflet::leaflet()
  m <- leaflet::addTiles(m)
  m <- leaflet::addProviderTiles(m, leaflet::providers$Esri.WorldImagery, group = "Satellite")

  layers <- c()




  # Original
  # colours <- colorNumeric(palette = "Reds", domain = polygon_data@data$ave.risk, reverse = FALSE)
  # leaflet() %>%
  #   addTiles() %>%
  #   addPolygons(
  #     data = polygon_data,
  #     fillColor = ~ colours(polygon_data@data$ave.risk),
  #     color = "transparent", weight = 1, fillOpacity = 0.8
  #   ) %>%
  #   addLegend(
  #     pal = colours, values = polygon_data@data$ave.risk, opacity = 0.8,
  #     title = "risk"
  #   )

  if (!is.null(polygon_data)) {
    if (!is.null(domain)) {
      colours <- leaflet::colorNumeric(palette = palette, domain = domain, reverse = FALSE)
      polygon_fill_colour <- ~ colours(domain)
    }
    m <- leaflet::addPolygons(m, data = polygon_data, fillColor = polygon_fill_colour, color = "transparent", weight = 1, fillOpacity = 0.8, group = "Poly")
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

  m <- leaflet::addLayersControl(m,
    position = "topright",
    baseGroups = c("OSM", "Satellite"),
    overlayGroups = layers,
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )

  if (add_scale_bar) {
    m <- leaflet::addScaleBar(m, position = "bottomleft")
  }

  # if (!is.null(domain)) {
  #   m <- leaflet::addLegend(m,
  #     pal = palette,
  #     values = domain,
  #     opacity = 0.8,
  #     title = legend_title
  #   )
  # }

  return(m)
}
