#' Create a simple Leaflet map from data
#'
#' @param data Data to plot
#' @param domain Domain for map
#' @param palette Palette, for example YlOrRd or Reds
#' @param legend_values Values for legend
#' @param legend_title Title for legend
#' @param add_scale_bar Add scale bar if TRUE
#' @param polygon_fill_opacity Leaflet polygon fill opacity, float from 0 to 1.0, passed to fillOpacity of leaflet::addPolygons
#' @param fill_colour_weight Polygon colour weight, float from 0 to 1.0, Passed to the weight argument of addPolygons
#'
#' @return map
#' @export
#' @importFrom magrittr %>%
plot_map <- function(data,
                     domain,
                     palette = "YlOrRd",
                     colour = "grey",
                     legend_values = NULL,
                     legend_title = NULL,
                     add_scale_bar = FALSE,
                     polygon_fill_opacity = 0.75,
                     fill_colour_weight = 1.0) {
  require_package(pkg_name = "leaflet")

  colours <- leaflet::colorNumeric(palette = palette, domain = domain, reverse = FALSE)

  m <- leaflet::leaflet(data = data)
  m <- leaflet::addTiles(m)


  # leaflet::addPolygons(
  #   fillColor = ~ colours(domain), color = colour, weight = fill_colour_weight,
  #   fillOpacity = polygon_fill_opacity
  # ) %>%
  # {
  #   if (add_scale_bar) leaflet::addScaleBar(., position = "bottomleft") else .
  # } %>%
  # {
  #   if (!is.null(legend_values)) {
  #     leaflet::addLegend(.,
  #       pal = colours,
  #       values = legend_values,
  #       opacity = 0.8,
  #       title = legend_title
  #     )
  #   } else {
  #     .
  #   }
  # }
  return(m)
}
