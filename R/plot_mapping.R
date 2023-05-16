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
#' @return NULL
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

  leaflet::leaflet(data = data) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(
      fillColor = ~ colours(domain), color = colour, weight = fill_colour_weight,
      fillOpacity = polygon_fill_opacity
    ) %>%
    {
      if (add_scale_bar) leaflet::addScaleBar(., position = "bottomleft") else .
    } %>%
    {
      if (!is.null(legend_values)) {
        leaflet::addLegend(.,
          pal = colours,
          values = legend_values,
          opacity = 0.8,
          title = legend_title
        )
      } else {
        .
      }
    }
}

#' Plot polygon data on a map
#'
#' @param data Polygon data
#' @param tiles Optional, currently select satellite for ARCGIS satellite imagery
#' @param fill_colour Fill colour of polygon
#'
#' @return leaflet map
#' @export
plot_polygon <- function(data, tiles = NULL, fill_colour = "red") {
  tile_data <- list(
    "satellite" = list(
      "tiles_url" = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
      "attribution" = "Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community"
    )
  )

  m <- leaflet::leaflet(data = data)
  if (!is.null(tiles)) {
    if (!(tiles %in% names(tile_data))) {
      stop(paste("Invalid tile selection, please select from one of: "), names(tile_data))
    }
    m <- leaflet::addTiles(m, tile_data[[tiles]][["tiles_url"]])
  } else {
    m <- leaflet::addTiles(m)
  }

  m <- leaflet::addPolygons(m,
    stroke = FALSE, smoothFactor = 0.3,
    fillColor = fill_colour,
    fillOpacity = 0.5
  )

  return(m)
}
