#' Create a simple Leaflet map from data
#'
#' @param data Polygon data
#' @param domain Domain for map
#' @param markers A vector of lists with lat/long and names for markers on the map
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
plot_map <- function(data = NULL,
                     markers = NULL,
                     domain = NULL,
                     palette = "YlOrRd",
                     colour = "grey",
                     legend_values = NULL,
                     legend_title = NULL,
                     add_scale_bar = FALSE,
                     polygon_fill_opacity = 0.75,
                     fill_colour_weight = 1.0) {
  require_packages(packages = "leaflet")

  # colours <- leaflet::colorNumeric(palette = palette, domain = domain, reverse = FALSE)

  m <- leaflet::leaflet()
  m <- leaflet::addTiles(m)

  m <- leaflet::addPolygons(m,
    data = data,
    # fillColor = ~colours(domain), color = colour, weight = fill_colour_weight,
    # fillColor = ~colours(), color = colour, weight = fill_colour_weight,
    fillColor = "red",
    fillOpacity = polygon_fill_opacity
  )

  if (!is.null(markers)) {
    m <- leaflet::addMarkers(m, lng = markers$longitude, lat = markers$latitude, label = markers$label)
  }

  if (add_scale_bar) {
    m <- leaflet::addScaleBar(m, position = "bottomleft")
  }

  # if (!is.null(legend_values)) {
  #   m <- leaflet::addLegend(m,
  #     pal = colours,
  #     values = legend_values,
  #     opacity = 0.8,
  #     title = legend_title
  #   )
  # }
  return(m)
}

#' Return tile data
#'
#' @param tiles
#'
#' @return list
#' @keywords internal
get_tile_data <- function(tiles) {
  if (is.null(tiles)) {
    return(NULL)
  }

  tile_data <- list(
    "satellite" = list(
      "url" = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
      "attribution" = "Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community"
    )
  )

  tiles <- tolower(tiles)
  if (!(tiles %in% names(tile_data))) {
    stop(paste("Invalid tile selection, please select from one of: "), names(tile_data))
  }

  return(tile_data[[tiles]])
}

#' Add tile data to the map
#'
#' @param map leaflet::map
#' @param tiles Optional, currently select satellite for ARCGIS satellite imagery
#'
#' @return leaflet map
#' @keywords internal
add_tiles <- function(map, tiles = NULL) {
  if (is.null(tiles)) {
    return(leaflet::addTiles(map))
  }

  tile_data <- list(
    "satellite" = list(
      "url" = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
      "attribution" = "Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community"
    )
  )

  tiles <- tolower(tiles)
  if (!(tiles %in% names(tile_data))) {
    stop(paste("Invalid tile selection, please select from one of: "), names(tile_data))
  }

  url <- tile_data[[tiles]][["url"]]
  attribution <- tile_data[[tiles]][["attribution"]]

  map <- leaflet::addTiles(map, url, attribution = attribution)
  return(map)
}

#' Plot polygon data on a Leaflet map
#'
#' @param data Polygon data
#' @param tiles Optional, currently select satellite for ARCGIS satellite imagery
#' @param fill_colour Fill colour of polygon
#'
#' @return leaflet map
#' @export
plot_map_polygon <- function(data, tiles = NULL, fill_colour = "red") {
  m <- leaflet::leaflet(data = data)
  m <- add_tiles(map = m, tiles = tiles)
  m <- leaflet::addPolygons(m,
    stroke = FALSE, smoothFactor = 0.3,
    fillColor = fill_colour,
    fillOpacity = 0.5
  )

  return(m)
}


#' Plot raster data on a Leaflet Map
#'
#' @param raster_data Raster data to plot
#' @param tiles Optional, currently select satellite for ARCGIS satellite imagery
#'
#' @return leaflet map
#' @export
plot_map_raster <- function(raster_data, tiles = NULL, polygon_data = NULL, polygon_fill_colour = "red") {
  m <- leaflet::leaflet(polygon_data)
  m <- add_tiles(map = m, tiles = tiles)
  m <- leaflet::addRasterImage(m,
    x = raster_data,
    opacity = 1
  )

  if (!is.null(polygon_data)) {
    m <- leaflet::addPolygons(m,
      stroke = FALSE, smoothFactor = 0.3,
      fillColor = polygon_fill_colour,
      fillOpacity = 0.5
    )
  }

  return(m)
}
