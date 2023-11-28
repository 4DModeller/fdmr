#' Convert latitude and longitude to UTM coordinates
#'
#' @param lat Latitude
#' @param lon Longitude
#'
#' @return vector of UTM coordinates
#' @export
latlong_to_utm <- function(lat, lon) {
  # Create a spatial points object with input coordinates
  input_point <- sp::SpatialPoints(matrix(c(lon, lat), ncol = 2), proj4string = sp::CRS("+proj=longlat +datum=WGS84"))

  # Define the UTM zone number and hemisphere for the output coordinate system
  utm_zone <- as.character((floor((lon + 180) / 6) %% 60) + 1)
  utm_hemisphere <- ifelse(lat < 0, "S", "N")
  utm_proj_string <- paste0("+proj=utm +zone=", utm_zone, " +", utm_hemisphere)

  # Convert the input point to the output coordinate system
  output_point <- sp::spTransform(input_point, sp::CRS(utm_proj_string))

  # Return the UTM coordinates as a vector
  return(c(output_point@coords[, 1], output_point@coords[, 2]))
}


#' Check if spatial data has coordinates set using sp::coordinates()
#' Returns TRUE if coordinates have been set
#'
#' @param spatial_data
#'
#' @return bool
#' @keywords internal
has_coords <- function(spatial_data) {
  tryCatch(
    {
      sp::coordinates(spatial_data)
      TRUE
    },
    error = function(err) {
      FALSE
    }
  )
}

#' Convert longitudes from 0 to 360 degrees to -180 to 180 degrees
#' @param sf_data An sf object; does not accept SpatialPolygon* objects
#' @param crs CRS as a proj4string or EPSG code
#' @param add_data Select if data associated with the object are carried forward by the transformed version, defaults to FALSE
#' @param longitude_column Name of longitude, defaults to LONG
convert_from_lon_360 <- function(sf_data, crs = 4326, add_data = TRUE, longitude_column = "LONG") {
  # Get polygon coordinates
  coords <- sf::st_coordinates(sf_data)
  
  # Check if POLYGON contains holes, remove if so
  if ((base::length(base::unique(coords[, "L1"])) > 1) & (base::ncol(coords) > 3)) {
    warning("Converter does not define the interior holes. Please convert your data yourself if these features are crucial")
    sf_data <- sfheaders::sf_remove_holes(obj = sf_data)
    coords <- sf::st_coordinates(sf_data)
  }
  
  coords_conv <- coords
  
  # Convert data coordinates if selected
  if (base::isTRUE(add_data) & (!base::is.null(longitude_column))) {
    if (base::is.null(sf_data[["LONG"]]) & (longitude_column == "LONG")) {
      warning("`LONG` not found. Please change the default `longitude_column` name to the longitude name in your dataset")
    } else {
      sf_data[[longitude_column]] <- base::ifelse(sf_data[[longitude_column]] > 180, 
                                                  sf_data[[longitude_column]] - 360, 
                                                  sf_data[[longitude_column]])
    }
  }
  
  # Convert polygon longitudes
  coords_conv[, "X"] <- base::ifelse(coords[, "X"] > 180, coords[, "X"] - 360, coords[, "X"])
  
  # Define the feature column and convert to POLYGON
  if (base::ncol(coords_conv) == 4) {
    poly <- "L2"
    conv <- sfheaders::sf_polygon(obj = coords_conv, x = 'X', y = 'Y', polygon_id = poly)
    conv <- sf::st_sf(conv, crs = crs)
  } else if (base::ncol(coords_conv) == 5) {
    poly <- "L3"
    multipoly <- "L2"
    conv <- sfheaders::sf_multipolygon(obj = coords_conv, x = 'X', y = 'Y', multipolygon_id = multipoly, polygon_id = poly)
    conv <- sf::st_sf(conv, crs = crs)
  } else {
    stop("Object type not supported (must be POLYGON or MULTIPOLYGON)")
  }
  
  # Add the associated data if requested
  if (base::isTRUE(add_data)) {
    r_conv <- sf_data
    r_conv$geometry <- conv$geometry
  } else {
    r_conv <- conv
  }
  
  # Return a list of POLYGON objects with shifted longitudes
  return(r_conv)
}

#' Split polygons into two if crossing the dateline (antimeridian)
#' 
#' @param sp_data SpatialPolygon or SpatialPolygonDataFrame object
#' @param crs CRS as a proj4string or EPSG code, default EPSG:4326 (datum=WGS84)
#' @param unique_inst Option to remove duplicate polygons (with the same coordinates), default TRUE
#' @param to_sp Option to convert POLYGON object into SpatialPolygon*, default TRUE
#' @return SpatialPolygon or SpatialPolygonDataFrame split along the antimeridian 
#' @export
antimeridian_wrapping <- function(polygon_data, crs = 4326, unique_inst = TRUE, to_sp = TRUE) {
  # Convert to sf object if needed
  if (!is(polygon_data, 'sf')) {
    if (isTRUE(unique_inst)) {
      all_coords <- base::lapply(polygon_data@polygons, function(x) {
        base::lapply(x@Polygons, function(x) {
          sp::coordinates(x)
        })
      })
      polygon_data <- polygon_data[!base::duplicated(all_coords),]
    }
    sf_data <- sf::st_as_sf(polygon_data)
    if (!isTRUE(sf::st_is_longlat(sf_data))) {
      stop("Projected coordinates not allowed, please choose a different projection to avoid the antimeridian problem")
    }
  } else {
    sf_data <- polygon_data
  }
  
  # Check if longitudes go from -180 to 180 rather than 0 to 360
  if (sf::st_bbox(sf_data)[['xmin']] > 0) {
    polygon_data_conv <- convert_from_lon_360(sf_data, crs = crs, add_data = FALSE)
    warning("Polygon coordinates [0;360] have been converted to [-180;180]")
  }
  
  # Apply the function
  sf_split <- sf::st_wrap_dateline(sf_data)
  
  # Revert sf objects back to sp
  if (isTRUE(to_sp)) {
    polygon_data_conv <- sf::as_Spatial(sf_split)
  } else {
    polygon_data_conv <- sf_split
  }
  
  return(polygon_data_conv)
}
