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
