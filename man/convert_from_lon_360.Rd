% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_coords.R
\name{convert_from_lon_360}
\alias{convert_from_lon_360}
\title{Convert longitudes from 0 to 360 degrees to -180 to 180 degrees}
\usage{
convert_from_lon_360(
  sf_data,
  crs = 4326,
  add_data = TRUE,
  longitude_column = "LONG"
)
}
\arguments{
\item{sf_data}{An sf object; does not accept SpatialPolygon* objects}

\item{crs}{CRS as a proj4string or EPSG code}

\item{add_data}{Select if data associated with the object are carried forward by the transformed version, defaults to FALSE}

\item{longitude_column}{Name of longitude, defaults to LONG}
}
\value{
polygons with converted coordinates
}
\description{
Convert longitudes from 0 to 360 degrees to -180 to 180 degrees
}
