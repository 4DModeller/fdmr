% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shiny_mapper.R
\name{raster_mapping_app}
\alias{raster_mapping_app}
\title{Shiny app for plotting raster and polygon data on a leaflet map.}
\usage{
raster_mapping_app(
  raster_data = NULL,
  polygon_data = NULL,
  date_format = NULL,
  palette = NULL
)
}
\arguments{
\item{raster_data}{Raster data in the form or a RasterStack or RasterBrick}

\item{polygon_data}{Polygon data to plot on map}

\item{date_format}{A date format that will be passed in to lubridate::as_date}

\item{palette}{Colour palette to use}
}
\value{
shinyApp
}
\description{
Shiny app for plotting raster and polygon data on a leaflet map.
}
\keyword{internal}
