% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meshing.R
\name{mesh_to_spatial}
\alias{mesh_to_spatial}
\title{Convert an INLA mesh to a SpatialPolygonsDataFrame}
\usage{
mesh_to_spatial(mesh, crs = NULL)
}
\arguments{
\item{mesh}{Mesh}

\item{crs}{Coordinate Reference System in proj4 format. Required if mesh has no CRS defined.}
}
\value{
SpatialPolygonsDataFrame
}
\description{
Convert an INLA mesh to a SpatialPolygonsDataFrame
}
