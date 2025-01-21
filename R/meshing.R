# This function taken from https://groups.google.com/g/r-inla-discussion-group/c/z1n1exlZrKM/m/8vYNr2D8BwAJ
#' Convert an INLA mesh to a SpatialPolygonsDataFrame
#'
#' @param mesh Mesh object from INLA or fmesher (only R2 manifolds are valid)
#' @param crs Coordinate Reference System as proj4string, does not support geocentric coordinates
#'
#' @return SpatialPolygonsDataFrame with mesh triangles
#' @export
mesh_to_spatial <- function(mesh, crs) {
  if (!is.character(crs)) {
    stop("crs must be a proj4string")
  }

  is_geocentric <- "geocent" %in% crs
  if (is_geocentric || mesh$manifold == "S2") {
    stop(paste0(
      "'sp' doesn't support storing polygons in geocentric coordinates.\n",
      "Convert to a map projection with inla.spTransform() before calling inla.mesh2sp()."
    ))
  }

  sp::SpatialPolygonsDataFrame(
    Sr = sp::SpatialPolygons(
      lapply(
        seq_len(nrow(mesh$graph$tv)),
        function(x) {
          tv <- mesh$graph$tv[x, , drop = TRUE]
          sp::Polygons(
            list(sp::Polygon(mesh$loc[tv[c(1, 3, 2, 1)],
              1:2,
              drop = FALSE
            ])),
            ID = x
          )
        }
      ),
      proj4string = sp::CRS(crs)
    ),
    data = as.data.frame(mesh$graph$tv[, c(1, 3, 2), drop = FALSE]),
    match.ID = FALSE
  )
}
