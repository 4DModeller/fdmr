# This function taken from https://groups.google.com/g/r-inla-discussion-group/c/z1n1exlZrKM/m/8vYNr2D8BwAJ
#' Convert an INLA mesh to a SpatialPolygonsDataFrame
#'
#' @param mesh Mesh
#' @param crs Coordinate Reference System in proj4 format. Required if mesh has no CRS defined.
#'
#' @return SpatialPolygonsDataFrame
#' @export
mesh_to_spatial <- function(mesh, crs) {
  is_geocentric <- "geocent" %in% fmesher::fm_proj4string(crs)
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
      proj4string = crs
    ),
    data = as.data.frame(mesh$graph$tv[, c(1, 3, 2), drop = FALSE]),
    match.ID = FALSE
  )
}
