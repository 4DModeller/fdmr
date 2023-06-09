# This function taken from https://groups.google.com/g/r-inla-discussion-group/c/z1n1exlZrKM/m/8vYNr2D8BwAJ
#' Convert an INLA mesh to a SpatialPolygonsDataFrame
#'
#' @param mesh Mesh
#' @param crs Coordinate Reference System in proj4 format. Required if mesh has no CRS defined.
#'
#' @return SpatialPolygonsDataFrame
#' @export
mesh_to_spatial <- function(mesh, crs = NULL) {
    # TODO - update this function so it uses the newer fmsesher functions available
    # in inlabru
    # Try and read the CRS from the mesh
    crs <- INLA::inla.CRS(INLA::inla.CRSargs(mesh$crs))

    if (is.null(crs)) {
        stop("Cannot read CRS from mesh, please pass in crs argument")
    }

    # crs_new <- inlabru::fm_crs(inlabru::fm_CRSargs(mesh$crs))
    isgeocentric <- identical(INLA::inla.as.list.CRS(crs)[["proj"]], "geocent")

    if (isgeocentric || (mesh$manifold == "S2")) {
        stop(paste0(
            "'sp' doesn't support storing polygons in geocentric coordinates.\n",
            "Convert to a map projection with inla.spTransform() before calling inla.mesh2sp()."
        ))
    }

    triangles <- sp::SpatialPolygonsDataFrame(
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
    # vertices <- sp::SpatialPoints(mesh$loc[, 1:2, drop = FALSE], proj4string = crs)
    # list(triangles = triangles, vertices = vertices)

    return(triangles)
}
