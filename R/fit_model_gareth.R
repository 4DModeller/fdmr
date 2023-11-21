#' Title
#'
#' @param location_data Location data as data.frame
#' @param max_edge Max edge value(s) - optional
#' @param offset Offset value(s) - optional
#' @param cutoff Cutoff value(s) - optional
#'
#' @return inla.mesh object
#' @keywords internal
create_mesh <- function(location_data, max_edge = NULL, offset = NULL, cutoff = NULL, ...) {
    fmesher::fm_mesh_2d_inla(loc = location_data, max.edge = max_edge, offset = offset, cutoff = cutoff, ...)
}


#' Create the formula
#'
#' @param mesh Mesh object if needed
#'
#' @return inla.mesh object
#' @keywords internal
create_formula <- function(mesh, ...) {
    ## TODO - John to fill this out
}

#' Fit the model itself
#'
#' @param location_data Location data as data.frame
#' @param max_edge Max edge value(s) - optional
#' @param offset Offset value(s) - optional
#' @param cutoff Cutoff value(s) - optional
#'
#' @return inla.mesh object
#' @keywords internal
model_fit <- function(formula, spatial_data, family = "gaussian", ...) {
  inlabru::bru(formula, family=family, data=spatial_data)
}

#' Fit the model
#'
#' @param location_data Location data as data.frame
#' @param max_edge Max edge value(s) - optional
#' @param offset Offset value(s) - optional
#' @param cutoff Cutoff value(s) - optional
#'
#' @return inla.mesh object
#' @export
fit_model <- function(location_data, exposure_parameter = NULL, spatial_data, family = "gaussian", mesh = NULL, formula = NULL, mesh_params = NULL, formula_params = NULL, model_params = NULL) {
    mesh <- create_mesh(location_data)
    formula <- create_formula(mesh)
    model_fit(formula, family = family, spatial_data = spatial_data)
}
