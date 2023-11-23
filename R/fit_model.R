#' Fit a model using inlabru::bru
#'
#' @param y outcome variable name
#' @param data Spatial data
#' @param locations list of coordinates associated with process
#' @param space_variables list of spatial variables
#' @param spacetime_variables list of spacetime variables
#' @param family gaussian or poisson
#' @param latitude_col name of latitude column
#' @param longitude_col name of longitude column
#' @param time_variable name of time variable in data
#' @param initial_range_mesh Initial range in mesh creation
#' @param max_edge_mesh Max edge value used in mesh creation
#'
#' @return list of model output and mesh
#' @export
fit_model <- function(
    y,
    data,
    locations,
    space_variables,
    spacetime_variables,
    time_variable = "time",
    family = "gaussian",
    latitude_col = "LAT",
    longitude_col = "LONG",
    initial_range_mesh = 0.1,
    max_edge_mesh = 0.05,
    verbose = FALSE) {
    library("inlabru")
    library("INLA")

    print("Creating mesh...")
    mesh <- fmesher::fm_mesh_2d_inla(
        loc = locations
    )

    print("Creating SPDE...")
    spde <- INLA::inla.spde2.pcmatern(
        mesh = mesh,
        prior.range = c(initial_range_mesh, 0.5),
        prior.sigma = c(1, 0.01)
    )

    space_variables <- append(space_variables, y)
    spacetime_variables <- append(spacetime_variables, y)

    rhoprior <- base::list(theta = list(prior = "pccor1", param = c(0, 0.9)))

    print("Running model...")
    model_output <- inlabru::bru(
        components = ~ space(coordinates, model = spde) +
            spacetime(
                coordinates,
                model = spde,
                group = time,
                control.group = list(
                    model = "ar1",
                    hyper = rhoprior
                )
            ) +
            beta_u(1, prec.linear = 1),
        inlabru::like(formula = data[[y]] ~ space, family = "gaussian", data = data[space_variables]),
        inlabru::like(formula = data[[y]] ~ beta_u * space + spacetime, family = "gaussian", data = data[spacetime_variables]),
        options = list(
            verbose = verbose
        )
    )

    print("Model run complete.")
    list(model_output = model_output, mesh = mesh)
}
