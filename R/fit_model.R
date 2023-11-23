#' Fit a model using inlabru::bru
#'
#' @param y outcome variable name
#' @param data Spatial data
#' @param process_coords list of coordinates associated with process
#' @param data data.frame of data
#' @param family gaussian or poisson
#' @param latitude_col name of latitude column
#' @param longitude_col name of longitude column
#'
#' @export
fit_model <- function(
    y,
    data,
    process_coords,
    time_variable = "time",
    family = "gaussian",
    latitude_col = "LAT",
    longitude_col = "LONG") {
    library("inlabru")
    library("INLA")

    initial_range <- diff(range(process_coords[, longitude_col])) / 3
    max_edge <- initial_range / 2

    print("Creating mesh...")
    mesh <- fmesher::fm_mesh_2d(
        loc = process_coords,
        max.edge = c(1, 2) * max_edge,
        offset = c(initial_range, initial_range),
        cutoff = max_edge / 7
    )

    print("Creating SPDE...")
    spde <- INLA::inla.spde2.pcmatern(
        mesh = mesh,
        prior.range = c(initial_range, 0.5),
        prior.sigma = c(1, 0.01)
    )

    rhoprior <- base::list(theta = list(prior = "pccor1", param = c(0, 0.9)))

    print("Running model...")
    model_out <- inlabru::bru(
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
        inlabru::like(formula = data[[y]] ~ space, family = "gaussian", data = data),
        inlabru::like(formula = data[[y]] ~ beta_u * space + spacetime, family = "gaussian", data = data)
    )

    print("Model run complete.")
    model_out
}
