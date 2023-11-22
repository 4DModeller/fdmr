#' Fit a model using inlabru::bru
#'
#' @param y outcome variable name
#' @param fixed_x  list of fixed effects variable names
#' @param process_x  list of process effects variable names
#' @param process_coords list of coordinates associated with process
#' @param data data.frame of data
#' @param family gaussian or poisson
#' @param latitude_col name of latitude column
#' @param longitude_col name of longitude column
#'
#' @export
fit_model <- function(
    y,
    fixed_x,
    process_x,
    process_coords,
    data,
    time_variable = "time",
    family = "gaussian",
    latitude_col = "LAT",
    longitude_col = "LONG") {
    library("inlabru")
    library("INLA")
    # y : str - outcome variable name
    # fixed_x : list - list of fixed effects variable names
    # process_x : list - list of process effects variable names
    # process_coords : list - list of coordinates associated with process
    # data : data.frame - data.frame of data

    initial_range <- 0.1
    max_edge <- initial_range / 8

    # Create another mesh
    mesh_space <- fmesher::fm_mesh_2d_inla(
        loc = process_coords
    )

    space_values <- data
    spacetime_values <- data

    mesh_spacetime <- fmesher::fm_mesh_2d_inla(
        loc = process_coords
    )

    # Build the two SPDEs / whatever you want
    space_spde <- INLA::inla.spde2.pcmatern(
        mesh = mesh_space,
        prior.range = c(initial_range, 0.5),
        prior.sigma = c(1, 0.01)
    )

    # spde <- INLA::inla.spde2.pcmatern(
    #     mesh = mesh_spacetime,
    #     prior.range = c(initial_range, 0.5),
    #     prior.sigma = c(1, 0.01)
    # )

    rhoprior <- base::list(theta = list(prior = "pccor1", param = c(0, 0.9)))
    time_mesh <- mesh_space


    model_out <- inlabru::bru(
        components = ~ space(geometry, model = space_spde) +
            spacetime(
                geometry,
                model = space_spde,
                group = time,
                control.group = list(
                    model = "ar1",
                    hyper = rhoprior
                ),
                group_mapper = bru_mapper(time_mesh)
            ) +
            beta_u(1, prec.linear = 1, marginal = bru_mapper_marginal(qexp, rate = 1)),
        like(formula = space_values ~ space, family = "gaussian"),
        like(formula = spacetime_values ~ beta_u * space + spacetime, family = "gaussian")
    )

    model_out
}
