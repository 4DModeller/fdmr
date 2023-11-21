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

    # TODO : make fixed effects formula string

    # TODO : make process effects formula string
    # If it's not a list then we create one so we can iterate over it
    # if (!is.list(process_x)) {
    #     process_x <- list(process_x)
    # }

    fe_formula <- ""
    if (!is.null(fixed_x)) {
        fe_formula <- stringr::str_c(fixed_x, collapse = " + ")
    }

    # Give it a default value and try and update it from the location data
    initial_range <- 0.5

    # <- tryCatch({
    #     diff(range(process_coords[, longitude_col])) / 5
    # }, error = function(e) {
    #     0.5
    # })

    max_edge <- initial_range / 8

    mesh <- fmesher::fm_mesh_2d_inla(
        loc = process_coords,
        max.edge = c(1, 2) * max_edge,
        offset = c(initial_range / 4, initial_range),
        cutoff = max_edge / 7
    )

    prior_range <- initial_range
    spde <- INLA::inla.spde2.pcmatern(
        mesh = mesh,
        prior.range = c(prior_range, 0.5),
        prior.sigma = c(1, 0.01)
    )

    rhoprior <- base::list(theta = list(prior = "pccor1", param = c(0, 0.9)))
    group_index <- data[[time_variable]]
    n_groups <- length(unique(data[[time_variable]]))

    sp::coordinates(data) <- c(longitude_col, latitude_col)

    formula <- eval(parse(text = paste0(y, "~ 0 + Intercept(1) + f(main = coordinates, model = spde, group = group_index, ngroup = n_groups, control.group = list(model = 'ar1', hyper = rhoprior))")))

    inlabru_model <- inlabru::bru(formula,
        data = data,
        family = "poisson",
        E = data[[process_x]],
        control.family = list(link = "log"),
        options = list(
            verbose = TRUE
        )
    )

    inlabru_model
}
