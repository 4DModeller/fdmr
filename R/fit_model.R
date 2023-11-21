#' Fit a model using inlabru::bru
#'
#' @param y outcome variable name
#' @param fixed_x  list of fixed effects variable names
#' @param process_x  list of process effects variable names
#' @param process_coords list of coordinates associated with process
#' @param data data.frame of data
#' @param family gaussian or poisson
#'
#' @export
fit_model <- function(y, fixed_x, process_x, process_coords, data, time_variable = "time", family = "gaussian") {
    loadNamespace("inlabru")
    loadNamespace("INLA")
    # y : str - outcome variable name
    # fixed_x : list - list of fixed effects variable names
    # process_x : list - list of process effects variable names
    # process_coords : list - list of coordinates associated with process
    # data : data.frame - data.frame of data

    # TODO : make fixed effects formula string

    # TODO : make process effects formula string
    # If it's not a list then we create one so we can iterate over it
    if (!is.list(process_x)) {
        process_x <- list(process_x)
    }

    pe_effects <- list()
    for (process in process_x) {
        # TODO - can add more params to the function signature and unpack them here
        # We want the equivalent of **mesh_params here
        initial_range <- 0.1
        mesh <- fmesher::fm_mesh_2d_inla(loc = process_coords)

        group_index <- data[[time_variable]]
        n_groups <- length(unique(group_index))
        # Create the spde
        spde <- INLA::inla.spde2.pcmatern(
            mesh = mesh,
            prior.range = c(initial_range, 0.5),
            prior.sigma = c(1, 0.01)
        )

        rhoprior <- list(theta = list(prior = "pccor1", param = c(0, 0.9)))

        tryCatch({
            sp::coordinates(process_coords) <- c("LONG", "LAT")
        })

        # Coordinates here is just read magically, it's not an object
        effect_str <- f(
            main = coordinates,
            model = spde,
            family = family,
            E = process,
            group = group_index,
            ngroup = n_groups,
            control.group = list(
                model = 'ar1',
                hyper = rhoprior
            )
        )

        effect_str
    }

    # TODO : make process mesh for specific process

    # TODO : make spde using mesh for specific process

    # TODO : make specific formula string for this specific process

    # TODO : combine all formula strings to make main formula string
    formula <- "y ~ 1 +"
    formula <- paste(formula, fe_effects, sep = "")
    formula <- paste(formula, pe_effects, sep = "")
    # print created formula for user inspection
    print(formula)

    inlabru::bru(formula, data, family = family)
}
