#' Parses inlabru::bru model output to create a list of model parameters
#'
#' @param model_output Output from running inlabru::bru
#' @param measurement_data Measurement data
#'
#' @return list
#' @keywords internal
parse_model_output_bru <- function(model_output, measurement_data) {
    fitted_mean_post <- model_output$summary.fitted.values$mean[seq_len(nrow(measurement_data))]
    fitted_sd_post <- model_output$summary.fitted.values$sd[seq_len(nrow(measurement_data))]

    random_effect_fields <- model_output$summary.random$f$mean
    sd_post <- model_output$summary.random$f$sd
    fixed_mean <- model_output$summary.fixed$mean

    dic <- model_output$dic$dic
    pars <- model_output$marginals.hyperpar

    parsed_output <- list(
        fitted_mean_post = fitted_mean_post,
        fitted_sd_post = fitted_sd_post,
        random_effect_fields = random_effect_fields,
        sd_post = sd_post,
        fixed_mean = fixed_mean,
        dic = dic,
        pars = pars
    )

    return(parsed_output)
}


#' Parse model output to create a list of model parameters
#'
#' @param model_output Data returned by model
#' @param measurement_data Measurement data
#' @param model_type Type of model, we currently support inlabru
#'
#' @return list
#' @export
parse_model_output <- function(model_output, measurement_data, model_type = "inlabru") {
    if (model_type == "inlabru") {
        return(parse_model_output_bru(model_output = model_output, measurement_data = measurement_data))
    }
}


#' Create a prediction field from the parsed model output and the mesh
#'
#' @param mesh INLA mesh
#' @param plot_type Type of plot to create, "predicted_mean_fields" etc
#' @param data_type Type of data, "poisson" etc
#' @param var_a Data for variable a, required for "predicted_mean_fields" and "random_effect_fields"
#' @param var_b Data for variable b, required for "predicted_mean_fields"
#'
#' @return data.frame
#' @export
create_prediction_field <- function(mesh,
                                    plot_type = "predicted_mean_fields",
                                    data_type = "poisson",
                                    var_a = NULL,
                                    var_b = NULL) {
    valid_plots <- c("predicted_mean_fields", "random_effect_fields")
    if (!(plot_type %in% valid_plots)) {
        stop("Invalid plot type, select from ", valid_plots)
    }

    valid_data_types <- c("poisson", "gaussian")
    if (!(data_type %in% valid_data_types)) {
        stop("Invalid data type, select from ", valid_data_types)
    }

    if (plot_type == "predicted_mean_fields" && is.null(var_b)) {
        stop("var_b must be provided for predicted_mean_fields plot")
    }

    mod_proj <- fmesher::fm_evaluator(mesh)
    xy_grid <- base::expand.grid(mod_proj$x, mod_proj$y)
    A_proj <- INLA::inla.spde.make.A(mesh = mesh, loc = as.matrix(xy_grid))

    if (plot_type == "predicted_mean_fields") {
        if (data_type == "poisson") {
            z <- base::exp(base::as.numeric(A_proj %*% var_a[1:mesh$n]) + base::sum(var_b))
        } else {
            z <- base::as.numeric(A_proj %*% var_a[1:mesh$n]) + base::sum(var_b)
        }
    } else {
        z <- var_a[1:mesh$n]
    }

    base::data.frame(x = xy_grid[, 1], y = xy_grid[, 2], z = z)
}


create_raster <- function(dataframe, crs) {
    raster::rasterFromXYZ(dataframe, crs = crs)
}
