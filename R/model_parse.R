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

    mean_post <- model_output$summary.random$f$mean
    sd_post <- model_output$summary.random$f$sd
    fixed_mean <- model_output$summary.fixed$mean

    dic <- model_output$dic$dic
    pars <- model_output$marginals.hyperpar

    parsed_output <- list(
        fitted_mean_post = fitted_mean_post,
        fitted_sd_post = fitted_sd_post,
        mean_post = mean_post,
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
#' @param var_a Variable a data
#' @param var_b Variable b data
#' @param mesh INLA mesh
#' @param crs CRS as a proj4string
#'
#' @return data.frame
#' @export
create_prediction_field <- function(var_a, var_b, mesh, crs = NULL) {
    if (is.null(crs)) {
        read_crs <- mesh$crs$input
        if (is.null(read_crs)) {
            warning("Cannot read CRS from mesh, pass proj4string to crs otherwise we'll assume crs = +proj=longlat +datum=WGS84")
            crs <- "+proj=longlat +datum=WGS84"
        } else {
            crs <- read_crs
        }
    }

    mod_proj <- fmesher::fm_evaluator(mesh, crs = crs)
    xy_grid <- base::expand.grid(mod_proj$x, mod_proj$y)
    A_proj <- INLA::inla.spde.make.A(mesh = mesh, loc = as.matrix(xy_grid))
    z <- base::exp(base::as.numeric(A_proj %*% var_a[1:mesh$n]) + base::sum(var_b))
    base::data.frame(x = xy_grid[, 1], y = xy_grid[, 2], z = z)
}


create_raster <- function(dataframe, crs) {
    raster::rasterFromXYZ(dataframe, crs = crs)
}
