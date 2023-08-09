#' Parses inlabru::bru model output
#'
#' @param model_output Output from running inlabru::bru
#' @param measurement_data Measurement data
#'
#' @return list
#' @keywords internal
parse_model_output_bru <- function(model_output, measurement_data) {
    fitted_mean_post <- model_output$summary.fitted.values$mean[seq_len(nrow(measurement_data))]
    fitted_sd_post <- model_output$summary.fitted.values$mean[seq_len(nrow(measurement_data))]

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


#' Parse model output
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
