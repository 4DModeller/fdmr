#' Parses inlabru::bru model output
#'
#' @param model_output Output from running inlabru::bru
#'
#' @return list
#' @keywords internal
parse_model_bru <- function(model_output) {
    fitted_mean_post <- model_output$summary_fitted_values$mean[seq_len(nrow(data))]
    fitted_sd_post <- model_output$summary_fitted_values$mean[seq_len(nrow(data))]

    mean_post <- model_output$summary_random$f$mean
    sd_post <- model_output$summary_random$f$sd
    fixed_mean <- model_output$summary_fixed$mean
    dic <- model_output$dic$dic
    pars <- model_output$marginals_hyperpar

    return(list(
        fitted_mean_post = fitted_mean_post,
        fitted_sd_post = fitted_sd_post,
        mean_post = mean_post,
        sd_post = sd_post,
        fixed_mean = fixed_mean,
        dic = dic,
        pars = pars
    ))
}


#' Parse model output
#'
#' @param model_output Data returned by model
#' @param model_type Type of model, we currently support inlabru
#'
#' @return list
#' @export
parse_model <- function(model_output, model_type = "inlabru") {
    if (model_type == "bru") {
        return(parse_model_bru(model_output = model_output))
    }
}
