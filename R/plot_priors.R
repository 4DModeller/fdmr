#' Plot line comparison for stdev etc
#'
#' @param data Parsed model output
#' @param to_plot Type of data to plot, "Range for f" etc
#'
#' @return ggplot2::ggplot
#' @keywords internal
plot_line_comparison <- function(data, to_plot, title) {
  ar1_data <- purrr::map(data, function(x) as.data.frame(x$pars[[to_plot]]))
  single_df <- dplyr::bind_rows(ar1_data, .id = "Run")
  if (nrow(single_df) == 0) {
    return("No pars data.")
  }

  ggplot2::ggplot(single_df, ggplot2::aes(x = x, y = y, color = Run)) +
    ggplot2::geom_line() +
    ggplot2::ggtitle(title) +
    ggplot2::theme(text = ggplot2::element_text(size = 16))
}


#' Plot AR(1)
#'
#' @param data Parsed model output
#' @param to_plot Type of data to plot, "Range for f" etc
#'
#' @return ggplot2::ggplot
#' @keywords internal
plot_ar1 <- function(data) {
  ar1_data <- purrr::map(data, function(x) as.data.frame(x$pars$`GroupRho for f`))
  single_df <- dplyr::bind_rows(ar1_data, .id = "Run")
  if (nrow(single_df) == 0) {
    return("No pars data.")
  }

  ggplot2::ggplot(single_df, ggplot2::aes(x = x, y = y, color = Run)) +
    ggplot2::geom_line() +
    ggplot2::theme(text = ggplot2::element_text(size = 16))
}

#' Create boxplots from priors run data
#'
#' @param data
#'
#' @return graphics::boxplot
#' @keywords internal
plot_priors_boxplot <- function(data) {
  # TODO - I'm sure this can be done in a nicer functional way
  fitted_mean_post <- purrr::map(data, function(x) x$fitted_mean_post)
  names(fitted_mean_post) <- purrr::map(seq(1, length(data)), function(x) paste("Run", x))

  post_rate <- cbind.data.frame(fitted_mean_post)
  graphics::boxplot(post_rate, xlab = "Prior scenario", ylab = "Fitted values")
}

#' Plot density function
#'
#'
#' @param data Parsed model outputs
#' @param measurement_data Measurement data
#'
#' @return ggplot2::ggplot
#' @keywords internal
plot_priors_density <- function(data, measurement_data) {
  # Can this be done in a cleaner way? Just create a dataframe from the lists?
  fitted_values <- unlist(purrr::map(data, function(x) x$fitted_mean_post))
  run_strings <- unlist(purrr::map(seq(1, length(data)), function(x) paste("Run", x)))

  post_rate <- base::cbind.data.frame(
    "Prior scenario" = rep(run_strings, each = nrow(measurement_data)),
    "Fitted values" = fitted_values
  )

  ggplot2::ggplot(post_rate, ggplot2::aes(x = `Fitted values`, color = `Prior scenario`)) +
    ggplot2::geom_density() +
    ggplot2::theme(text = ggplot2::element_text(size = 16))
}


#' Plot Deviance Information Criterion (DIC) values
#'
#' @param data
#'
#' @return ggplot2::ggplot
#' @keywords internal
plot_dic <- function(data) {
  infocri <- base::cbind.data.frame(
    priors = unlist(purrr::map(seq(1, length(data)), function(x) paste("Run", x))),
    DIC = unlist(purrr::map(data, function(x) x$dic))
  )

  infocri$priors <- base::as.factor(infocri$priors)

  ggplot2::ggplot(infocri, ggplot2::aes(x = priors, y = DIC)) +
    ggplot2::geom_point() +
    ggplot2::theme(text = ggplot2::element_text(size = 16))
}
