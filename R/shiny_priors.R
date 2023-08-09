#' Interactively set and see the result of different priors
#'
#' @param spatial_data Spatial data
#' @param measurement_data Measurement data
#' @param mesh INLA mesh
#' @param inla_exposure_param Exposure parameter for family = 'poisson' passed to inlabru::bru and then to INLA::inla. Must be column in measurement data.
#'
#' @importFrom INLA f
#'
#' @return shiny::app
#' @keywords internal
priors_shiny <- function(spatial_data,
                         measurement_data,
                         mesh,
                         prior_range = NULL,
                         ps_range = NULL,
                         prior_sigma = NULL,
                         pg_sigma = NULL,
                         prior_ar1 = NULL,
                         pg_ar1 = NULL) {
    require_packages(packages = "INLA")
    loadNamespace("INLA")

    initial_equation_val <- "formula <- model_var ~ 0 + Intercept"
    features <- names(measurement_data)
    if (is.null(features)) {
        stop("We require the columns of measurement_data to have the names of the features to use in the model.")
    }

    plot_choices <- c("Range", "Stdev", "AR(1)", "Boxplot", "Density", "DIC")

    # loading_gif <- system.file("logo/4DMlogo_loading.gif", package = "fdmr")

    # Define UI for application that draws a histogram
    ui <- shiny::fluidPage(
        # Use this function somewhere in UI
        shinybusy::add_busy_spinner(spin = "folding-cube", margins = c(20, 20)),
        # shinybusy::add_busy_gif(loading_gif, height = 100, width = 100, position = "top-right"),
        shiny::headerPanel(title = "Investigating priors"),
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                shiny::h3("Priors"),
                shiny::sliderInput(
                    inputId = "prior_range",
                    label = "Spatial range:",
                    min = 0.05, value = 0.05, max = 1
                ),
                shiny::sliderInput(
                    inputId = "ps_range",
                    label = "Range probabilty:",
                    min = 0.1, value = 0.1, max = 1
                ),
                shiny::sliderInput(
                    inputId = "prior_sigma",
                    label = "Standard deviation:",
                    min = 0.05, value = 0.05, max = 2
                ),
                shiny::sliderInput(
                    inputId = "pg_sigma",
                    label = "Standard dev. probability:",
                    min = 0.1, value = 0.2, max = 1
                ),
                shiny::h3("Temporal priors"),
                shiny::sliderInput(
                    inputId = "prior_ar1",
                    label = "Alpha:",
                    min = -1, value = -0.2, max = 1
                ),
                shiny::sliderInput(
                    inputId = "pg_ar1",
                    label = "PG Alpha:",
                    min = 0, value = 0.8, max = 1
                ),
                shiny::textOutput(outputId = "status")
            ),
            shiny::mainPanel(
                shiny::tabsetPanel(
                    type = "tabs",
                    shiny::tabPanel(
                        "Features",
                        shiny::selectInput(inputId = "model_var", label = "Model variable", choices = features),
                        shiny::selectInput(inputId = "exposure_param", label = "Exposure param", choices = features),
                        shiny::checkboxGroupInput(inputId = "features", label = "Features", choices = features),
                        shiny::checkboxInput(inputId = "f_func", label = "Add f()", value = FALSE),
                        shiny::actionButton(inputId = "clear", label = "Clear"),
                        shiny::fluidRow(
                            shiny::h2("Formula"),
                            shiny::textOutput(outputId = "final_equation"),
                            style = "height:20vh;"
                        )
                    ),
                    shiny::tabPanel(
                        "Model",
                        shiny::fluidRow(
                            shiny::h2("Model output"),
                            shiny::textOutput(outputId = "comparison_output"),
                            style = "height:80vh;"
                        ),
                        shiny::actionButton(inputId = "run_model", label = "Run"),
                    ),
                    shiny::tabPanel(
                        "Plot",
                        shiny::h2("Plot output"),
                        shiny::selectInput(inputId = "plot_type", label = "Plot type:", choices = plot_choices, selected = plot_choices[1]),
                        shiny::plotOutput(outputId = "plot_model_out")
                    ),
                    shiny::tabPanel(
                        "Code",
                        shiny::selectInput(inputId = "select_run", label = "Select run:", choices = c()),
                        shiny::verbatimTextOutput(outputId = "code_out")
                    )
                )
            ),
        )
    )

    server <- function(input, output, session) {
        status_value <- shiny::reactiveVal("OK")

        run_no <- shiny::reactiveVal(0)
        model_vals <- shiny::reactiveValues(model_outputs = list(), parsed_outputs = list(), run_params = list())

        output$status <- shiny::renderText({
            paste("Status : ", status_value())
        })

        initial_equation <- shiny::reactive({
            stringr::str_replace(initial_equation_val, "model_var", input$model_var)
        })

        run_names <- shiny::reactive({
            names(model_vals$run_params)
        })

        shiny::observe({
            shiny::updateSelectInput(session = session, inputId = "select_run", choices = run_names())
        })

        shiny::observeEvent(input$features, {
            print(paste0("You have chosen: ", input$features))
        })

        shiny::observeEvent(input$clear, {
            shiny::updateCheckboxGroupInput(session = session, inputId = "features", choices = features, selected = NULL)
        })

        shiny::observeEvent(input$model_var, {
            shiny::updateTextInput(session = session, inputId = initial_equation, value = initial_equation())
        })

        spde <- shiny::reactive({
            INLA::inla.spde2.pcmatern(
                mesh = mesh,
                prior.range = c(input$prior_range, input$ps_range),
                prior.sigma = c(input$prior_sigma, input$pg_sigma)
            )
        })

        alphaprior <- shiny::reactive({
            base::list(theta = list(
                prior = "pccor1",
                param = c(input$prior_ar1, input$pg_ar1)
            ))
        })

        formula_str <- shiny::reactive({
            eval_str <- initial_equation()

            if (length(input$features > 0)) {
                # features_copy <- input$features
                # features_copy <- features_copy[features_copy != input$model_var]
                # if (length(features_copy == 0)) {
                #     return(initial_equation())
                # }
                chosen <- stringr::str_c(input$features, collapse = " + ")
                eval_str <- paste(eval_str, " + ", chosen)
            }

            f_func <- "f(
                main = coordinates,
                model = spde(),
                group = group_index,
                ngroup = n_groups,
                control.group = list(
                    model = 'ar1',
                    hyper = alphaprior())
                )"

            if (input$f_func) {
                eval_str <- paste(eval_str, " + ", f_func)
            }

            return(eval_str)
        })

        output$final_equation <- shiny::renderText({
            formula_str()
        })

        shiny::observeEvent(input$run_model, ignoreNULL = TRUE, {
            # TODO - what would be the best way of creating these?
            group_index <- measurement_data$week
            n_groups <- length(unique(measurement_data$week))

            formula <- eval(parse(text = formula_str()))

            tryCatch(
                expr = {
                    model_output <- inlabru::bru(formula,
                        data = measurement_data,
                        family = "poisson",
                        E = measurement_data[[input$exposure_param]],
                        control.family = list(link = "log"),
                        options = list(
                            verbose = FALSE
                        )
                    )

                    run_no(run_no() + 1)
                    model_vals$model_outputs[[run_no()]] <- model_output
                    model_vals$parsed_outputs[[run_no()]] <- parse_model_output(
                        model_output = model_output,
                        measurement_data = measurement_data
                    )

                    # Save the model run parameters
                    run_params <- list(
                        "prior_range" = input$prior_range,
                        "ps_range" = input$ps_range,
                        "prior_sigma" = input$prior_sigma,
                        "pg_sigma" = input$pg_sigma,
                        "prior_ar1" = input$prior_ar1,
                        "pg_ar1" = input$pg_ar1
                    )

                    run_label <- paste0("Run-", run_no())
                    model_vals$run_params[[run_label]] <- run_params
                },
                error = function(e) {
                    # TODO - write to logfile
                    status_value("INLA Error, check log.")
                }
            )
        })

        output$comparison_output <- shiny::renderPrint({
            if (length(model_vals$model_outputs) == 0) {
                "No model output."
            } else {
                # TODO - improve this output, some kind of table format for the parsed values?
                paste("We have ", run_no(), " successful model runs.")
                # model_vals$parsed_outputs
            }
        })

        model_plot <- shiny::eventReactive(input$plot_type, ignoreNULL = FALSE, {
            if (length(model_vals$parsed_outputs) == 0) {
                return()
            }

            data <- model_vals$parsed_outputs

            if (input$plot_type == "Range") {
                return(plot_line_comparison(
                    data = data,
                    to_plot = "Range for f",
                    title = "Range"
                ))
            } else if (input$plot_type == "Stdev") {
                return(plot_line_comparison(
                    data = data,
                    to_plot = "Stdev for f",
                    title = "Marginal standard deviation"
                ))
            } else if (input$plot_type == "AR(1)") {
                return(plot_line_comparison(
                    data = data,
                    to_plot = "GroupRho for f",
                    title = "AR(1)"
                ))
            } else if (input$plot_type == "Boxplot") {
                return(plot_priors_boxplot(data = data))
            } else if (input$plot_type == "Density") {
                return(plot_priors_density(
                    data = data,
                    measurement_data = measurement_data
                ))
            } else if (input$plot_type == "DIC") {
                return(plot_dic(data = data))
            }
        })

        output$plot_model_out <- shiny::renderPlot({
            model_plot()
        })

        output$code_out <- shiny::reactive({
            if (is.null(run_names())) {
                return()
            }

            params <- model_vals$run_params[[input$select_run]]

            paste0(
                "spde <- INLA::inla.spde2.pcmatern(
                mesh = mesh,
                prior.range = c(", params[["prior_range"]], ",", params[["ps_range"]], "),
                prior.sigma = c(", params[["prior_sigma"]], ",", params[["pg_sigma"]], ")
            )", "\n\n",
                paste0(
                    "alphaprior <- list(theta = list(
                prior = 'pccor1',
                param = c(", params[["prior_ar1"]], ",", params[["pg_ar1"]], ")
            )", "\n\n",
                    paste0("model_output <- inlabru::bru(formula,
                        data = measurement_data,
                        family = 'poisson',
                        E = measurement_data[[", input$exposure_param, "]],
                        control.family = list(link = 'log'),
                        options = list(
                            verbose = FALSE
                        )
                    )")
                )
            )
        })
    }

    shiny::shinyApp(ui = ui, server = server)
}

#' Interactively set and see the result of different priors
#'
#' @param spatial_data Spatial data
#' @param measurement_data Measurement data
#' @param mesh INLA mesh
#'
#' @return shiny::app
#' @export
interactive_priors <- function(spatial_data, measurement_data, mesh = NULL) {
    shiny::runApp(priors_shiny(spatial_data = spatial_data, measurement_data = measurement_data, mesh = mesh))
}

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
    graphics::boxplot(post_rate, xlab = "Prior scenario", ylab = "Rate estimates")
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
    rate_estimates <- unlist(purrr::map(data, function(x) x$fitted_mean_post))
    run_strings <- unlist(purrr::map(seq(1, length(data)), function(x) paste("Run", x)))

    post_rate <- base::cbind.data.frame(
        "Prior scenario" = rep(run_strings, each = nrow(measurement_data)),
        "Rate estimates" = rate_estimates
    )

    ggplot2::ggplot(post_rate, ggplot2::aes(x = `Rate estimates`, color = `Prior scenario`)) +
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
