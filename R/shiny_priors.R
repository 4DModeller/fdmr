#' Interactively set and see the result of different priors
#'
#' @param spatial_data Spatial data
#' @param measurement_data Measurement data
#' @param time_variable Time variable in measurement_data
#' @param mesh INLA mesh
#'
#' @importFrom INLA f
#'
#' @return shiny::app
#' @keywords internal
priors_shiny <- function(spatial_data,
                         measurement_data,
                         time_variable,
                         mesh) {
    future::plan(future::multisession())

    got_coords <- has_coords(spatial_data = spatial_data)
    if (!got_coords) {
        stop("Please make sure you have set coordinates on spatial_data using sp::coordinates.")
    }

    # Text for priors help
    prior_range_text <- "A length 2 vector, with (range0, Prange) specifying that P(ρ < ρ_0)=p_ρ, where ρ is the spatial range of the random field. The spatial range ρ is defined as the distance at which the spatial correlation between two locations is approximately 0. 
                         P(ρ < ρ_0)=p_ρ indicates that the probability of ρ smaller than ρ_0 (range0) is p_ρ (Prange). Large values of Prange lead to a greater prior belief on small values of ρ."

    prior_sigma_text <- "A length 2 vector, with (sigma0, Psigma) specifying that P(σ > σ_0)=p_σ, 
                         where σ is the marginal standard deviation of the field. P(σ > σ_0)=p_σ indicates that the probability of σ greater than σ_0 (sigma0) is p_σ (Psigma).
                         Large values of Psigma lead to a greater prior belief on large values of σ."

    control_group_text <- "Temporal priors for the temporal autocorrelation parameter α are set using prior_alpha and pg_alpha, in the relation that P(α > prior_alpha) = pg_alpha, indicating that the probability of α greater than prior_alpha is pg_alpha.
                           Large values of pg_alpha lead to a greater prior belief on large values of α.
                           prior_alpha and pg_alpha are used to create alphaprior, which is then passed to the control.group argument, control.group = list(model = 'ar1', hyper = alphaprior).
                           It specifies that across time, the process evolves according to an AR(1) process where the prior for the autocorrelation parameter α is given by alphaprior. 
                           We define alphaprior with the prior 'pccor1', which is a Penalised Complexity (PC) prior for the temporal autocorrelation parameter α, with α = 1 indicating strong temporal dependence, and α = 0 indicating independence across time."
  
    citation_priors <- "Spatial and field prior explanation taken from https://rdrr.io/github/INBO-BMK/INLA/man/inla.spde2.pcmatern.html"
    citation_control_group <- "Prior explanation text modified from https://www.paulamoraga.com/book-geospatial/sec-geostatisticaldataexamplest.html"

    initial_equation_val <- "formula <- model_var ~ 0 + Intercept"
    features <- names(measurement_data)
    if (is.null(features)) {
        stop("We require the columns of measurement_data to have the names of the features to use in the model.")
    }

    # Logfile path
    timestamp_str <- lubridate::format_ISO8601(lubridate::now())
    log_filename <- paste0("priors_exploration_applog_", timestamp_str, ".txt")
    parameters_file <- paste0("priors_exploration_parameters_", timestamp_str, ".json")
    model_outputs_file <- paste0("priors_exploration_modelout_", timestamp_str, ".rds")

    log_folder <- fs::path(fs::path_home(), "fdmr", "logs")
    if (!fs::dir_exists(log_folder)) {
        fs::dir_create(log_folder)
    }

    log_filepath <- fs::path(log_folder, log_filename)
    parameters_filepath <- fs::path(log_folder, parameters_file)
    modeloutputs_filepath <- fs::path(log_folder, model_outputs_file)

    plot_choices <- c("Range", "Stdev", "AR(1)", "Boxplot", "Density", "DIC")

    # TODO - if we modularise the Shiny apps and setup a different directory
    # structure we can remove this
    got_internet <- curl::has_internet()
    if (got_internet) {
        busy_spinner <- shinybusy::add_busy_gif("https://raw.githubusercontent.com/4DModeller/logo/main/4DMlogo_loading.gif", height = 100, width = 100, position = "top-right")
    } else {
        busy_spinner <- shinybusy::add_busy_spinner(spin = "folding-cube", margins = c(20, 20))
    }

    # Define UI for application that draws a histogram
    ui <- shiny::fluidPage(
        # Use this function somewhere in UI
        busy_spinner,
        shiny::headerPanel(title = "Investigating priors"),
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                shiny::h3("Priors"),
                shiny::sliderInput(
                    inputId = "prior_range",
                    label = "range0:",
                    min = 0.05, value = 0.05, max = 1
                ),
                shiny::sliderInput(
                    inputId = "ps_range",
                    label = "Prange:",
                    min = 0.1, value = 0.1, max = 1
                ),
                shiny::sliderInput(
                    inputId = "prior_sigma",
                    label = "sigma0:",
                    min = 0.05, value = 0.05, max = 2
                ),
                shiny::sliderInput(
                    inputId = "pg_sigma",
                    label = "Psigma:",
                    min = 0.1, value = 0.2, max = 1
                ),
                shiny::h3("Temporal priors"),
                shiny::sliderInput(
                    inputId = "prior_ar1",
                    label = "prior_alpha:",
                    min = -1, value = -0.2, max = 1.0, step = 0.1,
                ),
                shiny::sliderInput(
                    inputId = "pg_ar1",
                    label = "pg_alpha:",
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
                            # shiny::conditionalPanel("output.gotoutput", shiny::h3("Hyperparameter summary")),
                            shiny::tableOutput(outputId = "hyper_param_out"),
                            # shiny::h3("Fixed summary"),
                            shiny::tableOutput(outputId = "fixed_out"),
                            style = "height:70vh;"
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
                    ),
                    shiny::tabPanel(
                        "Help",
                        shiny::h3("Help"),
                        shiny::h4("Spatial priors"),
                        shiny::p(prior_range_text),
                        shiny::h4("Field priors"),
                        shiny::p(prior_sigma_text),
                        shiny::h4("Temporal priors"),
                        shiny::p(control_group_text),
                        shiny::br(),
                        shiny::br(),
                        shiny::h4("Notes"),
                        shiny::p(citation_priors),
                        shiny::p(citation_control_group)
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

        output$gotoutput <- shiny::reactive({
            length(model_vals$model_outputs > 0)
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
                model = spde,
                group = group_index,
                ngroup = n_groups,
                control.group = list(
                    model = 'ar1',
                    hyper = alphaprior)
                )"

            if (input$f_func) {
                eval_str <- paste(eval_str, " + ", f_func)
            }

            eval_str
        })

        inla_formula <- shiny::reactive({
            group_index <- measurement_data[[time_variable]]
            n_groups <- length(unique(group_index))

            spde <- INLA::inla.spde2.pcmatern(
                mesh = mesh,
                prior.range = c(input$prior_range, input$ps_range),
                prior.sigma = c(input$prior_sigma, input$pg_sigma)
            )

            alphaprior <- list(theta = list(
                prior = "pccor1",
                param = c(input$prior_ar1, input$pg_ar1)
            ))

            eval(parse(text = formula_str()))
        })


        output$final_equation <- shiny::renderText({
            formula_str()
        })

        shiny::observeEvent(input$run_model, ignoreNULL = TRUE, {
            exposure_param_local <- input$exposure_param
            formula_local <- inla_formula()
            measurement_data_local <- measurement_data

            promise <- promises::future_promise(
                {
                    # Without loading INLA here we get errors
                    require("INLA")
                    inlabru::bru(formula_local,
                        data = measurement_data_local,
                        family = "poisson",
                        E = measurement_data_local[[exposure_param_local]],
                        control.family = list(link = "log"),
                        options = list(
                            verbose = FALSE
                        )
                    )
                },
                seed = TRUE
            )

            promises::then(promise,
                onFulfilled =
                    function(model_output) {
                        # Run the model
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

                        write_parameters(logfile = parameters_filepath, parameters = model_vals$run_params)
                        saveRDS(model_vals$parsed_outputs, file = modeloutputs_filepath)
                    },
                onRejected = function(err) {
                    warning("INLA crashed with error: ", err)
                    write_log(logfile = log_filepath, message = err)
                }
            )
        })

        output$hyper_param_out <- shiny::renderTable(
            {
                if (length(model_vals$model_outputs) == 0) {
                    return()
                } else {
                    last_run <- model_vals$model_outputs[[length(model_vals$model_outputs)]]
                    last_run$summary.hyperpar
                }
            },
            rownames = TRUE
        )

        output$fixed_out <- shiny::renderTable(
            {
                if (length(model_vals$model_outputs) == 0) {
                    return()
                } else {
                    last_run <- model_vals$model_outputs[[length(model_vals$model_outputs)]]
                    last_run$summary.fixed
                }
            },
            rownames = TRUE
        )

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
#' @param time_variable Time variable in measurement_data
#' @param mesh INLA mesh
#'
#' @return shiny::app
#' @export
interactive_priors <- function(spatial_data, measurement_data, time_variable, mesh) {
    shiny::runApp(priors_shiny(spatial_data = spatial_data, measurement_data = measurement_data, time_variable = time_variable, mesh = mesh))
}
