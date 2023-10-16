#' Interactively set and see the result of different priors
#'
#' @param spatial_data Spatial data
#' @param measurement_data Measurement data
#' @param time_variable Time variable in measurement_data
#' @param mesh INLA mesh
#' @param data_distribution Data distribution, Poisson or Gaussian
#' @param log_folder Folder to write out logs
#'
#' @importFrom INLA f
#'
#' @return shiny::app
#' @keywords internal
model_builder_shiny <- function(spatial_data,
                                measurement_data,
                                time_variable,
                                mesh,
                                data_distribution = "Poisson",
                                log_folder = NULL) {
    future::plan(future::multisession())

    if (!(data_distribution %in% c("Poisson", "Gaussian"))) {
        stop("We only support Poisson and Gaussian data")
    }

    got_coords <- has_coords(spatial_data = spatial_data)
    if (!got_coords) {
        stop("Please make sure you have set coordinates on spatial_data using sp::coordinates.")
    }

    spatial_crs <- sp::proj4string(spatial_data)
    mesh_crs <- mesh$crs$input

    if ((is.null(mesh_crs) || is.na(mesh_crs)) && (is.na(spatial_crs) || is.null(spatial_crs))) {
        warning("Cannot read CRS from mesh or spatial_data, using default CRS = +proj=longlat +datum=WGS84")
        crs <- "+proj=longlat +datum=WGS84"
    } else if (is.na(mesh_crs) || is.null(mesh_crs)) {
        crs <- spatial_crs
    } else {
        crs <- mesh_crs
    }

    brewer_palettes <- RColorBrewer::brewer.pal.info
    default_colours <- rownames(brewer_palettes[brewer_palettes$cat == "seq", ])

    # Text for priors help
    prior_range_text <- "A length 2 vector, with (range0, Prange) specifying that P(ρ < ρ_0)=p_ρ,
                         where ρ is the spatial range of the random field. P(ρ < ρ_0)=p_ρ indicates that the probability of ρ smaller than ρ_0 (range0) is p_ρ (Prange)."

    prior_sigma_text <- "A length 2 vector, with (sigma0, Psigma) specifying that P(σ > σ_0)=p_σ,
                         where σ is the marginal standard deviation of the field. P(σ > σ_0)=p_σ indicates that the probability of σ greater than σ_0 (sigma0) is p_σ (Psigma)."

    control_group_text <- "Temporal priors for the temporal autocorrelation parameter α are set using prior_alpha and pg_alpha, in the relation that P(α > prior_alpha) = pg_alpha, indicating that the probability of α greater than prior_alpha is pg_alpha.
                           These values are used to create alphaprior, which is then passed to the control.group argument, control.group = list(model = 'ar1', hyper = alphaprior).
                           It specifies that across time, the process evolves according to an AR(1) process where the prior for the autocorrelation parameter α is given by alphaprior.
                           We define alphaprior with the prior 'pccor1', which is a Penalised Complexity (PC) prior for the temporal autocorrelation parameter α, with α = 1 indicating strong temporal dependence, and α = 0 indicating independence across time."

    citation_priors <- "Spatial and field prior explanation taken from https://rdrr.io/github/INBO-BMK/INLA/man/inla.spde2.pcmatern.html"
    citation_control_group <- "Prior explanation text modified from https://www.paulamoraga.com/book-geospatial/sec-geostatisticaldataexamplest.html"

    initial_equation_val <- "formula <- model_var ~ 0 + Intercept(1)"
    features <- names(measurement_data)
    if (is.null(features)) {
        stop("We require the columns of measurement_data to have the names of the features to use in the model.")
    }

    # Logfile path
    timestamp_str <- lubridate::format_ISO8601(lubridate::now())
    log_filename <- paste0("priors_exploration_applog_", timestamp_str, ".txt")
    parameters_file <- paste0("priors_exploration_parameters_", timestamp_str, ".json")
    model_outputs_file <- paste0("priors_exploration_modelout_", timestamp_str, ".rds")

    write_logs <- TRUE
    if (is.null(log_folder)) {
        log_folder <- fs::path(fs::path_home(), "fdmr", "logs")

        if (!as.numeric(file.access(log_folder)) == 0) {
            tmpdir <- get_tmpdir()
            log_folder <- fs::path(tmpdir, "fdmr", "logs")

            if (!as.numeric(file.access(log_folder)) == 0) {
                warning("We are unable to find a folder to write logs to, please pass a folder path to log_folder")
                write_logs <- FALSE
            }
        }
    }

    if (!fs::dir_exists(log_folder)) {
        fs::dir_create(log_folder)
    }

    log_folder <- fs::path_expand(log_folder)
    cat("We will write log files to ", log_folder)

    log_filepath <- fs::path(log_folder, log_filename)
    parameters_filepath <- fs::path(log_folder, parameters_file)
    modeloutputs_filepath <- fs::path(log_folder, model_outputs_file)

    plot_choices <- c("Range", "Stdev", "AR(1)", "Boxplot", "Density", "DIC")

    # TODO - if we modularise the Shiny apps and setup a different directory
    # structure we can remove this
    busy_spinner <- get_busy_spinner()

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
                        shiny::fluidRow(
                            shiny::column(
                                6,
                                shiny::selectInput(inputId = "model_var", label = "Model variable", choices = features),
                                shiny::selectInput(inputId = "exposure_param", label = "Exposure (time variable)", choices = features),
                            ),
                            shiny::column(
                                6,
                                shiny::selectInput(inputId = "data_dist", label = "Data distribution", choices = c("Poisson", "Gaussian"), selected = data_distribution),
                            )
                        ),
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
                        "Map",
                        shiny::fluidRow(
                            shiny::column(
                                6,
                                shiny::selectInput(inputId = "map_plot_type", label = "Plot type", choices = c("Predicted mean fields", "Random effect fields"), selected = "Predicted mean fields"),
                                shiny::selectInput(inputId = "select_run_map", label = "Select run:", choices = c())
                            ),
                            shiny::column(
                                6,
                                shiny::selectInput(
                                    inputId = "colour_category",
                                    label = "Palette type",
                                    choices = c("Sequential", "Diverging", "Qualitative", "Viridis"),
                                    selected = "Viridis"
                                ),
                                shiny::selectInput(
                                    inputId = "colour_scheme",
                                    label = "Colour Scheme",
                                    choices = default_colours,
                                ),
                            )
                        ),
                        leaflet::leafletOutput(outputId = "map_out")
                    ),
                    shiny::tabPanel(
                        "Code",
                        shiny::selectInput(inputId = "select_run_code", label = "Select run:", choices = c()),
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
            shiny::updateSelectInput(session, inputId = "colour_scheme", label = "Colours", choices = category_colours())
            shiny::updateSelectInput(session = session, inputId = "select_run_map", choices = run_names())
            shiny::updateSelectInput(session = session, inputId = "select_run_code", choices = run_names())
            shiny::updateSelectInput(session, inputId = "colour_scheme", label = "Colours", choices = category_colours())
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

        data_distribution_internal <- shiny::reactive({
            tolower(input$data_dist)
        })

        shiny::observeEvent(input$run_model, ignoreNULL = TRUE, {
            exposure_param_local <- input$exposure_param
            formula_local <- inla_formula()
            measurement_data_local <- measurement_data

            data_dist_local <- data_distribution_internal()
            family_control <- NULL
            if (data_dist_local == "poisson") {
                family_control <- list(link = "log")
            }

            promise <- promises::future_promise(
                {
                    # Without loading INLA here we get errors
                    require("INLA")
                    inlabru::bru(formula_local,
                        data = measurement_data_local,
                        family = data_dist_local,
                        E = measurement_data_local[[exposure_param_local]],
                        control.family = family_control,
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
                        run_label <- paste0("Run-", run_no())

                        model_vals$model_outputs[[run_label]] <- model_output
                        model_vals$parsed_outputs[[run_label]] <- parse_model_output(
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

                        model_vals$run_params[[run_label]] <- run_params

                        if (write_logs) {
                            write_parameters(filepath = parameters_filepath, parameters = model_vals$run_params)
                            write_rds(modeloutputs_filepath, model_vals$parsed_outputs)
                        }
                    },
                onRejected = function(err) {
                    warning("INLA crashed with error: ", err)
                    if (write_logs) write_log(filepath = log_filepath, message = err)
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

        category_colours <- shiny::reactive({
            if (input$colour_category == "Viridis") {
                colours <- c("viridis", "magma", "inferno", "plasma")
            } else {
                palettes_mapping <- list("Sequential" = "seq", "Diverging" = "div", "Qualitative" = "qual")
                chosen_cat <- palettes_mapping[input$colour_category]
                colours <- rownames(subset(RColorBrewer::brewer.pal.info, category %in% chosen_cat))
            }
            colours
        })

        colour_scheme <- shiny::reactive({
            input$colour_scheme
        })

        prediction_field <- shiny::reactive({
            data <- model_vals$parsed_outputs[[input$select_run_map]]
            if (input$map_plot_type == "Predicted mean fields") {
                create_prediction_field(
                    mesh = mesh,
                    plot_type = "predicted_mean_fields",
                    data_dist = data_distribution_internal(),
                    var_a = data[["mean_post"]],
                    var_b = data[["fixed_mean"]]
                )
            } else {
                create_prediction_field(
                    mesh = mesh,
                    plot_type = "random_effect_fields",
                    data_dist = data_distribution_internal(),
                    var_a = data[["mean_post"]]
                )
            }
        })

        z_values <- shiny::reactive({
            prediction_field()[["z"]]
        })

        map_raster <- shiny::reactive({
            raster::rasterFromXYZ(prediction_field(), crs = crs)
        })

        map_colours <- shiny::reactive({
            leaflet::colorNumeric(palette = colour_scheme(), domain = z_values(), reverse = FALSE)
        })

        output$map_out <- leaflet::renderLeaflet({
            if (is.null(map_raster())) {
                return()
            }

            leaflet::leaflet() %>%
                leaflet::addTiles(group = "OSM") %>%
                leaflet::addRasterImage(map_raster(), colors = map_colours(), opacity = 0.9, group = "Raster") %>%
                leaflet::addLegend(position = "topright", pal = map_colours(), values = z_values())
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

            params <- model_vals$run_params[[input$select_run_code]]

            family_control_str <- "NULL"
            if (data_distribution_internal() == "poisson") {
                family_control_str <- "list(link = 'log'),"
            }

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
                        family = '", data_distribution_internal(), "',
                        E = measurement_data[[", input$exposure_param, "]],
                        control.family = ", family_control_str, "
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
#' @param log_folder Folder to write logs to
#'
#' @return shiny::app
#' @export
model_builder <- function(spatial_data, measurement_data, time_variable, mesh, data_distribution = "Poisson", log_folder = NULL) {
    shiny::runApp(model_builder_shiny(spatial_data = spatial_data, measurement_data = measurement_data, time_variable = time_variable, mesh = mesh, data_distribution = data_distribution, log_folder = log_folder))
}
