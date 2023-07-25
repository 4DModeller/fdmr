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
                         inla_exposure_param = "Population",
                         prior_range = NULL,
                         ps_range = NULL,
                         prior_sigma = NULL,
                         pg_sigma = NULL,
                         prior_ar1 = NULL,
                         pg_ar1 = NULL) {
    initial_equation_val <- "formula <- model_var ~ 0 + Intercept"
    features <- names(measurement_data)
    if (is.null(features)) {
        stop("We require the columns of measurement_data to have the names of the features to use in the model.")
    }

    plot_choices <- c("Range", "Marginal Stdev", "AR(1)", "Boxplot", "Density", "DIC")



    # Define UI for application that draws a histogram
    ui <- shiny::fluidPage(
        # Use this function somewhere in UI
        shinybusy::add_busy_spinner(spin = "folding-cube", margins = c(20, 20)),
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
                        shiny::selectInput(inputId = "plot_type", label = "Plot type:", choices = plot_choices),
                        shiny::plotOutput(outputId = "plot_model_out")
                    )
                )
            ),
        )
    )

    server <- function(input, output, session) {
        status_values <- shiny::reactiveValues("status" = "OK")

        run_no <- shiny::reactiveVal(0)
        rv <- shiny::reactiveValues(model_outputs = list())

        output$status <- shiny::renderText({
            paste("Status : ", status_values$status)
        })

        initial_equation <- shiny::reactive({
            stringr::str_replace(initial_equation_val, "model_var", input$model_var)
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
            group_index <- measurement_data$week
            n_groups <- length(unique(measurement_data$week))

            # TODO - add regex to check this for sensible values
            # formula <- eval(parse(text = formula_str()))
            spde <- INLA::inla.spde2.pcmatern(
                mesh = mesh,
                prior.range = c(input$prior_range, input$ps_range),
                prior.sigma = c(input$prior_sigma, input$pg_sigma)
            )

            # alphaprior <- base::list(theta = list(
            #     prior = "pccor1",
            #     param = c(-0.2, 0.8)
            # ))


            group_index <- measurement_data$week
            n_groups <- base::length(base::unique(measurement_data$week))
            # sp::coordinates(measurement_data) <- c("LONG", "LAT")

            formula <- cases ~ 0 + Intercept + f(
                main = coordinates,
                model = spde,
                group = group_index,
                ngroup = n_groups,
                control.group = list(
                    model = "ar1",
                    hyper = alphaprior()
                )
            )

            model_output <- inlabru::bru(formula,
                data = measurement_data,
                family = "poisson",
                E = measurement_data$Population,
                control.family = list(link = "log"),
                options = list(
                    verbose = FALSE
                )
            )
            # formula <- cases ~ 0 + Intercept + f(
            #     main = coordinates,
            #     model = spde(),
            #     group = group_index,
            #     ngroup = n_groups,
            #     control.group = list(
            #         model = "ar1",
            #         hyper = alphaprior()
            #     )
            # )

            # tryCatch(
            #     expr = {
            # model_output <- inlabru::bru(formula,
            #     data = measurement_data,
            #     family = "poisson",
            #     E = measurement_data[[inla_exposure_param]],
            #     control.family = list(link = "log"),
            #     options = list(
            #         verbose = FALSE
            #     )
            # )

            # TODO - just save the output of fdmr::parse_model_bru?
            # run_no(run_no() + 1)
            # rv$model_outputs[[run_no()]] <- model_output
            #     },
            #     error = function(e) {
            #         rv$model_outputs <- list("INLA_crashed" = TRUE, err = toString(e))
            #         run_no(0)
            #     }
            # )
        })

        output$comparison_output <- shiny::renderPrint({
            if (length(rv$model_outputs) == 0) {
                "No model output."
            } else {
                rv$model_outputs
            }
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
