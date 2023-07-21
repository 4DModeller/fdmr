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
                         prior_spatial_range = NULL,
                         prior_range_probability = NULL,
                         prior_std_dev = NULL,
                         prior_std_dev_prob = NULL,
                         prior_alpha = NULL,
                         prior_pg_alpha = NULL) {
    initial_equation_val <- "formula <- model_var ~ 0 + Intercept"
    features <- names(measurement_data)
    if (is.null(features)) {
        stop("We require the columns of measurement_data to have the names of the features to use in the model.")
    }

    # Define UI for application that draws a histogram
    ui <- shiny::fluidPage(
        # Use this function somewhere in UI
        shinybusy::add_busy_spinner(spin = "folding-cube", margins = c(20, 20)),
        shiny::headerPanel(title = "Investigating priors"),
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                shiny::h3("Priors"),
                shiny::sliderInput(
                    inputId = "prior_spatial_range",
                    label = "Spatial range:",
                    min = 0.05, value = 0.1, max = 1
                ),
                shiny::sliderInput(
                    inputId = "prior_range_probability",
                    label = "Range probabilty:",
                    min = 0.1, value = 0.7, max = 1
                ),
                shiny::sliderInput(
                    inputId = "prior_std_dev",
                    label = "Standard deviation:",
                    min = 0.05, value = 0.1, max = 2
                ),
                shiny::sliderInput(
                    inputId = "prior_std_dev_prob",
                    label = "Standard dev. probability:",
                    min = 0.1, value = 0.4, max = 1
                ),
                shiny::h3("Temporal priors"),
                shiny::sliderInput(
                    inputId = "prior_alpha",
                    label = "Alpha:",
                    min = -1, value = 0.1, max = 1
                ),
                shiny::sliderInput(
                    inputId = "prior_pg_alpha",
                    label = "PG Alpha:",
                    min = 0, value = 0.7, max = 1
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
                    shiny::tabPanel("Plot", shiny::verbatimTextOutput("Plot here"))
                )
            ),
        )
    )

    server <- function(input, output, session) {
        status_values <- shiny::reactiveValues("status" = "OK")

        model_outputs <- shiny::reactiveValues(model_out = list(), run_number = 0)

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
                prior.range = c(input$prior_spatial_range, input$prior_range_probability),
                prior.sigma = c(input$prior_std_dev, input$prior_std_dev_prob)
            )
        })

        alphaprior <- shiny::reactive({
            list(theta = list(prior = "pccor1", param = c(input$prior_alpha, input$prior_pg_alpha)))
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

        shiny::eventReactive(input$run_model, ignoreNULL = TRUE, {
            group_index <- measurement_data$week
            n_groups <- length(unique(measurement_data$week))

            # TODO - add regex to check this for sensible values
            formula <- eval(parse(text = formula_str()))

            tryCatch(
                expr = {
                    model_output <- inlabru::bru(formula,
                        data = measurement_data,
                        family = "poisson",
                        E = measurement_data[[inla_exposure_param]],
                        control.family = list(link = "log"),
                        options = list(
                            verbose = FALSE
                        )
                    )

                    model_outputs$run_number <- model_outputs$run_number + 1
                    append(model_outputs$model_out, model_output)
                },
                error = function(e) {
                    list("INLA_crashed" = TRUE, err = toString(e))
                }
            )
        })

        model_summary <- shiny::reactive({
            summary(model_out())
        })

        output$comparison_output <- shiny::renderPrint({
            model_summary()
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
