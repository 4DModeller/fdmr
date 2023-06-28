#' Interactively set and see the result of different priors
#'
#' @param spatial_data Spatial data
#' @param measurement_data Measurement data
#'
#' @importFrom INLA f
#'
#' @return shiny::app
#' @keywords internal
priors_shiny <- function(spatial_data, measurement_data) {
    # TODO - make this a bit more intelligent / add it to the data schema
    initial_equation_val <- "formula <- model_var ~ 0 + Intercept"
    features <- names(measurement_data)
    # We'll only create the mesh once
    initial_range <- diff(range(spatial_data@data[, "LONG"])) / 5
    max_edge <- initial_range / 8
    mesh <- INLA::inla.mesh.2d(
        loc = spatial_data@data[, c("LONG", "LAT")],
        max.edge = c(1, 2) * max_edge,
        offset = c(initial_range / 4, initial_range),
        cutoff = max_edge / 7
    )

    # Define UI for application that draws a histogram
    ui <- shiny::fluidPage(
        # Use this function somewhere in UI
        shinybusy::add_busy_spinner(spin = "folding-cube", margins = c(20, 20)),
        shiny::headerPanel("Investigating priors"),
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                shiny::sliderInput(
                    inputId = "wavelength",
                    label = "Prior: wavelength:",
                    min = 1, value = 1.5, max = 10
                ),
                shiny::p("Change the spatial wavelength prior"),
                shiny::selectInput(inputId = "model_var", label = "Model variable", choices = features),
                shiny::checkboxGroupInput(inputId = "features", label = "Features", choices = features),
                shiny::checkboxInput(inputId = "f_func", label = "Add f()", value = FALSE),
                shiny::actionButton(inputId = "clear", label = "Clear"),
                shiny::actionButton(inputId = "run_model", label = "Run"),
                shiny::textOutput(outputId = "status")
            ),
            shiny::mainPanel(
                shiny::fluidRow(shiny::textOutput(outputId = "comparison_output"), style = "height:80vh;"),
                shiny::fluidRow(shiny::textOutput(outputId = "final_equation"), style = "height:20vh;")
            ),
        )
    )

    server <- function(input, output, session) {
        status_values <- shiny::reactiveValues("status" = "OK")

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
                prior.range = c(input$wavelength, 0.5),
                prior.sigma = c(1, 0.01)
            )
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
                    hyper = rhoprior)
                )"

            if (input$f_func) {
                eval_str <- paste(eval_str, " + ", f_func)
            }
            return(eval_str)
        })

        output$final_equation <- shiny::renderText({
            formula_str()
        })

        model_out <- shiny::eventReactive(input$run_model, ignoreNULL = TRUE, {
            rhoprior <- base::list(theta = list(prior = "pccor1", param = c(0, 0.9)))
            group_index <- measurement_data$week
            n_groups <- length(unique(measurement_data$week))

            formula <- eval(parse(text = formula_str()))

            tryCatch(
                expr = {
                    inlabru::bru(formula,
                        data = measurement_data,
                        family = "poisson",
                        E = measurement_data$Population,
                        control.family = list(link = "log"),
                        # control.predictor = list(link = 1),
                        options = list(
                            control.inla = list(
                                reordering = "metis",
                                int.strategy = "eb"
                            ),
                            verbose = TRUE,
                            inla.mode = "experimental"
                        )
                    )
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
#'
#' @return shiny::app
#' @export
interactive_priors <- function(spatial_data, measurement_data) {
    shiny::runApp(priors_shiny(spatial_data = spatial_data, measurement_data = measurement_data))
}
