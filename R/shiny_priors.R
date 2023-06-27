#' Interactively set and see the result of different priors
#'
#' @param spatial_data
#' @param measurement_data
#'
#' @importFrom INLA f
#'
#' @return
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
                shiny::actionButton(inputId = "clear", label = "Clear"),
                shiny::actionButton(inputId = "run_model", label = "Run"),
                shiny::textOutput(outputId = "status")
            ),
            shiny::mainPanel(
                shiny::column(
                    12,
                    shiny::textOutput(outputId = "comparison_output"),
                    shiny::textOutput(outputId = "final_equation"),
                    shiny::verbatimTextOutput("value")
                )
            )
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


        final_equation_str <- shiny::reactive({
            if (length(input$features) == 0) {
                return(initial_equation())
            } else {
                # features_copy <- input$features
                # features_copy <- features_copy[features_copy != input$model_var]
                # if (length(features_copy == 0)) {
                #     return(initial_equation())
                # }
                chosen <- stringr::str_c(input$features, collapse = " + ")
                stringr::str_c(c(initial_equation(), chosen), collapse = " + ")
            }
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

        output$final_equation <- shiny::renderText({
            final_equation_str()
        })

        model_out <- shiny::eventReactive(input$run_model, ignoreNULL = TRUE, {
            rhoprior <- base::list(theta = list(prior = "pccor1", param = c(0, 0.9)))
            group_index <- measurement_data$week
            n_groups <- length(unique(measurement_data$week))

            formula <- eval(parse(text = final_equation_str()))
            # formula <- cases ~ 0 + Intercept + perc.chinese
            # + IMD +
            #     carebeds.ratio + AandETRUE +
            #     perc.chinese +
            # f(
            #     main = coordinates,
            #     model = spde(),
            #     group = group_index,
            #     ngroup = n_groups,
            #     control.group = list(
            #         model = "ar1",
            #         hyper = rhoprior
            #     )
            # )

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
#' @param spatial_data
#' @param measurement_data
#'
#' @return
#' @export
interactive_priors <- function(spatial_data, measurement_data) {
    shiny::runApp(priors_shiny(spatial_data = spatial_data, measurement_data = measurement_data))
}
