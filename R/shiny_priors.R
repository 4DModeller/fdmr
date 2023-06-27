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
    features <- colnames(spatial_data)
    inital_equation_val <- "formula <- model_var ~ 0 + Intercept"

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
                    min = 0.1, value = 0.2, max = 10
                ),
                shiny::p("Change the spatial wavelength prior"),
                shiny::textInput(inputId = "initial_equation", label = "Initial equation", value = inital_equation_val),
                shiny::checkboxGroupInput(inputId = "features", label = "Features", choices = features),
                shiny::actionButton(inputId = "clear_equation", label = "Clear"),
                shiny::actionButton(inputId = "run_model", label = "Run")
            ),
            shiny::mainPanel(
                shiny::column(
                    12,
                    shiny::plotOutput(outputId = "comparison_output"),
                    shiny::textOutput(outputId = "final_equation")
                )
            )
        )
    )

    server <- function(input, output, session) {
        final_equation_str <- shiny::reactive({
            chosen <- stringr::str_c(input$features, collapse = " + ")
            stringr::str_c(c(input$initial_equation, chosen), collapse = " + ")
        })

        spde <- shiny::reactive({
            INLA::inla.spde2.pcmatern(
                mesh = mesh,
                prior.range = c(input$wavelength, 0.5),
                prior.sigma = c(1, 0.01)
            )
        })

        output$final_equation <- shiny::renderText({
            chosen <- stringr::str_c(input$features, collapse = " + ")
            stringr::str_c(c(input$initial_equation, chosen), collapse = " + ")
        })

        model_out <- shiny::eventReactive(input$run_model, ignoreNULL = TRUE, {
            rhoprior <- base::list(theta = list(prior = "pccor1", param = c(0, 0.9)))
            group_index <- measurement_data$week
            n_groups <- length(unique(measurement_data$week))

            # formula <- eval(parse(text = final_equation_str)) +
            formula <- cases ~ 0 + Intercept
            # + IMD +
            #     carebeds.ratio + AandETRUE +
            #     perc.chinese +
            #     f(
            #         main = coordinates,
            #         model = spde(),
            #         group = group_index,
            #         ngroup = n_groups,
            #         control.group = list(
            #             model = "ar1",
            #             hyper = rhoprior
            #         )
            #     )

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
                    return("INLA crashed.")
                }
            )
        })


        output$comparison_output <- shiny::renderText({
            summary(model_out())
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
