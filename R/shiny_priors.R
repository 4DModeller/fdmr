#' Title
#'
#' @param data
#' @param model
#'
#' @return
#' @keywords internal
priors_shiny <- function(data, coordinates = NULL, spde = NULL) {
    # TODO - make this a bit more intelligent / add it to the data schema
    features <- colnames(data)
    inital_equation_val <- "formula <- cases ~ 0 + Intercept"
    f_section <- "f(
                main = coordinates,
                model = spde,
                group = group_index,
                ngroup = n_groups,
                control.group = list(
                model = 'ar1',
                hyper = rhoprior
                )
            )"

    # We'll only create the mesh once
    initial_range <- diff(range(data@data[, "LONG"])) / 5
    max_edge <- initial_range / 8
    mesh <- INLA::inla.mesh.2d(
        loc = data@data[, c("LONG", "LAT")],
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
                    shiny::plotOutput(outputId = "comparison"),
                    shiny::textOutput(outputId = "final_equation")
                )
            )
        )
    )

    server <- function(input, output, session) {
        final_equation_str <- shiny::reactive({
            chosen <- stringr::str_c(input$features, collapse = " + ")
            stringr::str_c(c(input$initial_equation, chosen, f_section), collapse = " + ")
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
            stringr::str_c(c(input$initial_equation, chosen, f_section), collapse = " + ")
        })

        model_out <- shiny::eventReactive(input$run_model, ignoreNULL = TRUE, {
            formula <- eval(parse(text = final_equation_str))

            inlabru::bru(formula,
                data = data,
                family = "poisson",
                E = data$Population,
                control.family = list(link = "log"),
                control.predictor = list(link = 1),
                options = list(
                    control.inla = list(
                        reordering = "metis",
                        int.strategy = "eb"
                    ),
                    verbose = TRUE,
                    inla.mode = "experimental"
                )
            )
        })
    }

    shiny::shinyApp(ui = ui, server = server)
}

#' Title
#'
#' @param data
#' @param model
#'
#' @return
#' @export
interactive_priors <- function(data) {
    shiny::runApp(priors_shiny(data = data))
}
