#' Parse inlabru model output
#'
#' @param model_output INLA model output
#' @param mesh INLA mesh
#' @param measurement_data Measurement data
#' 
#' @importFrom magrittr %>%
#'
#' @return shiny::app
#' @keywords internal
model_viewer_shiny <- function(model_output, mesh, measurement_data) {
    busy_spinner <- get_busy_spinner()

    parsed_model_output <- parse_model_output(
        model_output = model_output,
        measurement_data = measurement_data
    )

    plot_choices <- c("Range", "Stdev", "AR(1)", "Boxplot", "Density", "DIC")

    parsed_names <- names(parsed_model_output)
    map_vars <- parsed_names[!parsed_names %in% c("dic", "pars")]
  
    ui <- shiny::fluidPage(
        busy_spinner,
        shiny::headerPanel(title = "Model viewer"),
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                shiny::selectInput(
                    inputId = "plot_type",
                    label = "Model type:",
                    choices = c("inlabru"),
                    selected = "inlabru"
                ),
            ),
            shiny::mainPanel(
                shiny::tabsetPanel(
                    type = "tabs",
                    shiny::tabPanel(
                        "Plot",
                        shiny::h2("Plot output"),
                        shiny::selectInput(inputId = "plot_type", label = "Plot type:", choices = plot_choices, selected = plot_choices[1]),
                        shiny::plotOutput(outputId = "plot_model_out")
                    ),
                    shiny::tabPanel(
                        "Map",
                        shiny::selectInput(inputId = "map_var_a", label = "Variable a:", choices = map_vars),
                        shiny::selectInput(inputId = "map_var_b", label = "Variable b:", choices = map_vars),
                        leaflet::leafletOutput(outputId = "map_out")
                    ),
                    shiny::tabPanel(
                        "Help",
                        shiny::h3("Help"),
                    )
                )
            )
        )
    )

    # Define server logic required to draw a histogram
    server <- function(input, output, session) {
        map_raster <- shiny::reactive({
            pred_field <- create_prediction_field(
                var_a = parsed_model_output[[input$map_var_a]],
                var_b = parsed_model_output[[input$map_var_b]],
                mesh = mesh
            )

            crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
            raster = create_raster(dataframe = pred_field, crs = sp::CRS(crs))
            browser()
        })

        output$map_out <- leaflet::renderLeaflet({
            leaflet::leaflet() %>%
                leaflet::addTiles(group = "OSM") %>%
                leaflet::addRasterImage(map_raster(), opacity = 0.9, group = "Raster")
        })
        # Run the application
    }

    shiny::shinyApp(ui = ui, server = server)
}

#' Mesh building shiny app. Creates and visualises a mesh from some spatial data.
#'
#' @param model_output INLA model output
#' @param mesh INLA mesh
#' @param measurement_data Measurement data
#'
#' @return shiny::app
#' @export
model_viewer <- function(model_output, mesh, measurement_data) {
    shiny::runApp(model_viewer_shiny(model_output = model_output, mesh = mesh, measurement_data = measurement_data))
}
