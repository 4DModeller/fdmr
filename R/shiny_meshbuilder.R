# This app is partly based on the meshbuilder tool from r-inla
# Code for that app can be found here:
# https://github.com/hrue/r-inla/blob/Version_23.05.22/rinla/R/meshbuilder.R

# loc: the spatial locations of data points
# max.edge: it determines the maximum permitted length for a triangle (lower values for max.edge result in higher mesh resolution). This parameter can take either a scalar value, which controls the triangle edge lengths in the inner domain,
# or a length-two vector that controls edge lengths both in the inner domain and in the outer extension to avoid the boundary effect.
# offset: it specifies the size of the inner and outer extensions around the data locations.
# cutoff: it defines the minimum allowed distance between data points.

# mesh <- INLA::inla.mesh.2d(
#   loc = sp_data@data[, c("LONG", "LAT")],
#   max.edge = c(1, 2) * max_edge,
#   offset = c(initial_range / 4, initial_range),
#   cutoff = max_edge / 7

#' Mesh building shiny app
#'
#' @param location_data SpatialPolygon(ish) data
#'
#' @return shiny::app
#' @keywords internal
meshbuilder_shiny <- function(location_data) {
    ui <- shiny::fluidPage(
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                shiny::sliderInput(
                    inputId = "max_edge",
                    label = "Max edge:",
                    min = 0.02, value = c(0.1, 0.3), max = 10
                ),
                shiny::sliderInput(
                    inputId = "offset",
                    label = "Offset:",
                    min = 0.02, value = c(0.2, 0.7), max = 10
                ),
                shiny::sliderInput(
                    inputId = "cutoff",
                    label = "Cutoff:",
                    min = 0.005, value = 0.2, max = 0.9
                )
            ),
            shiny::mainPanel(
                leaflet::plotOutput("mesh_plot")
            )
        )
    )

    # Define server logic required to draw a histogram
    server <- function(input, output) {
        mesh <- shiny::reactive({
            INLA::inla.mesh.2d(
                loc = location_data,
                max.edge = input$max_edge,
                cutoff = input$cutoff,
                offset = input$offset,
            )
        })

        output$mesh_plot <- shiny::reactive({
            plot(mesh())
        })
    }

    # Run the application
    shiny::shinyApp(ui = ui, server = server)
}


#' Mesh building shiny app. Creates and visualises a mesh from some spatial data.
#'
#' @param location_data Spatial data
#'
#' @return shiny::app
#' @export
mesh_builder <- function(location_data) {
    require_packages(packages = c("INLA", "shiny", "leaflet"))

    shiny::runApp(meshbuilder_shiny(location_data = location_data))

}