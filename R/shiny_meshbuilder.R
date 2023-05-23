# This app is partly based on the meshbuilder tool from r-inla
# Code for that app can be found here:
# https://github.com/hrue/r-inla/blob/Version_23.05.22/rinla/R/meshbuilder.R

meshbuilder_shiny <- function() {
    ui <- shiny::fluidPage(
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                shiny::sliderInput(
                    inputId =
                        "bins",
                    label = "Number of bins:",
                    min = 1, value = 30, max = 50
                )
            ),
            shiny::mainPanel(
                leaflet::plotOutput("distPlot")
            )
        )
    )

    # Define server logic required to draw a histogram
    server <- function(input, output) {
        mesh <- shiny::reactive({
            out <- INLA::inla.mesh.2d(
                loc = loc1,
                boundary = bnd1,
                max.edge = input$max.edge,
                min.angle = rev(input$min.angle),
                max.n = c(48000, 16000),
                max.n.strict = c(128000, 128000),
                cutoff = input$cutoff,
                offset = input$offset,
                crs = if (INLA::inla.has_PROJ6()) {
                    INLA::inla.CRS(input$crs.mesh)
                } else {
                    INLA::inla.CRS(input$crs.mesh)
                }
            )
            out
        })
    }

    # Run the application
    shiny::shinyApp(ui = ui, server = server)
}
