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

# initial_range <- diff(range(sp_data@data[, "LONG"])) / 5

# max_edge <- initial_range / 8

# mesh <- INLA::inla.mesh.2d(
#   loc = sp_data@data[, c("LONG", "LAT")],
#   max.edge = c(1, 2) * max_edge,
#   offset = c(initial_range / 4, initial_range),
#   cutoff = max_edge / 7
# )

#' Mesh building shiny app
#'
#' @param spatial_data Spatial data
#'
#' @return shiny::app
#' @keywords internal
meshbuilder_shiny <- function(
    spatial_data,
    crs = NULL,
    max_edge = NULL,
    offset = NULL,
    cutoff = NULL) {
    # TODO - add in CRS reading from data
    # TODO - these defaults need changing,
    max_edge_default <- c(0.1, 0.3)
    offset_default <- c(0.2, 0.7)
    cutoff_default <- 0.2

    # Let's extract the data we want to create the mesh
    location_data <- spatial_data@data[, c("LONG", "LAT")]

    if (is.null(crs)) {
        crs <- sf::st_crs(spatial_data)
        if (is.null(crs)) {
            stop("Unable to read CRS from data, please pass proj4string CRS to crs argument.")
        }
    }

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
                leaflet::leafletOutput("map")
            )
        )
    )

    # Define server logic required to draw a histogram
    server <- function(input, output) {
        mesh <- shiny::reactive({
            # TODO - any feedback for progress bar here?
            shiny::withProgress(message = "Creating mesh...", value = 0, {
                mesh <- INLA::inla.mesh.2d(
                    loc = location_data,
                    max.edge = input$max_edge,
                    cutoff = input$cutoff,
                    offset = input$offset,
                    crs = crs
                )
            })
            return(mesh)
        })

        mesh_spatial <- shiny::reactive({
            fdmr::mesh_to_spatial(mesh = mesh())
        })

        output$map <- leaflet::renderLeaflet({
            leaflet::leaflet(mesh_spatial()) %>%
                leaflet::addTiles() %>%
                leaflet::addPolygons(weight = 0.5, fillOpacity = 0.2, fillColor = "blue") %>%
                leaflet::addPolygons(data = spatial_data, fillColor = "red", weight = 1)
            # leaflet::addPolygons(data = spatial_data, lng = location_data$LONG, lat = location_data$LAT, fillColor = "red", color = "white", weight = 0.5)

            # leaflet::leaflet(mesh_spatial()) %>%
            #     leaflet::addTiles() %>%
            #     leaflet::addPolygons(data = spatial_data, fillColor = "#d66363", stroke = 0.2)
        })

        # # From here to ylim taken from INLA's meshbuilder
        # mesh_proj <- shiny::reactive({
        #     if (is.null(mesh())) {
        #         NULL
        #     } else {
        #         INLA::inla.mesh.projector(mesh(), dims = c(500, 500))
        #     }
        # })

        # xlim <- shiny::reactive({
        #     lim1 <- if (is.null(mesh())) NA else range(mesh()$loc[, 1], na.rm = TRUE)

        #     r <- c(lim1)
        #     if (all(is.na(r))) {
        #         r <- c(0, 1)
        #     } else {
        #         r <- range(r, na.rm = TRUE)
        #     }
        #     r
        # })

        # ylim <- shiny::reactive({
        #     lim1 <- if (is.null(mesh())) NA else range(mesh()$loc[, 2], na.rm = TRUE)

        #     r <- lim1
        #     if (all(is.na(r))) {
        #         r <- c(0, 1)
        #     } else {
        #         r <- range(r, na.rm = TRUE)
        #     }
        #     r
        # })



        # shiny::observe({
        #     m <- leaflet::leafletProxy("map")
        #     m <- leaflet::addTiles(m, group = "OSM")
        #     m <- leaflet::addProviderTiles(m, leaflet::providers$Esri.WorldImagery, group = "Satellite")
        #     m <- leaflet::addProviderTiles(m, leaflet::providers$CartoDB.Positron, group = "Positron")
        #     m <- leaflet::addPolygons()
        # })

        # output$mesh_plot <- shiny::renderPlot({
        #     shiny::withProgress(message = "Creating plot...", value = 0, {
        #         col <- colorRampPalette(c("blue", "white", "red"))
        #         n_col <- 1 + 64
        #         fields::image.plot(
        #             range(mesh_proj()$x),
        #             range(mesh_proj()$y),
        #             matrix(0, 2, 2),
        #             # NOTE: this is fixed in the INLA code so I've just added it in here
        #             # zlim <- c(0.5, 1.5)
        #             zlim = c(0.5, 1.5),
        #             xlim = xlim(),
        #             ylim = ylim(),
        #             col = col(n_col),
        #             asp = 1,
        #             main = "",
        #             xlab = "Longitude", ylab = "Latitude"
        #         )
        #         plot(mesh(), add = TRUE)
        #         # TODO - why are we getting errors from
        #         # tryCatch({
        #         # plot(mesh())
        #         # })
        #     })
        # })
    }

    # Run the application
    shiny::shinyApp(ui = ui, server = server)
}


#' Mesh building shiny app. Creates and visualises a mesh from some spatial data.
#'
#' @param spatial_data Spatial data
#'
#' @return shiny::app
#' @export
mesh_builder <- function(spatial_data, max_edge, offset, cutoff) {
    require_packages(packages = c("INLA", "shiny", "leaflet"))

    shiny::runApp(meshbuilder_shiny(
        spatial_data = spatial_data,
        max_edge = max_edge,
        offset = offset,
        cutoff = cutoff
    ))
}
