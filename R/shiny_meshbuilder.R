# This app is partly based on the meshbuilder tool from r-inla
# Code for that app can be found here:
# https://github.com/hrue/r-inla/blob/Version_23.05.22/rinla/R/meshbuilder.R



#' Mesh building shiny app
#'
#' @param spatial_data Spatial data
#' @param crs CRS as a proj4string
#' @param offset Specifies the size of the inner and outer extensions around data locations, passed to inla.mesh.2d
#' @param max_edge The largest allowed triangle edge length. One or two values, passed to inla.mesh.2d
#' @param cutoff The minimum allowed distance between points, passed to inla.mesh.2d
#'
#' @importFrom magrittr %>%
#'
#' @return shiny::app
#' @keywords internal
meshbuilder_shiny <- function(
    spatial_data,
    crs = NULL,
    max_edge = NULL,
    offset = NULL,
    cutoff = NULL) {
  # TODO - these defaults need changing?
  if (is.null(max_edge)) max_edge <- c(0.1, 0.3)
  if (is.null(offset)) offset <- c(0.2, 0.7)
  if (is.null(cutoff)) cutoff <- 0.2

  if (is.null(crs)) {
    crs <- sf::st_crs(spatial_data)
    if (is.null(crs)) {
      stop("Unable to read CRS from data, please pass proj4string CRS to crs argument.")
    }
  }

  # TODO - make this a bit more intelligent so we can handle differently named position data
  got_lat_long <- all(c("LONG", "LAT") %in% names(spatial_data@data))
  if (!got_lat_long) {
    stop("Cannot read latitude and longitude data from spatial data.")
  }

  # Extract the extent from the data
  extent <- raster::extent(sp_data)
  # x is longitude, y is latitude
  long_min <- extent@xmin
  long_max <- extent@xmax
  lat_min <- extent@ymin
  lat_max <- extent@ymax

  # Let's extract the data we want to create the mesh
  location_data <- spatial_data@data[, c("LONG", "LAT")]

  # loc: the spatial locations of data points
  # max.edge: it determines the maximum permitted length for a triangle (lower values for max.edge result in higher mesh resolution). This parameter can take either a scalar value, which controls the triangle edge lengths in the inner domain,
  # or a length-two vector that controls edge lengths both in the inner domain and in the outer extension to avoid the boundary effect.
  # offset: it specifies the size of the inner and outer extensions around the data locations.
  # cutoff: it defines the minimum allowed distance between data points.
  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::sliderInput(
          inputId = "max_edge",
          label = "Max edge:",
          min = 0.02, value = c(0.1, 0.3), max = 10
        ),
        shiny::p("Max permitted edge length for a triangle"),
        shiny::sliderInput(
          inputId = "offset",
          label = "Offset:",
          min = 0.02, value = c(0.2, 0.7), max = 10
        ),
        shiny::p("Specifies the size of the inner and outer extensions around data locations."),
        shiny::sliderInput(
          inputId = "cutoff",
          label = "Cutoff:",
          min = 0.005, value = 0.2, max = 0.9
        ),
        shiny::p("Minimum allowed distance between data points."),
        shiny::actionButton("plot_mesh", label = "Plot mesh"),
        shiny::checkboxInput("auto_plot", label = "Auto render mesh", value = TRUE),
        shiny::actionButton("check_button", "Check mesh"),
        shiny::verbatimTextOutput("value")
      ),
      shiny::mainPanel(
        leaflet::leafletOutput("map", height = "80vh")
      )
    )
  )



  # Define server logic required to draw a histogram
  server <- function(input, output) {
    mesh <- shiny::reactive({
      input$plot_mesh
      if (shiny::req(input$auto_plot)) {
        shiny::withProgress(message = "Creating mesh...", value = 0, {
          return(INLA::inla.mesh.2d(
            loc = location_data,
            max.edge = input$max_edge,
            cutoff = input$cutoff,
            offset = input$offset,
            crs = crs
          ))
        })
      }
      return(
        shiny::withProgress(message = "Creating mesh...", value = 0, {
          return(INLA::inla.mesh.2d(
            loc = location_data,
            max.edge = input$max_edge,
            cutoff = input$cutoff,
            offset = input$offset,
            crs = crs
          ))
        })
      )
    })

    # Can use this to write out to the terminal
    shiny::observeEvent(input$plot_mesh, {
      print(paste0("You have chosen: ", input$plot_mesh))
    })

    mesh_spatial <- shiny::reactive({
      shiny::withProgress(message = "Extracting mesh...", value = 0, {
        # TODO - suppress the warnings for now until we can convert
        # to functions that are PROJ6 acceptable
        base::suppressWarnings(
          fdmr::mesh_to_spatial(mesh = mesh())
        )
      })
    })

    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addTiles(group = "OSM") %>%
        leaflet::addPolygons(data = mesh_spatial(), weight = 0.5, fillOpacity = 0.2, fillColor = "#5252ca", group = "Mesh") %>%
        leaflet::addPolygons(data = spatial_data, fillColor = "#d66363", color = "green", weight = 1, group = "Spatial") %>%
        leaflet::addLayersControl(
          position = "topright",
          baseGroups = c("OSM"),
          overlayGroups = c("Mesh", "Spatial"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )
    })

    # output$map <- leaflet::renderLeaflet({
    #   leaflet::leaflet(mesh_spatial()) %>%
    #     leaflet::addTiles() %>%
    #     leaflet::addPolygons(weight = 0.5, fillOpacity = 0.2, fillColor = "#5252ca") %>%
    #     leaflet::addPolygons(data = spatial_data, fillColor = "#d66363", color = "green", weight = 1)
    # })

    # shiny::observe({
    #   leaflet::leafletProxy("map") %>%

    # })



    shiny::observeEvent(input$check_button, {
      shiny::showModal(shiny::modalDialog(
        "There is an error with the mesh: the error is ...",
        title = "Mesh error",
        easyClose = TRUE,
        footer = NULL
      ))
    })
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}


#' Mesh building shiny app. Creates and visualises a mesh from some spatial data.
#'
#' @param spatial_data Spatial data
#' @param crs CRS as a proj4string
#' @param offset Specifies the size of the inner and outer extensions around data locations, passed to inla.mesh.2d
#' @param max_edge The largest allowed triangle edge length. One or two values, passed to inla.mesh.2d
#' @param cutoff The minimum allowed distance between points, passed to inla.mesh.2d
#'
#' @return shiny::app
#' @export
mesh_builder <- function(spatial_data, crs = NULL, max_edge = NULL, offset = NULL, cutoff = NULL) {
  require_packages(packages = c("INLA", "shiny", "leaflet"))

  shiny::runApp(meshbuilder_shiny(
    spatial_data = spatial_data,
    crs = crs,
    max_edge = max_edge,
    offset = offset,
    cutoff = cutoff
  ))
}
