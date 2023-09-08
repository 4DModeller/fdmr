#' Mesh building shiny app
#'
#' @param spatial_data a data.frame or tibble containing spatial data
#' @param data Observations data, for use with the check_mesh functionality
#' @param crs CRS as a proj4string
#' @param offset Specifies the size of the inner and outer extensions around data locations, passed to inla.mesh.2d
#' @param max_edge The largest allowed triangle edge length. One or two values, passed to inla.mesh.2d
#' @param cutoff The minimum allowed distance between points, passed to inla.mesh.2d
#' @param latitude_column Name of the latitude column in the spatial data
#' @param longitude_column Name of the longitude column in the spatial data
#'
#' @importFrom magrittr %>%
#'
#' @return shiny::app
#' @keywords internal
meshbuilder_shiny <- function(
    spatial_data,
    obs_data = NULL,
    crs = NULL,
    max_edge = NULL,
    offset = NULL,
    cutoff = NULL,
    plot_poly = FALSE,
    latitude_column = "LAT",
    longitude_column = "LONG") {
  if (!is.data.frame(spatial_data)) {
    stop("spatial_data must be a data.frame or tibble containing columns with latitude and longitude data.")
  }

  default_max_edge <- c(0.1, 0.3)
  default_offset <- c(0.2, 0.7)
  default_cutoff <- 0.2
  # TODO - these defaults need changing?
  if (is.null(max_edge)) max_edge <- default_max_edge
  if (is.null(offset)) offset <- default_offset
  if (is.null(cutoff)) cutoff <- default_cutoff

  # plot_poly <- FALSE
  # plot_points <- FALSE
  # We
  # spatial_points <- NULL
  # Do some checks to see what kind of data we have
  # If we have a SpatialPolygonsDataFrame, we can plot the polygons
  # otherwise if we just have SpatialPoints we can plot the points
  # otherwise we don't plot anything
  # if (class(spatial_data) == "SpatialPolygonsDataFrame") {
  #   plot_poly <- TRUE
  #   plot_points <- FALSE
  # } else {
  #   # Try and create some points from the dataframe
  #   spatial_points <- spatial_data[, c(longitude_column, longitude_column)]
  #   names(spatial_points) <- c("LONG", "LAT")
  #   spatial_points <- sp::SpatialPoints(
  #     coords = spatial_points,
  #     proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  #   )
  # }



  # TODO - should we enforce these?
  max_n_mesh <- c(48000, 16000)
  max_n_strict_mesh <- c(128000, 128000)

  create_mesh <- function(location_data, max_edge, cutoff, offset, crs) {
    shiny::withProgress(message = "Creating mesh...", value = 0, {
      fmesher::fm_mesh_2d(
        loc = location_data,
        max.edge = max_edge,
        cutoff = cutoff,
        offset = offset,
        # crs = crs,
        # max.n = c(48000, 16000),
        # max.n.strict = c(128000, 128000)
      )
    })
  }

  if (is.null(crs)) {
    crs <- sf::st_crs(spatial_data)
    if (is.null(crs)) {
      stop("Unable to read CRS from data, please pass proj4string CRS to crs argument.")
    }
  }

  got_lat_long <- all(c(longitude_column, latitude_column) %in% names(spatial_data))
  if (!got_lat_long) {
    stop("Cannot read latitude and longitude data from spatial data.")
  }

  # Extract the extent from the data
  # extent <- raster::extent(spatial_data)
  # # x is longitude, y is latitude
  # long_min <- extent@xmin
  # long_max <- extent@xmax
  # lat_min <- extent@ymin
  # lat_max <- extent@ymax

  # Let's extract the data we want to create the mesh
  location_data <- spatial_data[, c(longitude_column, latitude_column)]

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
        shiny::actionButton("reset_mesh", label = "Reset"),
        shiny::actionButton("check_button", "Check mesh"),
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          type = "tabs",
          shiny::tabPanel("Plot", leaflet::leafletOutput("map", height = "80vh")),
          shiny::tabPanel("Code", shiny::verbatimTextOutput("mesh_code"))
        )
      )
    )
  )


  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    shiny::observeEvent(input$reset_mesh, {
      shiny::updateSliderInput(session, inputId = "max_edge", value = default_max_edge)
      shiny::updateSliderInput(session, inputId = "offset", value = default_offset)
      shiny::updateSliderInput(session, inputId = "cutoff", value = default_cutoff)
    })

    mesh <- shiny::eventReactive(input$plot_mesh, ignoreNULL = FALSE, {
      create_mesh(
        location_data,
        input$max_edge,
        input$cutoff,
        input$offset,
        crs
      )
    })

    mesh_spatial <- shiny::reactive(
      suppressMessages(
        suppressWarnings(
          fdmr::mesh_to_spatial(mesh = mesh())
        )
      )
    )

    output$map <- leaflet::renderLeaflet({
      m <- leaflet::leaflet()
      m <- leaflet::addTiles(m, group = "OSM")
      m <- leaflet::addPolygons(m, data = mesh_spatial(), weight = 0.5, fillOpacity = 0.2, fillColor = "#5252ca", group = "Mesh")
      if (plot_poly) {
        m <- leaflet::addPolygons(m, data = spatial_data, lat = latitude_column, lng = longitude_column, fillColor = "#d66363", color = "green", weight = 1, group = "Spatial")
      }
      # else if (plot_points) {
      #   m <- leaflet::addCircles(m, data = spatial_data_points)
      # }

      m <- leaflet::addLayersControl(m,
        position = "topright",
        baseGroups = c("OSM"),
        overlayGroups = c("Mesh", "Spatial"),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
    })

    output$mesh_code <- shiny::reactive(
      paste0(
        "mesh <- inla.mesh.2d(loc = location_data,
          max.edge = c(", paste0(input$max_edge, collapse = ", "), "),
          cutoff = ", input$cutoff, ",
          offset=c(", paste0(input$offset, collapse = ", "), "))\n"
      )
    )


    shiny::observeEvent(input$check_button, {
      if (is.null(obs_data) || is.null(mesh())) {
        errors <- "No observation data. Cannot check mesh."
      } else {
        errors <- fdmr::mesh_checker(mesh = mesh(), observations = obs_data)
        if (!length(errors)) {
          errors <- "No errors found."
        }
      }

      shiny::showModal(shiny::modalDialog(
        stringr::str_flatten(errors, collapse = "\n"),
        title = "Mesh check",
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
#' @param obs_data Measurement data
#' @param crs CRS as a proj4string
#' @param offset Specifies the size of the inner and outer extensions around data locations, passed to inla.mesh.2d
#' @param max_edge The largest allowed triangle edge length. One or two values, passed to inla.mesh.2d
#' @param cutoff The minimum allowed distance between points, passed to inla.mesh.2d
#' @param latitude_column Name of the latitude column in the spatial data
#' @param longitude_column Name of the longitude column in the spatial data
#'
#' @return shiny::app
#' @export
mesh_builder <- function(spatial_data, obs_data = NULL, crs = NULL, max_edge = NULL, offset = NULL, cutoff = NULL, latitude_column = "LAT", longitude_column = "LONG") {
  shiny::runApp(meshbuilder_shiny(
    spatial_data = spatial_data,
    obs_data = obs_data,
    crs = crs,
    max_edge = max_edge,
    offset = offset,
    cutoff = cutoff,
    latitude_column = latitude_column,
    longitude_column = longitude_column
  ))
}
