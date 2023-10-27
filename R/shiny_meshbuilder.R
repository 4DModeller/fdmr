#' Mesh building shiny app
#'
#' @param spatial_data a data.frame or tibble containing spatial data
#' @param data Observations data, for use with the check_mesh functionality
#' @param crs CRS as a proj4string
#' @param offset Specifies the size of the inner and outer extensions around data locations, passed to fmesher::fm_mesh_2d_inla
#' @param max_edge The largest allowed triangle edge length. One or two values, passed to fmesher::fm_mesh_2d_inla
#' @param cutoff The minimum allowed distance between points, passed to fmesher::fm_mesh_2d_inla
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
  if (!is.data.frame(spatial_data) && !is(spatial_data, "SpatialPolygonsDataFrame") && !is(spatial_data, "SpatialPointsDataFrame")) {
    stop("spatial_data must be a data.frame or tibble containing columns with latitude and longitude data.")
  }

  if (is.data.frame(spatial_data)) {
    # Check we don't have any NAs in our spatial data
    if (sum(is.na(spatial_data)) > 0) {
      warning("spatial_data contains NA values, removing them.")
      spatial_data <- na.omit(spatial_data)
    }
  }

  if (is.null(crs)) {
    crs <- tryCatch(
      {
        crs <- sp::proj4string(spatial_data)
      },
      error = function(err) {
        warning("Unable to read CRS from data, using default CRS = '+proj=longlat +datum=WGS84'")
        crs <- "+proj=longlat +datum=WGS84"
      }
    )
  }

  got_lat_long <- all(c(longitude_column, latitude_column) %in% names(spatial_data))
  if (!got_lat_long) {
    stop("Cannot read latitude and longitude data from spatial data. Please ensure given names are correct.")
  }

  # The number of nodes we count as being a big mesh
  n_nodes_big_mesh <- 10000

  default_max_edge <- c(0.1, 0.3)
  default_offset <- c(0.2, 0.7)
  default_cutoff <- 0.2
  # TODO - these defaults need changing?
  if (is.null(max_edge)) max_edge <- default_max_edge
  if (is.null(offset)) offset <- default_offset
  if (is.null(cutoff)) cutoff <- default_cutoff

  # Make sure we have our own internal correctly formatted version of the data
  coords_only <- spatial_data[, c(longitude_column, latitude_column)]
  names(coords_only) <- c("LONG", "LAT")

  plot_polygons <- FALSE
  plot_points <- FALSE
  # We may not use this
  spatial_points <- NULL
  # Do some checks to see what kind of data we have
  # If we have a SpatialPolygonsDataFrame, we can plot the polygons
  # otherwise if we just have SpatialPoints we can plot the points
  # otherwise we don't plot anything
  if (class(spatial_data) == "SpatialPolygonsDataFrame") {
    plot_polygons <- TRUE
  } else {
    plot_points <- TRUE

    spatial_points <- sp::SpatialPointsDataFrame(
      coords = coords_only,
      data = spatial_data,
      proj4string = sp::CRS(crs)
    )
  }

  busy_spinner <- get_busy_spinner()

  ui <- bslib::page_fluid(
    theme = bslib::bs_theme(bootswatch = "cosmo"),
    busy_spinner,
    shinybusy::add_loading_state(
      "#map",
      timeout = 600,
      text = "Calculating mesh...",
      svgColor = "steelblue"
    ),
    shiny::headerPanel(title = "Creating a mesh"),
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
          shiny::tabPanel(
            "Plot",
            shiny::div(id = "map_div", leaflet::leafletOutput("map", height = "80vh"))
          ),
          shiny::tabPanel("Code", shiny::verbatimTextOutput("mesh_code")),
          shiny::tabPanel(
            "Help",
            shiny::h3("Help"),
            shiny::h4("Max edge"),
            shiny::p("Determines the maximum permitted length for a triangle (lower values for max.edge result in higher mesh resolution). This parameter can take either a scalar value, which controls the triangle edge lengths in the inner domain,
                      or a length-two vector that controls edge lengths both in the inner domain and in the outer extension to avoid the boundary effect."),
            shiny::h4("Offset"),
            shiny::p("Specifies the size of the inner and outer extensions around data locations."),
            shiny::h4("Cutoff"),
            shiny::p("Minimum allowed distance between data points."),
          )
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
      fmesher::fm_mesh_2d_inla(
        loc = coords_only,
        max.edge = input$max_edge,
        cutoff = input$cutoff,
        offset = input$offset,
        crs = crs,
      )
    })

    # large_mesh <- shiny::reactive({
    #   mesh()$n > n_nodes_big_mesh
    # })

    mesh_spatial <- shiny::reactive(
      suppressMessages(
        suppressWarnings(
          fdmr::mesh_to_spatial(mesh = mesh(), crs = crs)
        )
      )
    )

    output$map <- leaflet::renderLeaflet({
      m <- leaflet::leaflet()
      m <- leaflet::addTiles(m, group = "OSM")
      m <- leaflet::addPolygons(m, data = mesh_spatial(), weight = 0.5, fillOpacity = 0.2, fillColor = "#5252ca", group = "Mesh")
      if (plot_polygons) {
        m <- leaflet::addPolygons(m, data = spatial_data, fillColor = "#d66363", color = "green", weight = 1, group = "Spatial")
      } else if (plot_points) {
        m <- leaflet::addCircles(m, data = spatial_points, group = "Spatial", fillColor = "#b9220b", color = "#b9220b")
      }

      m <- leaflet::addLayersControl(m,
        position = "topright",
        baseGroups = c("OSM"),
        overlayGroups = c("Mesh", "Spatial"),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
    })

    output$mesh_code <- shiny::reactive(
      paste0(
        "mesh <- fmesher::fm_mesh_2d_inla(loc = location_data,
          max.edge = c(", paste0(input$max_edge, collapse = ", "), "),
          cutoff = ", input$cutoff, ",
          offset=c(", paste0(input$offset, collapse = ", "), "))\n"
      )
    )

    # shiny::observe({
    #   if (large_mesh() && !modal_shown()) {
    #     shiny::showModal(shiny::modalDialog(
    #       "Mesh is large, plotting may be slow.",
    #       title = "Mesh warning",
    #       easyClose = TRUE,
    #       footer = NULL
    #     ))
    #   }
    #   modal_shown(TRUE)
    # })


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
#' @param offset Specifies the size of the inner and outer extensions around data locations, passed to fmesher::fm_mesh_2d_inla
#' @param max_edge The largest allowed triangle edge length. One or two values, passed to fmesher::fm_mesh_2d_inla
#' @param cutoff The minimum allowed distance between points, passed to fmesher::fm_mesh_2d_inla
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
