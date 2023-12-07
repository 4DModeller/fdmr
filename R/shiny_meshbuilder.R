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

  # If the user passes in any of these then we enable the sliders
  enable_inputs <- (!is.null(max_edge) || !is.null(offset) || !is.null(cutoff))

  got_lat_long <- all(c(longitude_column, latitude_column) %in% names(spatial_data))
  if (!got_lat_long) {
    stop("Cannot read latitude and longitude data from spatial data. Please ensure given names are correct.")
  }

  default_max_edge <- c(0.01, 0.3)
  default_offset <- c(0.02, 0.7)
  default_cutoff <- 0.02
  # TODO - these defaults need changing?
  if (is.null(max_edge)) max_edge <- default_max_edge
  if (is.null(offset)) offset <- default_offset
  if (is.null(cutoff)) cutoff <- default_cutoff

  # Make sure we have our own internal correctly formatted version of the data
  coords_only <- spatial_data[, c(longitude_column, latitude_column)]
  names(coords_only) <- c("LONG", "LAT")


  busy_spinner <- get_busy_spinner()

  ui <- shiny::fluidPage(
    busy_spinner,
    shinyjs::useShinyjs(),
    shiny::headerPanel(title = "Creating a mesh"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::checkboxInput(inputId = "enable_inputs", label = "Enable customisation", value = enable_inputs),
        shiny::sliderInput(
          inputId = "max_edge",
          label = "Max edge:",
          min = 0.02, value = 0.1, max = 10
        ),
        shiny::p("Max permitted edge length for a triangle"),
        shiny::sliderInput(
          inputId = "offset",
          label = "Offset:",
          min = 0.02, value = 0.1, max = 10
        ),
        shiny::p("Specifies the size of the inner and outer extensions around data locations."),
        shiny::sliderInput(
          inputId = "cutoff",
          label = "Cutoff:",
          min = 0.005, value = 0.1, max = 0.9
        ),
        shiny::p("Minimum allowed distance between data points."),
        shiny::actionButton("plot_mesh", label = "Plot mesh"),
        shiny::actionButton("reset_mesh", label = "Reset"),
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          type = "tabs",
          shiny::tabPanel(
            "Plot",
            class = "p-3 border",
            shiny::div(leaflet::leafletOutput("map", height = "80vh")),
            shiny::br(),
            shiny::textOutput(outputId = "mesh_crs")
          ),
          shiny::tabPanel("Code", class = "p-3 border", shiny::verbatimTextOutput("mesh_code")),
          shiny::tabPanel(
            "Help",
            class = "p-3 border",
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
    shiny::observeEvent(input$enable_inputs, {
      if (input$enable_inputs) {
        shinyjs::enable("max_edge")
        shinyjs::enable("offset")
        shinyjs::enable("cutoff")
      } else {
        shinyjs::disable("max_edge")
        shinyjs::disable("offset")
        shinyjs::disable("cutoff")
      }
    })

    shiny::observeEvent(input$reset_mesh, {
      shiny::updateSliderInput(session, inputId = "max_edge", value = default_max_edge)
      shiny::updateSliderInput(session, inputId = "offset", value = default_offset)
      shiny::updateSliderInput(session, inputId = "cutoff", value = default_cutoff)
    })

    output$mesh_crs <- shiny::renderText({
      paste("Mesh CRS: ", crs)
    })

    mesh <- shiny::eventReactive(input$plot_mesh, ignoreNULL = FALSE, {
      if (input$enable_inputs) {
        max_edge <- input$max_edge
        offset <- input$offset
        cutoff <- input$cutoff
      } else {
        max_edge <- NULL
        offset <- NULL
        cutoff <- NULL
      }

      fmesher::fm_mesh_2d_inla(
        loc = coords_only,
        max.edge = max_edge,
        cutoff = cutoff,
        offset = offset,
        crs = crs,
      )
    })

    mesh_spatial <- shiny::reactive(
      suppressMessages(
        suppressWarnings(
          fdmr::mesh_to_spatial(mesh = mesh(), crs = crs)
        )
      )
    )

    output$map <- leaflet::renderLeaflet({
      spatial <- sf::st_as_sf(
        spatial_data,
        coords = c("LONG", "LAT"),
        crs = "+proj=utm +zone=33"
      )

      m <- mapview::mapview(list(spatial, mesh_spatial()), layer.name = (c("Spatial data", "Mesh")))
      m@map
    })

    output$mesh_code <- shiny::reactive({
      if (input$enable_inputs) {
        max_edge_str <- paste0("max.edge = c(", paste0(input$max_edge, collapse = ","), "),")
        offset_str <- paste0("offset = c(", paste0(input$offset, collapse = ", "), "),")
        cutoff_str <- paste0("cutoff = ", input$cutoff, ",")
      } else {
        max_edge_str <- "max.edge = NULL,"
        offset_str <- "offset = NULL,"
        cutoff_str <- "cutoff = NULL,"
      }

      paste0(
        "location_data <- spatial_data[, c('", longitude_column, "', '", latitude_column, "')],\n",
        "names(location_data) <- c('LONG', 'LAT')\n",
        "mesh <- fmesher::fm_mesh_2d_inla(loc = location_data,\n\t",
        max_edge_str, "\n\t",
        cutoff_str, "\n\t",
        offset_str, ")\n"
      )
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
