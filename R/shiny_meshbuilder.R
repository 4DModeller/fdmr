# This app is partly based on the meshbuilder tool from r-inla
# Code for that app can be found here:
# https://github.com/hrue/r-inla/blob/Version_23.05.22/rinla/R/meshbuilder.R



#' Mesh building shiny app
#'
#' @param spatial_data Spatial data
#' @param data Observations data, for use with the check_mesh functionality
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
    obs_data = NULL,
    crs = NULL,
    max_edge = NULL,
    offset = NULL,
    cutoff = NULL) {
  default_max_edge <- c(0.1, 0.3)
  default_offset <- c(0.2, 0.7)
  default_cutoff <- 0.2
  # TODO - these defaults need changing?
  if (is.null(max_edge)) max_edge <- default_max_edge
  if (is.null(offset)) offset <- default_offset
  if (is.null(cutoff)) cutoff <- default_cutoff

  # TODO - should we enforce these?
  max_n_mesh <- c(48000, 16000)
  max_n_strict_mesh <- c(128000, 128000)

  create_mesh <- function(location_data, max_edge, cutoff, offset, crs) {
    shiny::withProgress(message = "Creating mesh...", value = 0, {
      INLA::inla.mesh.2d(
        loc = location_data,
        max.edge = max_edge,
        cutoff = cutoff,
        offset = offset,
        crs = crs,
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

  # TODO - make this a bit more intelligent so we can handle differently named position data
  got_lat_long <- all(c("LONG", "LAT") %in% names(spatial_data@data))
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
          value = c(0.05, 0.5), min = 0.01, max = 2
        ),
        shiny::p("Max permitted edge length for a triangle"),
        shiny::sliderInput(
          inputId = "offset",
          label = "Offset:",
          value = c(0.1, 0.3), min = 0.05, max = 2
        ),
        shiny::p("Specifies the size of the inner and outer extensions around data locations."),
        shiny::sliderInput(
          inputId = "cutoff",
          label = "Cutoff:",
          value = 0.01, min = 0, max = 0.2
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
    # mesh_1 <- shiny::reactive({
    #   if (input$auto_plot) {
    #     create_mesh(
    #       location_data,
    #       input$max_edge,
    #       input$cutoff,
    #       input$offset,
    #       crs
    #     )
    #   }
    # })

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
      # mesh <- if (input$auto_plot) mesh_1() else mesh_2()
      # mesh <- mesh_2()
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

    output$mesh_code <- shiny::reactive(
      paste0(
        "mesh <- inla.mesh.2d(loc = location_data,
          max.edge = c(", paste0(input$max_edge, collapse = ", "), "),
          cutoff = ", input$cutoff, ",
          offset=c(", paste0(input$offset, collapse = ", "), "))\n"
      )
    )

    # Code below here is from INLA::meshbuilder()
    axis_infos <- shiny::reactiveValues(offset = NULL, max_edge = NULL, cutoff = NULL)
    axis_update <- shiny::reactiveValues(offset = FALSE, max_edge = FALSE, cutoff = FALSE)

    limits <- shiny::reactiveValues(
      input_xlim = c(0, 1),
      input_ylim = c(0, 1),
      inputplot.xlim = c(0, 1),
      inputplot.ylim = c(0, 1)
    )

    shiny::observeEvent(c(input$offset, limits$input_xlim, limits$input$ylim), {
      val <- input$offset
      new_info <- pretty_axis_info(
        lim = range(c(
          val,
          diff(limits$input_xlim) / 5,
          diff(limits$input_ylim) / 5
        )),
        value = val,
      )

      axis_update$offset <- !identical(axis_infos$offset, new_info)

      if (axis_update$offset) {
        axis_infos$offset <- new_info
      }
    })

    shiny::observeEvent(c(input$max_edge, limits$input_xlim, limits$input_ylim), {
      val <- input$max_edge
      new_info <- pretty_axis_info(
        lim =
          range(c(
            val,
            diff(limits$input_xlim) / 5,
            diff(limits$input_ylim) / 5
          )),
        value = val,
      )

      axis_update$max_edge <- !identical(axis_infos$max_edge, new_info)

      if (axis_update$max_edge) {
        axis_infos$max_edge <- new_info
      }
    })

    shiny::observeEvent(c(input$cutoff, input$max_edge), {
      val <- min(input$cutoff, input$max_edge[1])
      new_info <- pretty_axis_info(
        lim =
          range(c(val, input$max_edge[1])),
        value = val,
      )

      new_info$lim[2] <- min(new_info$lim[2], input$max_edge[1])
      axis_update$cutoff <- !identical(axis_infos$cutoff, new_info)

      if (axis_update$cutoff) {
        axis_infos$cutoff <- new_info
      }
    })

    shiny::observeEvent(axis_update$offset,
      {
        if (axis_update$offset) {
          axis_update$offset <- FALSE
          info <- axis_infos$offset
          new_val <- pretty_axis_value(info, input$offset)
          shiny::updateSliderInput(
            session, "offset",
            min = info$lim[1], max = info$lim[2],
            step = info$step, value = new_val
          )
        }
      },
      priority = 10
    )

    shiny::observeEvent(axis_update$max_edge,
      {
        if (axis_update$max_edge) {
          axis_update$max_edge <- FALSE
          info <- axis_infos$max_edge
          new_val <- pretty_axis_value(info, input$max_edge)

          shiny::updateSliderInput(
            session,
            "max_edge",
            min = info$lim[1],
            max = info$lim[2],
            step = info$step,
            value = new_val
          )
        }
      },
      priority = 9
    )

    shiny::observeEvent(c(axis_update$cutoff, input$cutoff, input$max_edge),
      {
        if (axis_update$cutoff || (input$cutoff > input$max_edge[1])) {
          axis_update$cutoff <- FALSE
          info <- axis_infos$cutoff

          new_val <- pretty_axis_value(info, min(input$cutoff, input$max_edge[1]))

          shiny::updateSliderInput(
            session,
            inputId = "cutoff",
            min = info$lim[1],
            max = info$lim[2],
            step = info$step,
            value = new_val
          )
        }
      },
      priority = 8
    )

    pretty_axis_info <- function(lim, value = NA, verbose = FALSE) {
      if (verbose) {
        message(paste(
          "pretty_axis_info <-",
          "lim =", paste(lim, collapse = ", "),
          "val = ", paste(value, collapse = ", ")
        ))
      }

      limits <- range(c(lim, value), na.rm = TRUE)

      maxi <- c(1.5, 2, 3, 4, 6, 8, 10)
      cutoff <- maxi * 9 / 10
      step <- c(1e-2, 1e-2, 1e-2, 2e-2, 2e-2, 5e-2, 5e-2)
      mini <- step

      maxi <- c(maxi, maxi * 10)
      step <- c(step, step * 10)
      mini <- c(mini, mini * 10)
      cutoff <- c(cutoff, cutoff * 10)
      level <- floor(log10(limits[2]))

      maxi <- maxi * 10^level
      mini <- mini * 10^level
      step <- step * 10^level
      cutoff <- cutoff * 10^level

      # Find smallest k such that limits[2] < maxi[k]
      k <- min(which(limits[2] < maxi))

      # Move to next k if limits[2] > cutoff[k]
      values <- mini[k] + round((value - mini[k]) / step[k]) * step[k]
      limits <- range(c(lim, value, values), na.rm = TRUE)

      while (limits[2] > cutoff[k]) {
        k <- k + 1
        values <- mini[k] + round((value - mini[k]) / step[k]) * step[k]
        limits <- range(c(lim, value, value), na.rm = TRUE)
      }

      if (verbose) {
        message(paste(
          "pretty_axis_info ->",
          "lim =", paste(c(mini[k], maxi[k]), collapse = ", ")
        ))
      }
      list(lim = c(mini[k], maxi[k]), step = step[k])
    }

    pretty_axis_value <- function(axis_info, value) {
      pmax(axis_info$lim[1], pmin(
        axis_info$lim[2],
        axis_info$lim[1] + round((value - axis_info$lim[1]) /
          axis_info$step) * axis_info$step
      ))
    }


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
#'
#' @return shiny::app
#' @export
mesh_builder <- function(spatial_data, obs_data = NULL, crs = NULL, max_edge = NULL, offset = NULL, cutoff = NULL) {
  require_packages(packages = c("INLA", "shiny", "leaflet"))

  shiny::runApp(meshbuilder_shiny(
    spatial_data = spatial_data,
    obs_data = obs_data,
    # crs = crs,
    # max_edge = max_edge,
    # offset = offset,
    # cutoff = cutoff
  ))
}
