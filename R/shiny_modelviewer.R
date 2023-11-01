#' Parse inlabru model output
#'
#' @param model_output INLA model output
#' @param mesh INLA mesh
#' @param measurement_data Measurement data
#' @param data_distribution Type of data, Poisson or Gaussian
#'
#' @importFrom magrittr %>%
#'
#' @return shiny::app
#' @keywords internal
model_viewer_shiny <- function(model_output, mesh, measurement_data, data_distribution) {
  busy_spinner <- get_busy_spinner()

  crs <- mesh$crs$input
  if (is.null(crs) || is.na(crs)) {
    warning("Cannot read CRS from mesh, using default CRS = +proj=longlat +datum=WGS84")
    crs <- "+proj=longlat +datum=WGS84"
  }

  data_distribution <- stringr::str_to_title(data_distribution)
  if (!(data_distribution %in% c("Poisson", "Gaussian"))) {
    stop("data_distribution must be one of Poisson or Gaussian")
  }

  parsed_model_output <- parse_model_output(
    model_output = model_output,
    measurement_data = measurement_data
  )

  # The comparison plotting functions expect a list of lists
  parsed_modeloutput_plots <- list(parsed_model_output)

  brewer_palettes <- RColorBrewer::brewer.pal.info
  default_colours <- rownames(brewer_palettes[brewer_palettes$cat == "seq", ])

  plot_choices <- c("Range", "Stdev", "AR(1)", "Boxplot", "Density", "DIC")

  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    busy_spinner,
    shiny::headerPanel(title = "Model viewer"),
    shiny::tabsetPanel(
      type = "tabs",
      shiny::tabPanel(
        "Plots",
        shiny::h2("Plot output"),
        shiny::selectInput(inputId = "plot_type", label = "Plot type:", choices = plot_choices, selected = plot_choices[1]),
        shiny::plotOutput(outputId = "plot_model_out")
      ),
      shiny::tabPanel(
        "Map",
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::selectInput(inputId = "map_plot_type", label = "Plot type", choices = c("Predicted mean fields", "Random effect fields"), selected = "Predicted mean fields"),
            shiny::selectInput(inputId = "map_data_type", label = "Data type", choices = c("Poisson", "Gaussian"), selected = data_distribution),
          ),
          shiny::column(
            4,
            shiny::selectInput(
              inputId = "colour_category",
              label = "Palette type",
              choices = c("Sequential", "Diverging", "Qualitative", "Viridis"),
              selected = "Viridis"
            ),
            shiny::selectInput(
              inputId = "colour_scheme",
              label = "Color Scheme",
              choices = default_colours,
            ),
            shiny::checkboxInput(inputId = "custom_range", label = "Enable custom range", value = FALSE),
          ),
          shiny::column(
            3,
            shiny::numericInput(inputId = "custom_range_min", label = "Min", value = 0, step = 0.01),
            shiny::numericInput(inputId = "custom_range_max", label = "Max", value = 1, step = 0.01),
          )
        ),
        leaflet::leafletOutput(outputId = "map_out")
      ),
      shiny::tabPanel(
        "Help",
        shiny::h3("Help"),
      )
    )
  )

  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    category_colours <- shiny::reactive({
      if (input$colour_category == "Viridis") {
        colours <- c("viridis", "magma", "inferno", "plasma")
      } else {
        palettes_mapping <- list("Sequential" = "seq", "Diverging" = "div", "Qualitative" = "qual")
        chosen_cat <- palettes_mapping[input$colour_category]
        colours <- rownames(subset(RColorBrewer::brewer.pal.info, category %in% chosen_cat))
      }
      colours
    })

    shiny::observe({
      shiny::updateSelectInput(session, inputId = "colour_scheme", label = "Colours", choices = category_colours())
    })

    shiny::observe({
      step <- diff(z_values()[2] - z_values()[1])
      shiny::updateNumericInput(session, inputId = "custom_range_min", label = "Min", value = min(z_values()), step = step)
      shiny::updateNumericInput(session, inputId = "custom_range_max", label = "Max", value = max(z_values()), step = step)
    })

    shiny::observeEvent(input$custom_range, {
      shinyjs::toggleState("custom_range_min", condition = input$custom_range)
      shinyjs::toggleState("custom_range_max", condition = input$custom_range)
    })

    prediction_field <- shiny::reactive({
      data_dist <- tolower(input$map_data_type)
      if (input$map_plot_type == "Predicted mean fields") {
        create_prediction_field(
          mesh = mesh,
          plot_type = "predicted_mean_fields",
          data_dist = data_dist,
          var_a = parsed_model_output[["mean_post"]],
          var_b = parsed_model_output[["fixed_mean"]]
        )
      } else {
        create_prediction_field(
          mesh = mesh,
          plot_type = "random_effect_fields",
          data_dist = data_dist,
          var_a = parsed_model_output[["mean_post"]]
        )
      }
    })

    z_values <- shiny::reactive({
      prediction_field()[["z"]]
    })

    map_raster <- shiny::reactive({
      raster::rasterFromXYZ(prediction_field(), crs = crs)
    })

    map_colours <- shiny::reactive({
      if (input$custom_range) {
        domain <- c(input$custom_range_min, input$custom_range_max)
      } else {
        domain <- z_values()
      }
      leaflet::colorNumeric(palette = input$colour_scheme, domain = domain, reverse = FALSE)
    })

    legend_values <- shiny::reactive({
      if (input$custom_range) {
        vals <- c(input$custom_range_min, input$custom_range_max)
      } else {
        vals <- z_values()
      }
      vals
    })

    output$map_out <- leaflet::renderLeaflet({
      if (is.null(map_raster())) {
        return()
      }

      leaflet::leaflet() %>%
        leaflet::addTiles(group = "OSM") %>%
        leaflet::addRasterImage(map_raster(), colors = map_colours(), opacity = 0.9, group = "Raster") %>%
        leaflet::addLegend(position = "topright", pal = map_colours(), values = legend_values())
    })

    model_plot <- shiny::eventReactive(input$plot_type, ignoreNULL = FALSE, {
      if (input$plot_type == "Range") {
        return(plot_line_comparison(
          data = parsed_modeloutput_plots,
          to_plot = "Range for f",
          title = "Range"
        ))
      } else if (input$plot_type == "Stdev") {
        return(plot_line_comparison(
          data = parsed_modeloutput_plots,
          to_plot = "Stdev for f",
          title = "Marginal standard deviation"
        ))
      } else if (input$plot_type == "AR(1)") {
        return(plot_line_comparison(
          data = parsed_modeloutput_plots,
          to_plot = "GroupRho for f",
          title = "AR(1)"
        ))
      } else if (input$plot_type == "Boxplot") {
        return(plot_priors_boxplot(data = parsed_modeloutput_plots))
      } else if (input$plot_type == "Density") {
        return(plot_priors_density(
          data = parsed_modeloutput_plots,
          measurement_data = measurement_data
        ))
      } else if (input$plot_type == "DIC") {
        return(plot_dic(data = parsed_modeloutput_plots))
      }
    })

    output$plot_model_out <- shiny::renderPlot({
      model_plot()
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}

#' Mesh building shiny app. Creates and visualises a mesh from some spatial data.
#'
#' @param model_output INLA model output
#' @param mesh INLA mesh
#' @param measurement_data Measurement data
#' @param data_distribution Type of data, Poisson or Gaussian
#'
#' @return shiny::app
#' @export
model_viewer <- function(model_output, mesh, measurement_data, data_distribution = "Poisson") {
  shiny::runApp(model_viewer_shiny(model_output = model_output, mesh = mesh, measurement_data = measurement_data, data_distribution = data_distribution))
}
