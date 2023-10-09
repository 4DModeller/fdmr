#' Shiny app for plotting raster and polygon data on a leaflet map.
#'
#' @param raster_data Raster data in the form or a RasterStack or RasterBrick
#' @param polygon_data Polygon data to plot on map
#' @param date_format A date format that will be passed in to lubridate::as_date
#' @param palette Colour palette to use
#'
#' @importFrom magrittr %>%
#'
#' @return shinyApp
#' @keywords internal
raster_mapping_app <- function(raster_data = NULL, polygon_data = NULL, date_format = NULL, palette = NULL) {
  # TODO - can we check the projection of the data?
  # TODO - check projection of the polygon data? Or require users to do this?

  # Remove any letters from the names of the RasterLayers
  cleaned_names <- stringr::str_replace_all(names(raster_data), pattern = "[A-Za-z]", replacement = "")
  # Create Date objects
  date_strings <- lubridate::as_date(cleaned_names, format = date_format)
  date_strings <- base::sort(date_strings)

  if (any(is.na(date_strings))) {
    stop(
      "Unable to parse dates from layer names. Please ensure they are named correctly",
      "or pass the date_format argument with the correct format."
    )
  }

  # We want to be able to lookup the RasterLayer names using the date so we create a list
  date_list <- as.list(base::sort(names(raster_data)))
  # Then we assign the date strings we'll use in the UI as the name (or key) for each element in the list
  names(date_list) <- date_strings

  # Get the extent of the RasterLayer in lat/long
  raster_extents <- raster::extent(raster::projectRaster(raster_data, crs = "+proj=longlat"))
  lat_min <- raster_extents@ymin
  lat_max <- raster_extents@ymax
  long_min <- raster_extents@xmin
  long_max <- raster_extents@xmax

  valid_layers <- list("raster")
  if (!is.null(polygon_data)) valid_layers <- append("polygon", valid_layers)

  brewer_palettes <- RColorBrewer::brewer.pal.info
  default_colours <- rownames(brewer_palettes[brewer_palettes$cat == "seq", ])

  # Define UI for application that draws a histogram
  ui <- shiny::fillPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
            .irs-grid-text {
            font-size: 10px;
            }
            .irs--shiny .irs-min,.irs--shiny .irs-max {
            font-size: 10px;
            }
            .irs--shiny .irs-from,.irs--shiny .irs-to,.irs--shiny .irs-single {
            font-size: 12px;
            }
            "))
    ),
    shiny::sidebarLayout(
      position = "right",
      shiny::sidebarPanel(
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
          selected = "viridis"
        ),
        shiny::sliderInput(
          inputId = "raster_opacity",
          label = "Raster opacity",
          min = 0,
          max = 1,
          value = 0.9
        ),
        shiny::sliderInput(
          inputId = "polygon_opacity",
          label = "Polygon opacity",
          min = 0,
          max = 1,
          value = 0.6
        ),
        shiny::checkboxInput(
          inputId = "legend",
          label = "Show legend",
          value = TRUE
        )
      ),
      shiny::mainPanel(
        shiny::column(
          12,
          leaflet::leafletOutput("map", height = "80vh"),
          # Need to make sure the steps here match the data
          shiny::column(12,
            align = "center",
            shiny::sliderInput(
              inputId = "date_slider",
              label = "Date",
              min = as.Date(min(date_strings)),
              max = as.Date(max(date_strings)),
              value = as.Date(min(date_strings[[1]])),
              timeFormat = "%F"
            )
          )
        )
      )
    )
  )

  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    raster_image <- shiny::reactive({
      layer_name <- date_list[[as.character(input$date_slider)]]
      raster_image <- raster_data[[layer_name]]
    })

    raster_values <- shiny::reactive({
      layer_name <- date_list[[as.character(input$date_slider)]]
      raster::values(raster_data[[layer_name]])
    })

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

    colour_scheme <- shiny::reactive({
      input$colour_scheme
    })

    raster_opacity <- shiny::reactive({
      input$raster_opacity
    })

    polygon_opacity <- shiny::reactive({
      input$polygon_opacity
    })


    colour_palette <- shiny::reactive({
      if (is.null(palette)) {
        pal <- leaflet::colorNumeric(palette = colour_scheme(), domain = raster_values())
      } else {
        pal <- palette
      }
      pal
    })

    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>% leaflet::fitBounds(lng1 = long_min, lat1 = lat_min, lng2 = long_max, lat2 = lat_max)
    })

    shiny::observe({
      shiny::updateSelectInput(session, inputId = "colour_scheme", label = "Color Scheme", choices = category_colours())
    })

    shiny::observe({
      m <- leaflet::leafletProxy("map")
      m <- leaflet::addTiles(m, group = "OSM")
      m <- leaflet::addProviderTiles(m, leaflet::providers$Esri.WorldImagery, group = "Satellite")
      m <- leaflet::addProviderTiles(m, leaflet::providers$CartoDB.Positron, group = "Positron")
      m <- leaflet::addRasterImage(m,
        x = raster_image(),
        opacity = raster_opacity(),
        group = "Raster",
        layerId = "raster",
        colors = colour_scheme(),
      )
      if (!is.null(polygon_data)) {
        m <- leaflet::addPolygons(m, data = polygon_data, weight = 2, opacity = polygon_opacity(), group = "Poly", layerId = "poly")
      }
      # Layers control
      m <- leaflet::addLayersControl(m,
        position = "topright",
        baseGroups = c("OSM", "Satellite", "Positron"),
        overlayGroups = c("Raster", "Poly"),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
    })

    # Use a separate observer to recreate the legend as needed.
    shiny::observe({
      proxy <- leaflet::leafletProxy("map")
      # Remove any existing legend, and only if the legend is
      # enabled, create a new one.
      proxy %>% leaflet::clearControls()
      if (input$legend) {
        proxy %>% leaflet::addLegend(
          position = "bottomright",
          bins = 5,
          pal = colour_palette(), values = raster_values()
        )
      }
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}

#' Run the Shiny app for plotting raster and polygon data on a leaflet map.
#'
#' @param raster_data Raster data in the form or a RasterStack or RasterBrick
#' @param polygon_data Polygon data to plot on map
#' @param date_format A date format that will be passed in to lubridate::as_date
#'
#' @return shinyApp
#' @export
plot_interactive_map <- function(raster_data = NULL, polygon_data = NULL, date_format = NULL) {
  shiny::runApp(raster_mapping_app(raster_data = raster_data, polygon_data = polygon_data, date_format = date_format))
}
