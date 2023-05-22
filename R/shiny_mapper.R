#' Shiny app for plotting raster and polygon data on a leaflet map.
#'
#' @param raster_data Raster data in the form or a RasterStack or RasterBrick
#' @param polygon_data Polygon data to plot on map
#' @param date_format A date format that will be passed in to lubridate::as_date
#'
#' @return
#' @keywords internal
#'
#' @examples
raster_mapping_app <- function(raster_data = NULL, polygon_data = NULL, date_format = NULL) {
    # TODO - can we check the projection of the data?
    # TODO - check projection of the polygon data? Or require users to do this?
    # Remove any letters from the names of the RasterLayers
    cleaned_names <- stringr::str_replace_all(names(raster_data), pattern = "[A-Za-z]", replacement = "")
    # Create Date objects
    date_strings <- lubridate::as_date(cleaned_names, format = date_format)
    date_strings <- base::sort(date_strings)

    # if (any(is.na(lubridate::parse_date_time(names(raster_data), orders = date_format)))) {
    #     stop(paste(
    #         "Unable to parse dates from layer names. Please ensure they are named correctly",
    #         "or pass the date_format argument with the correct format."
    #     ))
    # }

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

    # Define UI for application that draws a histogram
    ui <- shiny::fluidPage(
        shiny::sidebarLayout(
            position = "right",
            shiny::sidebarPanel(
                shiny::selectInput(
                    inputId = "map_tiles", "Map tiles",
                    choices = c("OpenStreetMap.Mapnik", "CartoDB.Positron", "Esri.WorldImagery"),
                    selected = "OpenStreetMap.Mapnik"
                ),
                shiny::selectInput(
                    inputId = "colour_scheme", "Colour scheme",
                    choices = c("viridis", "magma", "inferno", "plasma"),
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
                shiny::checkboxGroupInput("layers", "Layers:",
                    choiceNames = valid_layers,
                    choiceValues = valid_layers,
                    selected = valid_layers,
                )
            ),
            shiny::mainPanel(
                shiny::fluidRow(
                    shiny::column(12,
                        align = "center",
                        leaflet::leafletOutput("map"),
                        # Need to make sure the steps here match the data
                        shiny::sliderInput(
                            inputId = "date_slider",
                            label = "Date:",
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
    server <- function(input, output) {
        raster_image <- shiny::reactive({
            layer_name <- date_list[[as.character(input$date_slider)]]
            raster_data[[layer_name]]
        })

        # Do we need these?
        map_layers <- shiny::reactive({
            input$layers
        })

        raster_opacity <- shiny::reactive({
            input$raster_opacity
        })

        polygon_opacity <- shiny::reactive({
            input$polygon_opacity
        })

        colour_scheme <- shiny::reactive({
            input$colour_scheme
        })

        map_tiles <- shiny::reactive({
            input$map_tiles
        })

        output$map <- leaflet::renderLeaflet({
            leaflet::leaflet() %>%
                # leaflet::addMapPane("raster", zIndex = 410) %>%
                # leaflet::addMapPane("polygon", zIndex = 420) %>%
                leaflet::fitBounds(lng1 = long_min, lat1 = lat_min, lng2 = long_max, lat2 = lat_max)
        })

        shiny::observe({
            leaflet::leafletProxy("map") %>% leaflet::addProviderTiles(map_tiles(), leaflet::providerTileOptions(zIndex = -10))
        })

        # Incremental changes to the map (in this case, replacing the
        # circles when a new color is chosen) should be performed in
        # an observer. Each independent set of things that can change
        # should be managed in its own observer.
        shiny::observe({
            proxy <- leaflet::leafletProxy("map")
            proxy %>% leaflet::clearImages()

            if ("raster" %in% map_layers()) {
                proxy %>%
                    leaflet::addRasterImage(
                        x = raster_image(),
                        opacity = raster_opacity(),
                        layerId = "raster",
                        colors = colour_scheme(),
                    )
            }
        })

        shiny::observe({
            proxy <- leaflet::leafletProxy("map")
            proxy %>% leaflet::clearShapes()
            if ("polygon" %in% map_layers()) {
                leaflet::leafletProxy("map") %>%
                    leaflet::addPolygons(data = polygon_data, weight = 2, opacity = polygon_opacity(), layerId = "poly")
            }
        })
    }

    # Run the application
    shiny::shinyApp(ui = ui, server = server)
}
#


#' Run the Shiny app for plotting raster and polygon data on a leaflet map.
#'
#' @param raster_data Raster data in the form or a RasterStack or RasterBrick
#' @param polygon_data Polygon data to plot on map
#' @param date_format A date format that will be passed in to lubridate::as_date
#'
#' @return NULL
#' @export
plot_interative_map <- function(raster_data = NULL, polygon_data = NULL, date_format = NULL) {
    require_packages(packages = c("leaflet", "shiny", "stringr"))

    shiny::runApp(raster_mapping_app(raster_data = raster_data, polygon_data = polygon_data, date_format = date_format))
}
