#' Shiny app fo plotting raster data
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
    # Remove any letters from the names of the RasterLayers
    cleaned_names <- stringr::str_replace_all(names(raster_data), pattern = "[A-Za-z]", replacement = "")
    # Create Date objects
    date_strings <- lubridate::as_date(cleaned_names, format = date_format)
    date_strings <- base::sort(date_strings)

    if (any(is.na(lubridate::parse_date_time(names(raster_data), orders = date_format)))) {
        stop("Unable to parse dates from layer names. Please ensure they are named correctly or pass the date_format argument with the correct format.")
    }

    # We want to be able to lookup the RasterLayer names using the date so we create a list
    date_list <- as.list(base::sort(names(raster_data)))
    # Then we assign the date strings we'll use in the UI as the name (or key) for each element in the list
    names(date_list) <- date_strings

    # Define UI for application that draws a histogram
    ui <- shiny::fluidPage(
        shiny::titlePanel("4DModeller/fdmr date raster plotting"),
        shiny::fluidRow(
            shiny::column(
                8,
                leaflet::leafletOutput("raster_map"),
                shiny::textOutput("selected_date"),
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


    # Define server logic required to draw a histogram
    server <- function(input, output) {
        output$raster_map <- leaflet::renderLeaflet({
            layer_name <- date_list[[as.character(input$date_slider)]]
            raster_image <- raster_data[[layer_name]]

            m <- leaflet::leaflet()
            m <- leaflet::addTiles(m)

            if (!is.null(raster_data)) {
                m <- leaflet::addRasterImage(m,
                    x = raster_image,
                    opacity = 1
                )
            }

            if (!is.null(polygon_data)) {
                m <- leaflet::addPolygons(m, data = polygon_data, weight = 2)
            }
        })

        output$selected_date <- shiny::renderText({
            date_list[[as.character(input$date_slider)]]
        })
    }

    # Run the application
    shiny::shinyApp(ui = ui, server = server)
}
#


#' Run the interactive plotting Shiny app
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
