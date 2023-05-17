#' Shiny app fo plotting raster data
#'
#' @param raster_data
#' @param polygon_data
#'
#' @return
#' @keywords internal
#'
#' @examples
raster_mapping_app <- function(raster_data = NULL, polygon_data = NULL) {
    #  WIP - quick design for a slider app using the Kvilldall Catchment area data
    # and some ERA5 precipitation data
    library(shiny)
    library(stringr)
    library(leaflet)

    norway_polygon_location <- "/Users/gar/fdmr/tutorial_data/Kvilldal_Catch_Boundary.geojson"
    norway_polygon <- rgdal::readOGR(norway_polygon_location)
    norway_polygon <- sf::st_as_sf(norway_polygon,
        coords = c("longitude", "latitude"),
        crs = "+proj=utm +zone=32"
    )

    sfc <- sf::st_transform(norway_polygon, crs = "+proj=longlat +datum=WGS84")

    era5_data_filepath <- "/Users/gar/fdmr/tutorial_data/era5_land_daily.nc"
    era5_precip <- raster::stack(era5_data_filepath)
    era5_precip <- raster::projectRaster(era5_precip, crs = "+proj=utm +zone=32")
    # Create a named list so we can display the dates
    # NOTE - So RasterLayer names can't start with a number
    date_strings <- lubridate::as_date(sub("X", "", sort(names(era5_precip))))
    date_list <- as.list(names(era5_precip))
    names(date_list) <- date_strings

    # Define UI for application that draws a histogram
    ui <- fluidPage(
        titlePanel("4DModeller/fdmr date raster plotting"),
        fluidRow(
            column(
                8,
                leaflet::leafletOutput("raster_map"),
                textOutput("selected_date"),
                # Need to make sure the steps here match the data
                sliderInput(
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
            raster_image <- era5_precip[[layer_name]]

            leaflet::leaflet() %>%
                leaflet::addTiles() %>%
                leaflet::addRasterImage(
                    x = raster_image,
                    opacity = 1
                )
        })

        output$selected_date <- shiny::renderText({
            date_list[[as.character(input$date_slider)]]
        })
    }

    # Run the application
    shinyApp(ui = ui, server = server)
}
#


#' Run the interactive plotting Shiny app
#'
#' @param raster_data
#' @param polygon_data
#'
#' @return NULL
#' @export
plot_interative_map <- function(raster_data = NULL, polygon_data = NULL) {
    # require_package(name = "shinydocu")

    shiny::runApp(raster_mapping_app())
}
