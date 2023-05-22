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

    # Create a named list so we can display the dates
    # NOTE - So RasterLayer names can't start with a number
    date_strings <- lubridate::as_date(sub("X", "", sort(names(raster_data))))
    date_list <- as.list(names(raster_data))
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
    shiny::shinyApp(ui = ui, server = server)
}
#


#' Run the interactive plotting Shiny app
#'
#' @param raster_data Raster data in the form or a RasterStack or RasterBrick
#' @param polygon_data
#'
#' @return NULL
#' @export
plot_interative_map <- function(raster_data = NULL, polygon_data = NULL) {
    require_packages(packages = c("leaflet", "shiny", "stringr"))

    shiny::runApp(raster_mapping_app())
}
