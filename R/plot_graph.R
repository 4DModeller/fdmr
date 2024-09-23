#' Plot a bar chart using ggplot2
#'
#' @param data Data to plot (all data types recognised by ggplot2::geom_bar() like data.frame)
#' @param x x-axis data, must be a Date object
#' @param y y-axis data
#' @param breaks Break points, must be a sequence of dates
#' @param x_label x-axis label
#' @param y_label y-axis label
#' @param fill Fill colour
#' @param colour Line colour
#'
#' @return ggplot
#' @export
plot_barchart <- function(data, x, y, breaks, x_label, y_label, fill = "pink", colour = "blue") {
  ggplot2::ggplot() +
    ggplot2::geom_bar(data = data, ggplot2::aes(x = x, y = y), stat = "identity", fill = fill, color = colour) +
    ggplot2::theme_bw() +
    ggplot2::xlab(x_label) +
    ggplot2::ylab(y_label) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 7)
    ) +
    ggplot2::scale_x_date(date_labels = "%Y-%m-%d", breaks = breaks) +
    ggplot2::scale_y_continuous(labels = scales::label_comma())
}


# #' Create a timeseries plot using ggplot2
# #' Note that the x-axis will be converted to Date objects
# #'
# #' @param data Data to plot
# #' @param x Name of datetime column
# #' @param y Name of y column
# #' @param x_label x-axis label
# #' @param y_label y-axis label
# #' @param line_colour Line colour
# #' @param line_width Line width
# #'
# #' @return ggplot
# #' @export
# plot_timeseries <- function(data, x, y, x_label, y_label, line_colour = "red", line_width = 1) {
#   ggplot2::ggplot(data, ggplot2::aes(x = as.Date(.data[[x]]), y = .data[[y]])) +
#   ggplot2::geom_line(color = line_colour, size=line_width) +
#   ggplot2::labs(x = x_label, y = y_label)
# }

#' Plot a boxplot using ggplot2
#'
#' @param data Data to plot (all data types recognised by ggplot2::geom_boxplot() like data.frame)
#' @param x x-axis data, must be a Date object
#' @param y y-axis data
#' @param breaks Break points, must be a sequence of dates
#' @param x_label x-axis label
#' @param y_label y-axis label
#'
#' @return ggplot
#' @export
plot_boxplot <- function(data, x, y, breaks, x_label, y_label) {
  ggplot2::ggplot() +
    ggplot2::geom_boxplot(
      data = data,
      ggplot2::aes(
        x = x, y = y,
        group = as.factor(date)
      ),
      outlier.size = 0.5
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(
        angle = 90,
        hjust = 1,
        size = 7
      )
    ) +
    ggplot2::scale_x_date(date_labels = "%Y-%m-%d", breaks = breaks) +
    ggplot2::xlab(x_label) +
    ggplot2::ylab(y_label)
}

#' Plot timeseries data
#'
#' @param data Data to plot (data.frame with x-axis column a character convertible to Date)
#' @param x Name of column to plot on x-axis
#' @param y Name of column to plot on y-axis
#' @param breaks Date break points, must be a sequence of dates
#' @param x_label x-axis label
#' @param y_label y-axis label
#' @param title Figure title
#' @param line_width Line width
#' @param line_colour Line colour
#' @param horizontal_y y-intercept for horizontal line
#' @param vertical_x x-intercept for vertical line
#' @param x_lim Limits for x-axis continuous scale, vector passed to scale_x_continuous
#' @param y_lim Limits for y-axis continuous scale, vector passed to scale_y_continuous
#'
#' @return ggplot
#' @export
plot_timeseries <- function(data,
                            x,
                            y,
                            breaks = NULL,
                            x_label = NULL,
                            y_label = NULL,
                            title = NULL,
                            line_colour = "blue",
                            horizontal_y = NULL,
                            vertical_x = NULL,
                            x_lim = NULL,
                            y_lim = NULL) {
  lineplot <- ggplot2::ggplot(data) +
    ggplot2::geom_line(
      ggplot2::aes(x = as.Date(.data[[x]]), y = .data[[y]], group = 1),
      color = line_colour,
      size = 1,
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(
        angle = 90,
        hjust = 1,
        size = 7
      )
    )

  if (!is.null(breaks)) {
    lineplot <- lineplot + ggplot2::scale_x_date(date_labels = "%Y-%m-%d", breaks = breaks)
  }

  if (!is.null(x_label)) {
    lineplot <- lineplot + ggplot2::xlab(x_label)
  }

  if (!is.null(y_label)) {
    lineplot <- lineplot + ggplot2::ylab(y_label)
  }

  if (!is.null(x_lim)) {
    lineplot <- lineplot + ggplot2::scale_x_continuous(labels = scales::label_comma(), limits = x_lim)
  }

  if (!is.null(y_lim)) {
    lineplot <- lineplot + ggplot2::scale_y_continuous(labels = scales::label_comma(), limits = y_lim)
  }


  if (!is.null(title)) {
    lineplot <- lineplot + ggplot2::labs(title = title)
  }

  if (!is.null(horizontal_y)) {
    lineplot <- lineplot + ggplot2::geom_hline(
      yintercept = horizontal_y,
      col = "red",
      linetype = "dashed"
    )
  }

  if (!is.null(vertical_x)) {
    lineplot <- lineplot + ggplot2::geom_vline(
      xintercept = vertical_x,
      col = "red",
      linetype = "dashed"
    )
  }

  return(lineplot)
}

#' Create a line plot with a confidence interval.
#'
#' @param data Data to plot (all data types recognised by ggplot2::geom_line() like data.frame)
#' @param x x-axis data, must be a Date object
#' @param y1 y1 line data - solid
#' @param y2 y2 line data - dashed
#' @param y3 y3 line data - dashed
#' @param breaks Breaks vector, must be a sequence of dates
#' @param x_label x-axis label
#' @param y_label y-axis label
#' @param y1_colour Colour for y1
#' @param y2_colour Colour for y2
#' @param y3_colour Colour for y3
#' @param x_lim x-axis limits as vector e.g. c(0, 0.1)
#' @param y_lim y-axis limits as vector e.g. c(0, 1.0)
#'
#' @return ggplot
#' @export
plot_line_average <- function(data, x,
                              y1,
                              y2,
                              y3,
                              breaks,
                              x_label,
                              y_label,
                              y1_colour = "blue",
                              y2_colour = "red",
                              y3_colour = "red",
                              x_lim = NULL,
                              y_lim = NULL) {
  line_average <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = data,
      ggplot2::aes(x = x, y = y1, group = 1),
      color = y1_colour
    ) +
    ggplot2::geom_line(
      data = data,
      ggplot2::aes(x = x, y = y2, group = 1),
      color = y2_colour,
      linetype = "dashed"
    ) +
    ggplot2::geom_line(
      data = data,
      ggplot2::aes(x = x, y = y3, group = 1),
      color = y3_colour,
      linetype = "dashed"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(
        angle = 90,
        hjust = 1,
        size = 7
      )
    ) +
    ggplot2::scale_x_date(date_labels = "%Y-%m-%d", breaks = breaks) +
    ggplot2::xlab(x_label) +
    ggplot2::ylab(y_label)

  if (!is.null(x_lim)) {
    line_average <- line_average + do.call(ggplot2::xlim, as.list(x_lim))
  }

  if (!is.null(y_lim)) {
    line_average <- line_average + do.call(ggplot2::ylim, as.list(y_lim))
  }

  return(line_average)
}
