# see https://plotly.com/r/horizontal-bar-charts/


#' BC_h
#'
#' This function creates a horizontal stacked bar chart in plotly with bar labels displaying category labels, values, and percentages.
#'
#' @param cat_labels a character vector of category labels for the bars.
#' @param values a numeric vector of values for each category.
#' @param colors a character vector of colors for each category.
#' @param rev Should the order of categories be reversed?
#'
#' @return a plotly object of the stacked bar chart.
#'
#' @examples
#' BC_h(
#'   cat_labels=c("Yes", "No", "not applicable"),
#'   values=c(10, 2, 4),
#'   colors=c("#90c916", "#FED976", "#eeeeee"),
#'   title="The publication contained a preregistered replication attempt (either direct/close or conceptual)")
#'
#' BC_h(
#'   cat_labels=c("Yes", "No"),
#'   values=c(10, 2),
#'   colors=c("#90c916", "#FED976"))
#'
#' BC_h(cat_labels=c("Yes"), values=c(10), colors=c("#90c916"))
#'
#' @importFrom plotly plot_ly
#' @importFrom plotly add_trace
#' @importFrom plotly add_annotations
#' @importFrom plotly layout
#' @importFrom stats as.formula
#' @import dplyr
#' @export

BC_h <- function(cat_labels, values, colors, title="", rev=FALSE) {

  # remove zero values
  cat_labels <- cat_labels[values!=0]
  colors <- colors[values!=0]
  values <- values[values!=0]

  if (rev==TRUE) {
    cat_labels <- rev(cat_labels)
    colors <- rev(colors)
    values <- rev(values)
  }

  df <- data.frame(t(values))
  colnames(df) <- paste0("X", 1:length(values))

  df_perc <- paste0(round(df*100 / sum(df)), "%")
  names(df_perc) <- names(df)

  fig <- plot_ly(df,
                 x = as.formula(paste0("~", colnames(df)[1])),
                 y = ~1,
                 type = 'bar', name = cat_labels[1],
                 orientation = 'h',
                 marker = list(color = colors[1],
                               line = list(color = 'rgb(248, 248, 249)', width = 1)),
                 width = 300, height = 200
        )

  if (length(values) > 1) {
    for (i in 2:length(values)) {
      fig <- fig %>% add_trace(
        x = as.formula(paste0("~", colnames(df)[i])),
        name = cat_labels[i],
        marker = list(color = colors[i],
                      line = list(color = 'rgb(248, 248, 249)', width = 1))
      )
    }
  }

  fig <- fig %>% layout(
    title = list(
      text = title,
      x = 0.05,  # Horizontal position (0 left, 0.5 center, 1 right)
      y = 0.95,  # Vertical position (0 bottom, 0.5 middle, 1 top)
      xanchor = 'left',  # 'left', 'center', or 'right'
      yanchor = 'top',      # 'top', 'middle', or 'bottom',
      font = list(size = 14)
    ),
    xaxis = list(
      title = "",
      showgrid = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      zeroline = FALSE,
      autorange = FALSE,
      range = c(0, sum(values))
    ),
    yaxis = list(
      title = "",
      showgrid = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      zeroline = FALSE),
    barmode = 'stack',
    showlegend = FALSE,
    margin = list(l = 0)
  )




  # labeling the percentages of each bar (x_axis)
  x_pos <- c(0, cumsum(values)[1:(length(values)-1)]) + (values/2)
  for (i in 1:length(values)) {

    fig <- fig %>% add_annotations(
      x = x_pos[i], y = 1,
      text = paste0(cat_labels[i], "<br>(n=", values[i], ")<br>", df_perc[i]),
      font = list(family = 'Arial', size = 12,
                  color = '#000000'),
      showarrow = FALSE)
  }


  fig
}


BC_h(
  cat_labels=c("Yes", "No", "not<br>applicable"),
  values=c(10, 2, 4),
  colors=c("#90c916", "#FED976", "#eeeeee"),
  title="Preregistered replication attempt")
