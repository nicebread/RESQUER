#' Radar Chart for Relative Rigor Score
#'
#' This function generates a radar chart that visualizes the rigor profile for a given applicant's data.
#'
#' @param radardat The output form the `compute_RRS` function.
#' @param overall_score Show the overall score in the plot?
#' @param base_size Base font size
#'
#' @return A radar chart visualizing the rigor profile for the applicant, showing the distribution of scores across different dimensions.
#'
#' @import ggplot2
#' @import dplyr
#' @import scales
#'
#' @examples
#' radar_dat <- data.frame(
#'   dimension = c("Open Data", "Open Materials", "Preregistration", "Reproducible Code \n& Verification", "Theorizing \n& Formal Modeling"),
#'   max_points = rep(10, 5),
#'   rel_score=c(0.5, 0.3, 0.1, 0.6, 0.2),
#'   xstart = 0:4,
#'   xend = 1:5,
#'   xmid=0:4 + 0.5
#' )
#' RRS_radarchart(radar_dat)
#'
#' @importFrom ggplot2 ggplot geom_rect coord_polar geom_hline geom_text xlab ylab ggtitle scale_x_continuous scale_y_continuous theme_void guides scale_fill_brewer
#' @importFrom dplyr %>%
#' @importFrom scales brewer_pal
#'
#' @export
RRS_radarchart <- function(radar_dat, title="", overall_score=FALSE, base_size=14) {
  max_points <- sum(radar_dat$max_points)

  # overall score, equally weighted across all publications:
  overall_score <- mean(radar_dat$rel_score)

  # Standardized and categorized rigor scores:
  # Standarized within category
  # Quantiles in Franka's study were:
  # 0.63 (top 1%), 0.54 (top 10%), 0.50 (top 20%), 0.30 (top 50%)
  # radar_applicant$indicators$norm_rigor <- cut(radar_applicant$indicators$rel_score, breaks=c(0, 0.30, .50, .54, 1),
  #  labels=c("low50", "top50", "top20", "top10"))

  p1 <- radar_dat %>% ggplot() +
    geom_rect(aes(xmin=xstart, xmax=xend, ymin=0, ymax=rel_score, fill=dimension)) +
    coord_polar("x", start=0) +
    geom_hline(yintercept=0.30, col="grey30") +  # top 50%
    geom_hline(yintercept=0.54, col="grey50") + # top 10%
    geom_hline(yintercept=0.63, col="grey70") +  # top  1%
    geom_text(x=max_points*0, y=0.30, label = "Top 50%", col="grey30", size=base_size/4, vjust=-0.2) +
    geom_text(x=max_points*0, y=0.54, label = "Top 10%", col="grey50", size=base_size/4, vjust=-0.2) +
    geom_text(x=max_points*0, y=0.63, label = "Top 1%" , col="grey50", size=base_size/4, vjust=-0.2) +

    xlab("") + ylab("") +
    scale_x_continuous(labels = NULL, breaks = NULL) + scale_y_continuous(labels = NULL, breaks = NULL, limits=c(0, 1)) + theme_void(base_size=base_size) +
    guides(fill=guide_legend("Rigor Dimension")) +
    scale_fill_brewer(palette="Set3") +
    geom_text(aes(x=xmid, y=0.75, label = dimension), vjust = -0.5, size=base_size/4)

  if (overall_score == TRUE) {
    p1 <- p1 + ggtitle(title, subtitle = paste0("Overall score = ", round(overall_score, 2)))
  } else {
    p1 <- p1 + ggtitle(title)
  }

  p1
}
