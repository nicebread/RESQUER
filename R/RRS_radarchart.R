#' Radar Chart for Relative Rigor Score
#'
#' This function generates a radar chart that visualizes the rigor profile for a given applicant's data.
#'
#' @param RRS The output from the `compute_RRS` function.
#' @param title The main title for the plot.
#' @param overall_score Show the overall score in the plot?
#' @param base_size Base font size
#' @param minimal If TRUE, a very simple chart without any legends is drawn
#' @param show.legend Show the legend to the right?
#' @param show.n_pub Show the number of publication on which the chart is based?
#'
#' @return A radar chart visualizing the rigor profile for the applicant, showing the distribution of scores across different dimensions.
#'
#' @import ggplot2
#' @import dplyr
#' @import scales
#'
#' @examples
#' radar_dat <- data.frame(
#'   dimension = c("Open Data", "Open Materials", "Preregistration",
#'     "Reproducible Code \n& Verification", "Theorizing \n& Formal Modeling"),
#'   max_points = rep(10, 5),
#'   rel_score=c(0.5, 0.3, 0.1, 0.6, 0.2),
#'   xstart = 0:4,
#'   xend = 1:5,
#'   xmid=0:4 + 0.5
#' )
#' RRS <- list(radar_dat=radar_dat, overall_score=0.5, n_papers=4)
#' RRS_radarchart(RRS)
#'
#' @importFrom ggplot2 ggplot geom_rect coord_polar geom_hline geom_text xlab ylab ggtitle scale_x_continuous scale_y_continuous theme_void guides scale_fill_brewer
#' @importFrom dplyr %>%
#' @importFrom scales brewer_pal
#'
#' @export
RRS_radarchart <- function(RRS, title="", overall_score=FALSE, minimal=FALSE, show.legend=TRUE, show.n_pub=TRUE, base_size=14) {

  # Standardized and categorized rigor scores:
  # Standarized within category
  # Quantiles in Franka's study were:
  # 0.63 (top 1%), 0.54 (top 10%), 0.50 (top 20%), 0.30 (top 50%)
  # radar_applicant$indicators$norm_rigor <- cut(radar_applicant$indicators$rel_score, breaks=c(0, 0.30, .50, .54, 1),
  #  labels=c("low50", "top50", "top20", "top10"))

  p1 <- RRS$radar_dat %>% ggplot() +
    geom_rect(aes(xmin=xstart, xmax=xend, ymin=0, ymax=rel_score, fill=dimension), show.legend=show.legend) +
    coord_polar("x", start=0) +
    xlab("") + ylab("") +
    scale_x_continuous(labels = NULL, breaks = NULL) + scale_y_continuous(labels = NULL, breaks = NULL, limits=c(0, 1.2)) + theme_void(base_size=base_size)

  subtitle <- ""

  if (!minimal) {
    p1 <- p1 + geom_rect(aes(xmin=xstart, xmax=xend, ymin=rel_score, ymax=1), fill="#EFEFEF", color="#DEDEDE") +
      guides(fill=guide_legend("Rigor Dimension")) +
      scale_fill_brewer(palette="Set3") +
      geom_text(aes(x=xmid, y=1, label = dimension), vjust = 0, hjust=0.5, size=base_size/5)

      if (overall_score == TRUE) {
        subtitle <- paste0(subtitle, "Overall score = ", round(RRS$overall_score, 2), ". ")
      }
      if (show.n_pub==TRUE) {
        subtitle <- paste0(subtitle, "Chart is based on ", RRS$n_papers, ifelse(RRS$n_papers == 1, " publication", " publications"), ".")
      }
      if (!show.n_pub & !overall_score) {
        p1 <- p1 + theme(plot.margin=unit(c(-2.5, 0, -1, 0), 'cm'))
      }

  } else {
    p1 <- p1 + guides(fill="none") +
      geom_hline(yintercept=1, col="grey80") +
      theme(plot.margin=unit(rep(-2.5, 4), 'cm')) # t, r, b, l
      # + theme(plot.background = element_rect(fill = "darkblue"))  # for debugging the margin
  }

  p1 <- p1 + ggtitle(title, subtitle=subtitle)

  p1
}




# Full Circles at specific sector heights
#geom_hline(yintercept=0.30, col="grey30") +  # top 50%
#geom_hline(yintercept=0.54, col="grey50") + # top 10%
#geom_hline(yintercept=0.63, col="grey70") +  # top  1%

# For sector specific norm values
#geom_segment(x=0, xend=10, y=0.30, yend=0.30, col="grey30") +

#geom_text(x=0, y=0.30, label = "Top 50%", col="grey30", size=base_size/4, vjust=-0.2) +
#geom_text(x=0, y=0.54, label = "Top 10%", col="grey50", size=base_size/4, vjust=-0.2) +
#geom_text(x=0, y=0.63, label = "Top 1%" , col="grey50", size=base_size/4, vjust=-0.2) +
