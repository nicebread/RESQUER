#' Compute Relative Rigor Score (RRS)
#'
#' This function computes a relative rigor score based on the indicators provided from an applicant.
#'
#' @param applicant The applicant data that has been preprocessed by the `read_RESQUE` function.
#' @return A tibble containing the dimension (category), maximum points, and relative score for each category, suitable for creating a radar chart.
#' @import dplyr
#' @import stringr
#' @import tibble
#'
#' @export
compute_RRS <- function(applicant) {

  # Which outputs should be scored? At the moment, only publications
  score_list <- applicant$scores$scores[applicant$indicators$P_Suitable == "Yes"]
  n_scorable <- length(score_list)

  scores_all <- get_indicators(sc=score_list)

  scores_all$category <- NA
  scores_all$category[str_detect(scores_all$indicator, "Data")] <- "Open Data"
  scores_all$category[str_detect(scores_all$indicator, "Prereg")] <- "Preregistration"
  scores_all$category[str_detect(scores_all$indicator, "ReproducibleScripts|IndependentVerification")] <- "Reproducible Code \n& Verification"
  scores_all$category[str_detect(scores_all$indicator, "Theorizing")] <- "Theorizing"
  scores_all$category[str_detect(scores_all$indicator, "OpenMaterials")] <- "Open Materials"

  # plausibility check: Which indicators have not been categorized?
  table(scores_all$category, useNA="always")
  scores_all[is.na(scores_all$category), ]

  rigor_category_names <- c("Open Data", "Preregistration", "Reproducible Code \n& Verification", "Theorizing", "Open Materials")

  scores_all_aggr <- scores_all %>%
    group_by(category) %>%
    summarise(
      points = sum(value),
      max_points = sum(max),
      rel_score = mean(rel_score),
    ) %>%
    filter(!is.na(category))

  # = mean of the mean scores (with equal weighting of each research output)
  # Note (TODO): In the webform, it's the *weighted*
  overall_mean_rel_score <- scores_all %>%
    group_by(output) %>%
    summarise(
      scores = sum(value),
      max = sum(max),
      rel_score = scores/max
    )

  # overall rigor score as in webform:
  sum(overall_mean_rel_score$scores)/sum(overall_mean_rel_score$max)

  # equally weighted overall score:
  overall_score <- mean(scores_all_aggr$rel_score)

  categories_present <- nrow(scores_all_aggr)

  radar_dat <- tibble(
    dimension = factor(scores_all_aggr$category),
    max_points = scores_all_aggr$max_points,
    rel_score = scores_all_aggr$rel_score,
    # Alternative: Wedge width proportional to max_points
    #xstart = c(0, cumsum(max_points)[1:(length(max_points)-1)]),
    #xend = cumsum(max_points),
    xstart = 0:(categories_present-1),
    xend = 1:categories_present,
    xmid = (xend-xstart)/2 + xstart
  )

  return(radar_dat)
}
