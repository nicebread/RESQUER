#' Compute Relative Rigor Score (RRS)
#'
#' This function computes a relative rigor score based on the indicators provided from an applicant.
#'
#' @param applicant The applicant data that has been preprocessed by the `read_RESQUE` function.
#' @param sectors Should sectors be all equally sized ("equal") or weighted by the maximum sum of attainable points in each category ("weighted")?
#' @return A tibble containing the dimension (category), maximum points, and relative score for each category, suitable for creating a radar chart.
#' @import dplyr
#' @import stringr
#' @import tibble
#'
#' @export
compute_RRS <- function(applicant, sectors = c("weighted", "equal")) {

  sectors <- match.arg(sectors, choices=c("weighted", "equal"))

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

  # = mean of the mean scores (with equal weighting of each research output)
  RRS_by_paper <- scores_all %>%
    group_by(output) %>%
    summarise(
      scores = sum(value),
      max_points = sum(max),
      rel_score = scores/max_points
    )

  mean(RRS_by_paper$rel_score)

  RRS_by_category <- scores_all %>%
    group_by(category) %>%
    summarise(
      scores = sum(value),
      max_points = sum(max),
      rel_score = scores/max_points
    ) %>%
    filter(!is.na(category))


  # overall rigor score as in webform:
  sum(RRS_by_paper$scores)/sum(RRS_by_paper$max_points)

  # overall score averaged across papers (each paper gets same weight)
  overall_score <- mean(RRS_by_paper$rel_score)

  n_categories_present <- nrow(RRS_by_category)

  radar_dat <- tibble(
    dimension = factor(RRS_by_category$category),
    max_points = RRS_by_category$max_points,
    rel_score = RRS_by_category$rel_score
  )

  if (sectors == "equal") {
    # Sectors all have the same width
    radar_dat <- radar_dat %>% mutate(
      xstart = 0:(n_categories_present-1),
      xend = 1:n_categories_present,
      xmid = (xend-xstart)/2 + xstart
    )
  } else if (sectors == "weighted") {
    # Sector width proportional to max_points
    radar_dat <- radar_dat %>% mutate(
      xstart = c(0, cumsum(max_points)[1:(length(max_points)-1)]),
      xend = cumsum(max_points),
      xmid = (xend-xstart)/2 + xstart
    )
  }


  return(list(
    overall_score = overall_score,
    paper_scores = RRS_by_paper,
    n_papers = nrow(RRS_by_paper),
    sector_scores = RRS_by_category,
    radar_dat = radar_dat
  ))
}

#RRS <- compute_RRS(applicant, sectors="weighted")
#RRS_radarchart(RRS, overall_score = TRUE)
