#' Replaces CamelCase in a vector with spaces between words
#' @param x A character vector which should be uncameled.
#' @import stringr
#' @export
unCamel0 <- function(x) {
  gsub("([A-Z]){1}", " \\1", x) |> str_trim()
}

#' For all cells in selected columns, replace CamelCase with spaces between words
#' @param df data.frame
#' @param cname Column names in which cells should be uncameled.
#' @import stringr
#' @export
unCamel <- function(df, cname) {
  if (cname %in% colnames(df)) {
    x <-  df[, cname] |> unlist()
    df[, cname] <- unCamel0(x)
  } else {
    df[, cname] <- NA
  }

  return(df)
}

#' @import stringr
clean_title <- function(x) {
  x |>
    str_replace_all("\n", " ") |>
    str_squish()
}


#' Collate the indicator list into a data.frame
#' @param sc Scores object (as provided by `read_RESQUE`)
#' @param pattern regexp pattern that filters the results. The default selects all indicators
#' @export
get_indicators <- function(sc, pattern=".*") {
  res <- data.frame()
  for (i in 1:length(sc)) {
    ind <- sc[[i]]$indicators
    if (!is.null(ind) & is.list(ind) & (length(ind) > 0)) {
      res <- rbind(res, tibble(
        output = i,
        indicator = names(ind),
        value = sapply(ind, "[[", "value"),
        max = sapply(ind, "[[", "max"),
        rel_score = value/max
      ))
    }
  }

  rownames(res) <- NULL
  return(res[grepl(pattern, res$indicator), ])
}
