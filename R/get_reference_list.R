#' Format Author Names of a single OpenAlex work object
#'
#' Formats the names of authors in a data frame.
#'
#' @param works A data frame containing author information.
#' @param alphabetical Logical indicating whether to sort the names alphabetically by last name (default: TRUE).
#'
#' @return A character vector of formatted names.
#' @export
#'
format_names <- function(works, alphabetical=TRUE) {
  if (nrow(works)>1) warning("Only the author names of the first work are returned.")
  al <- works[1, ]$author[[1]]
  n_authors <- nrow(al)
  names <- al$au_display_name
  namesplit <- str_split(names, " ")
  lastnames <- sapply(namesplit, function(x) x[length(x)])
  lastname_order <- order(lastnames)

  names2 <- sapply(namesplit, function(x) {
    # first name initials:
    FNI <- sapply(x[1:(length(x)-1)], function(y) str_sub(y, 1, 1))
    paste0(x[length(x)], ", ", paste0(FNI, ".", collapse=" "))
  })

  if (alphabetical) {
    names2 <- names2[lastname_order]
  }

  if (n_authors == 1) {
    return(names2)
  } else if (n_authors == 2) {
    return(paste(names2, collapse=" & "))
  } else {
    return(paste0(paste(names2[1:(length(names2)-1)], collapse=", "), " & ", names2[length(names2)]))
  }
}


#' Convert OpenAlex works into formatted reference list (in HTML)
#'
#' This function generates a reference list for a given set of OpenAlex works. The formatting is inspired by APA style, but it does (by default) omit the journal name and can re-order authors alphabetically
#'
#' @param works A data frame containing information about the works to be referenced. Each row of the data frame represents a single work, and columns represent different attributes of the works.
#' @param alphabetical Logical value indicating whether the references should be sorted alphabetically by author's last name. Default is TRUE.
#' @param journal_name Logical value indicating whether the journal name should be displayed. Default is FALSE.
#' @param bullet Logical value indicating whether the references should be formatted as an unordered list (<ul>) or separated by line breaks (<br>). Default is TRUE.
#' @return A character vector containing the formatted APA references.
#' @export
#'
get_reference_list <- function(works, alphabetical=TRUE, journal_name=FALSE, bullet=TRUE) {
  res <- ifelse(bullet, "<ul>", "")
  for (i in 1:nrow(works)) {
    n_authors <- nrow(works[i, ]$author[[1]])

    res <- paste0(res, paste0(
      ifelse(bullet, "<li>", ""),
      format_names(works[i, ], alphabetical=alphabetical),

      " (", works[i, ]$publication_year, "). ",
      ifelse(journal_name, "", "<i>"),
      str_to_sentence(works[i, ]$display_name),
      ifelse(journal_name, "", "</i>"),
      ifelse(journal_name, paste0("<i>", works[i, ]$so, "</i>"), ""),
      ". <a href='", works[i, ]$doi, "'>", works[i, ]$doi, "</a>",
      ifelse(bullet, "</li>", "<br>")
    ))
  }

  res <- paste0(res, ifelse(bullet, "</ul>", ""))
  return(res)
}

