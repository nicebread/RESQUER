# file <- system.file("extdata", "resque_Schönbrodt.json", package="RESQUER")

#' Read and preprocess a single RESQUE JSON file
#'
#' @param file Path to RESQUE JSON file
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @export
read_RESQUE <- function(file) {
  dat0 <- read_json(file, simplifyVector = TRUE)

  meta <- dat0[1, 1:16]
  dat <- dat0[-1, ]

  # clean and create some fields
  meta$FullName <- paste0(meta$FirstName, " ", meta$LastName)
  dat$Title <- clean_title(dat$Title)
  dat$TitleLink <- paste0("[", dat$Title, "](", dat$DOI, ")")

  # remove unnecessary column that breaks the structure of dat (nested data frame)
  dat$queryConfig <- NULL

  # read the scores
  scores <- score_all_from_file(file=file)

  # remove the first element: This is the meta-information which has no scores
  # Now each list entry is one publication, in the same order as in `dat`
  scores$scores <- scores$scores[-1]

  # Create nice factor labels
  dat$type <- factor(dat$type, levels=c("pub", "data", "software"), labels=c("Publication", "Data set", "Research software"))

  dat <- unCamel(dat, "P_TypePublication")
  dat <- unCamel(dat, "P_ReproducibleScripts")

  dat$dois_normalized <- str_extract(dat$DOI, pattern="10.\\d{4,9}/[-._;()/:a-z0-9A-Z]+")

  #-----------------------------------------------------------------
  # CRediT
  credit <- dat %>%
    select(contains("CRediT")) %>%
    pivot_longer(everything(), names_prefix = "P_CRediT_")

  colnames(credit) <- c("Role", "Degree")
  credit$Degree <- factor(credit$Degree, levels = rev(c("Lead", "Equal", "Support", "NoRole", "NA")), labels = rev(c("Lead", "Equal", "Support", "NoRole", "not applicable")))

  # add space to camelCase; make nice labels
  credit <- unCamel(credit, "Role")
  credit$Role[credit$Role == "Writing Review Editing"] <- "Writing: Review & Editing"
  credit$Role[credit$Role == "Writing Original Draft"]  <- "Writing: Original draft"


  return(list(meta=meta, indicators=dat, scores=scores, credit=credit))
}
