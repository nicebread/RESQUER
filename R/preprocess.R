#' Preprocess and enrich the raw information from an applicant's JSON
#'
#' This function preprocesses the applicant data and enriches it with additional data.
#' Needs an internet connection to query the BIP! API.
#'
#' @param applicant The applicant data to be preprocessed (as loaded with the `read_RESQUE` function).
#'
#' @return Preprocessed applicant data.
#'
#' @importFrom dplyr filter mutate arrange
#' @importFrom stringr str_replace_all str_trim
#' @importFrom magrittr %>%
#' @importFrom jsonlite fromJSON
#' @importFrom curl curl_fetch_memory
#' @importFrom openalexR oa_fetch
#' @importFrom utils URLencode
#' @importFrom lubridate year
#' @export

# applicant <- read_RESQUE(system.file("extdata", "resque_Schönbrodt.json", package="RESQUER"))
preprocess <- function(applicant) {

  # create missing indicator variables
  ind_vars <- c("P_PreregisteredReplication")
  for (i in ind_vars) {
    if (!i %in% colnames(applicant$indicators)) applicant$indicators[, i] <- NA
  }


  # fix meta data
  if (!is.null(applicant$meta$YearPhD)) {
    applicant$meta$YearPhD <- as.numeric(applicant$meta$YearPhD)
  } else {
    applicant$meta$YearPhD <- NA
  }
  if (!is.null(applicant$meta$AcademicAgeBonus)) {
    applicant$meta$AcademicAgeBonus <- as.numeric(applicant$meta$AcademicAgeBonus)
  } else {
    applicant$meta$AcademicAgeBonus <- 0
  }
  applicant$meta$AcademicAge <- year(Sys.Date()) - applicant$meta$YearPhD - applicant$meta$AcademicAgeBonus

  # Split the research outputs into types, reduce to suitable submissions
  applicant$pubs <- applicant$indicators %>% filter(type == "Publication", P_Suitable == "Yes")

  # assign new verbose factor levels
  applicant$pubs$P_Preregistration2 <- factor(applicant$pubs$P_Preregistration, levels=c("NotApplicable", "No", "Yes", "RegisteredReport"), labels=c("Not<br>Applicable", "Not<br>prereg", "Prereg", "Registered<br>Report"))

  applicant$pubs$replication <- factor(applicant$pubs$P_PreregisteredReplication, levels=c("NotApplicable", "No", "Yes"), labels=c("not<br>applicable", "No", "Yes"))

  # fix some logical dependencies
  applicant$pubs$replication[is.na(applicant$pubs$replication) & applicant$pubs$P_Preregistration2 == "Not preregistered"] <- "No"


  # clean the dois:
  dois <- applicant$indicators$doi
  dois <- dois %>%
    str_replace_all("doi: ", "") %>%
    str_replace_all(" ", "") %>%
    str_trim()

  applicant$indicators$doi_links <- paste0("https://doi.org/", dois)
  applicant$indicators$doi_links_md <- paste0("[", applicant$indicators$doi_links, "](", applicant$indicators$doi_links, ")")

  applicant$indicators$title_links_html <- paste0("<a href='", applicant$indicators$doi_links, "'>", applicant$indicators$Title, "</a>")

  if (!is.null(applicant$indicators$P_TopPaper_Select)) {
    applicant$indicators$P_TopPaper_Select[is.na(applicant$indicators$P_TopPaper_Select)] <- FALSE
  } else {
    applicant$indicators$P_TopPaper_Select <- FALSE
  }


  # CRediT preprocessing
  #--------------------------------------------------------

  credit_tab <- table(applicant$credit$Role, applicant$credit$Degree)

  # arrange credit roles by weight (Lead > Support > Equal), summed across works
  applicant$credit_ordered <- as.data.frame.matrix(credit_tab) %>%
    mutate(
      LeadEqual = Lead + Equal,
      Sum = Lead + Equal + Support + NoRole,
      # normalized weight: All "Lead" (=max) would be 1
      weight = (Lead * 4 + Equal * 3 + Support * 1) / (Sum * 4),
      Role = rownames(.)
    ) %>%
    arrange(-LeadEqual, -Support)

  applicant$credit$Role_ordered <- factor(applicant$credit$Role, levels = rev(rownames(applicant$credit_ordered)))

  # The "CRediT involvement" categories
  # ---------------------------------------------------------------------
  # TODO: Refactor into function

  credit_inv <- applicant$indicators %>% select(contains("CRediT"))
  roles <- colnames(credit_inv) |> str_replace("P_CRediT_", "") |> unCamel0()
  roles[roles == "Writing Review Editing"] <- "Writing: Review & Editing"
  roles[roles == "Writing Original Draft"]  <- "Writing: Original draft"

  main_roles <- rep("", nrow(credit_inv))
  for (i in 1:nrow(credit_inv)) {
    leads <- credit_inv[i, ] == "Lead"
    equals <- credit_inv[i, ] == "Equal"
    main_roles[i] <- paste0(
      ifelse(sum(leads)>0, paste0(
        "<b>Lead:</b> ",
        paste0(roles[leads], collapse=", ")), ""),
      ifelse(sum(equals)>0, paste0(
        "<br><b>Equal:</b> ",
        paste0(roles[equals], collapse=", ")), "")
    )
  }

  credit_inv$sum_lead <- apply(credit_inv[, 1:14], 1, function(x) sum(x=="Lead"))
  credit_inv$sum_equal <- apply(credit_inv[, 1:14], 1, function(x) sum(x=="Equal"))
  credit_inv$sum_leadequal <- apply(credit_inv[, 1:14], 1, function(x) sum(x %in% c("Lead", "Equal")))
  credit_inv$sum_support <- apply(credit_inv[, 1:14], 1, function(x) sum(x=="Support"))

  # define the categories
  credit_inv$CRediT_involvement <- factor(rep("Low", nrow(credit_inv)), levels=c("Low", "Medium", "High", "Very High"), ordered=TRUE)
  credit_inv$CRediT_involvement[credit_inv$sum_lead >= 3] <- "Very High"
  credit_inv$CRediT_involvement[credit_inv$sum_leadequal >= 5] <- "Very High"

  credit_inv$CRediT_involvement[credit_inv$sum_lead %in% c(1, 2)] <- "High"
  credit_inv$CRediT_involvement[credit_inv$sum_leadequal %in% c(3, 4) & credit_inv$CRediT_involvement != "Very High"] <- "High"

  credit_inv$CRediT_involvement[credit_inv$sum_equal %in% c(1, 2) & credit_inv$sum_lead == 0] <- "Medium"
  credit_inv$CRediT_involvement[credit_inv$sum_support >= 5 & credit_inv$CRediT_involvement <= "Medium"] <- "Medium"

  applicant$indicators$CRediT_involvement <- credit_inv$CRediT_involvement
  applicant$indicators$CRediT_involvement_roles <- main_roles

  rm(credit_tab, credit_inv, main_roles)


  #----------------------------------------------------------------
  # Call BIP! API for impact measures
  #----------------------------------------------------------------

  applicant$BIP <- get_BIP(applicant$indicators$dois_normalized)
  applicant$BIP_n_papers <- sum(applicant$BIP$pop_class <= "C5", na.rm=TRUE)
  applicant$BIP_n_papers_top10 <- sum(applicant$BIP$pop_class <= "C4", na.rm=TRUE)

  #----------------------------------------------------------------
  # Retrieve submitted works from OpenAlex
  #----------------------------------------------------------------

  all_pubs <- applicant$indicators[applicant$indicators$type == "Publication", ]

  all_papers <- oa_fetch(entity = "works", doi = all_pubs$doi_links)

  #cat(paste0(nrow(all_papers), " out of ", nrow(all_pubs), " submitted publications could be automatically retrieved with openAlex.\n"))

  if (nrow(all_papers) < nrow(all_pubs)) {
    warning(paste0(
      '## The following papers could *not* be retrieved by openAlex:\n\n',
      all_pubs[!all_pubs$doi_links %in% all_papers$doi, ] %>%
        select(Title, Year, DOI, P_TypePublication)
    ))
  }

  all_papers$n_authors <- sapply(all_papers$author, nrow)

  all_papers$team_category <- cut(all_papers$n_authors, breaks=c(0, 1, 5, 15, Inf), labels=c("Single authored", "Small team (<= 5 co-authors)", "Large team (6-15 co-authors)", "Big Team (> 15 co-authors)"))

  applicant$all_papers <- all_papers
  rm(all_papers)

  #----------------------------------------------------------------
  # Create table of publications
  #----------------------------------------------------------------

  ref_list <- left_join(applicant$all_papers, applicant$indicators %>% select(doi=doi_links, CRediT_involvement, CRediT_involvement_roles, title_links_html, P_TopPaper_Select), by="doi") %>%
    arrange(-P_TopPaper_Select, -as.numeric(CRediT_involvement))

  names_vec <- c()
  for (i in 1:nrow(ref_list)) {
    names_vec <- c(names_vec, format_names(ref_list[i, ], alphabetical = TRUE))
  }

  ref_table <- data.frame(
    Title=paste0(ifelse(ref_list$P_TopPaper_Select, "⭐️", ""), ref_list$title_links_html),
    Authors = names_vec,
    ref_list$CRediT_involvement,
    ref_list$CRediT_involvement_roles
  )

  colnames(ref_table) <- c("Title", "Authors (alphabetical)", "Candidates' CRediT involvement", "Candidates' CRediT main roles")

  applicant$ref_table <- ref_table
  rm(ref_table, ref_list)

  #----------------------------------------------------------------
  # Get TOP factor of the publication venues
  #----------------------------------------------------------------

  TOP <- read.csv(system.file("extdata", "top-factor.csv", package="RESQUER"))

  applicant$TOP_journals <- TOP %>%
    select(issn=Issn, Journal, Total) %>%
    filter(issn %in% applicant$all_papers$issn_l)

  rm(TOP)

  #----------------------------------------------------------------
  # Compute Relative Rigor Score RRS
  #----------------------------------------------------------------

  applicant$RRS <- compute_RRS(applicant)

  return(applicant)
}
