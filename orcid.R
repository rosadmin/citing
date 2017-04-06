library(rorcid)
library(rcrossref)
library(bibtex)
library(scitations)

compact <- function(x) Filter(Negate(is.null), x)

bibfile <- "citations.bib"
cites <- bibtex::read.bib(bibfile)
sci <- lapply(cites, function(z) {
  z <- unclass(z)[[1]]
  scitations::scitation(attr(z, "bibtype"), attr(z, "key"), .list = z)
})
sci <- compact(dois)
(scidf <- scitation_df(.list = sci))
dois <- unname(unlist(compact(lapply(sci, "[[", "doi"))))

# ORCID ------------------------
orc <- rorcid::orcid_doi(dois)
orc <- stats::setNames(orc, dois)
orc <- bind_rows(orc, .id = 'doi')


# Crossref ------------------------
# some of these have funder info, some don't
wrks <- cr_works(dois = dois)
wrks <- wrks$data
wrks <- wrks %>% rename(doi = DOI)

## flatten
### funders
fund <- stats::setNames(lapply(alldat$funder, function(z) {
  if (length(z) == 0) {
    dplyr::data_frame()
  } else {
    aw <- unname(apply(z, 1, function(b) {
      tmp <- b[grep("award", names(b))]
      if (all(is.na(tmp))) NA else paste0(tmp, collapse = ", ")
    }))
    z <- select(z, -starts_with("award"))
    data.frame(z, award = aw, stringsAsFactors = FALSE)
  }
}), alldat$doi)
fund <- bind_rows(fund, .id = "doi")

### funders
author <- stats::setNames(lapply(alldat$author, function(z) {
  if (length(z) == 0) {
    dplyr::data_frame()
  } else {
    aw <- unname(apply(z, 1, function(b) {
      tmp <- b[grep("affiliation", names(b))]
      if (all(is.na(tmp))) NA else paste0(tmp, collapse = ", ")
    }))
    z <- select(z, -starts_with("affiliation"))
    data.frame(z, affiliation = aw, stringsAsFactors = FALSE)
  }
}), alldat$doi)
author <- bind_rows(author, .id = "doi")


# Combine ------------------------
wrks[, sapply(wrks, class) == "list"] <- NULL
orc[, sapply(orc, class) == "list"] <- NULL
fa <- dplyr::full_join(fund, author, by = "doi")
alldat <- dplyr::full_join(wrks, orc, by = "doi")
alldat <- dplyr::full_join(alldat, fa, by = "doi")

# write
readr::write_csv(alldat, 'crossref_and_orcid.csv')


