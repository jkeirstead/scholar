.onAttach <- function(libname, pkgname) {
    sample_url <- "https://scholar.google.com/citations?user=B7vSqZsAAAAJ"
    sink <- httr::GET(sample_url)
}
