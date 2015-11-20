#' @import httr
.onLoad <- function(libname, pkgname) {
    
    sample_url <- "https://scholar.google.com/citations?user=B7vSqZsAAAAJ"
    sink <- GET(sample_url)

}
