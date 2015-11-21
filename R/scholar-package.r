##' scholar
##'
##' The \code{scholar} package provides functions to extract citation
##' data from Google Scholar.  There are also convenience functions
##' for comparing multiple scholars and predicting h-index scores
##' based on past publication records.
##'
##' @note A complementary set of Google Scholar functions can be found
##' at  \url{http://biostat.jhsph.edu/~jleek/code/googleCite.r}.  The
##' \code{scholar} package was developed independently.
##'
##' @source The package reads data from
##' \url{http://scholar.google.com}.  Dates and citation counts are
##' estimated and are determined automatically by a computer program.
##' Use at your own risk.
##' 
##' @name scholar
##' @docType package
NULL


##' Ensures that specified IDs are correctly formatted
##'
##' @param id a character string specifying the Google Scholar ID.
##' If multiple ids are specified, only the first value is used and a
##' warning is generated.
##' @export
##' @importFrom httr GET
##' @keywords internal
tidy_id <- function(id) {
  if (length(id)!=1) {
    id <- id[1]
    msg <- sprintf("Only one ID at a time; retrieving %s", id)
    warning(msg)
  }

  ## Check with Google to set cookies
  if (getOption("scholar_call_home")) {
      sample_url <- "https://scholar.google.com/citations?user=B7vSqZsAAAAJ"
      sink <- GET(sample_url)      
      options("scholar_call_home"=FALSE, "scholar_handle"=sink)
  }
  
  return(id)
}
