# Ugly hack for CRAN checks
utils::globalVariables(c("year", "cites"))

##' Compare the citation records of multiple scholars
##'
##' Compares the citation records of multiple scholars.  This function
##' compiles a data frame comparing the citations received by each of
##' the scholar's publications by year of publication.
##'
##' @param ids 	a vector of Google Scholar IDs
##' @param pagesize an integer specifying the number of articles to
##' fetch for each scholar
##' @return a data frame giving the ID of each scholar and the total
##' number of citations received by work published in a year.
##' @examples {
##' \dontrun{
##'     ## How do Richard Feynmann and Isaac Newton compare?
##' 	ids <- c("B7vSqZsAAAAJ", "xJaxiEEAAAAJ")
##'     df <- compare_scholars(ids)
##' }
##' }
##' @export
##' @import plyr
compare_scholars <- function(ids, pagesize=100) {

  ## Load in the publication data and summarize
  data <- lapply(ids, function(x) return(cbind(id=x, get_publications(x, pagesize=pagesize))))
  data <- ldply(data)
  data <- ddply(data, .(id, year), summarize, cites=sum(cites, na.rm=TRUE))
  data <- ddply(data, .(id), transform, total=cumsum(cites))

  ## Fetch the scholar names 
  tmp <- lapply(ids, get_profile)
  names <- ldply(tmp, function(l) return(data.frame(id=l$id, name=l$name)))

  ## Merge together with the citation info
  data <- merge(data, names)
  return(data)
}

##' Compare the careers of multiple scholars
##'
##' Compares the careers of multiple scholars based on their citation
##' histories.  The scholar's \emph{career} is defined by the number
##' of citations to his or her work in a given year (i.e. the bar
##' chart at the top of a scholar's profile). The function has an
##' \code{career} option that allows users to compare scholars
##' directly, i.e. relative to the first year in which their
##' publications are cited.
##'
##' @param ids 	a character vector of Google Scholar IDs
##' @param career  a boolean, should a column be added to the results
##' measuring the year relative to the first citation year.  Default =
##' TRUE
##' 
##' @examples {
##' 	## How do Richard Feynmann and Isaac Newton compare?
##' 	ids <- c("B7vSqZsAAAAJ", "xJaxiEEAAAAJ")
##'     df <- compare_scholar_careers(ids)
##' }
##' @export
##' @import plyr
compare_scholar_careers <- function(ids, career=TRUE) {

  data <- lapply(ids, function(x) return(cbind(id=x, get_citation_history(x))))
  data <- ldply(data)

  ## Calculate the minimum year for each scholar and create a career year
  if (career) {
    data <- ddply(data, .(id), transform, career_year=year-min(year))
  } 

  ## Fetch the scholar names 
  tmp <- lapply(ids, get_profile)
  names <- ldply(tmp, function(l) return(data.frame(id=l$id, name=l$name)))

  ## Add the name data
  data <- merge(data, names)
  return(data)
}
