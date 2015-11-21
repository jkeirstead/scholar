## Originally started on 14 May 2013

##' Gets profile information for a scholar
##'
##' Gets profile information for a researcher from Google Scholar.
##' Each scholar profile page gives the researcher's name,
##' affiliation, their homepage (if specified), and a summary of their
##' key citation and impact metrics.  The scholar ID can be found by
##' searching Google Scholar at \url{http://scholar.google.com}.
##' 
##' @param id 	a character string specifying the Google Scholar ID.
##' If multiple ids are specified, only the first value is used and a
##' warning is generated.  See the example below for how to profile
##' multiple scholars.
##' 
##' @return 	a list containing the scholar's name, affiliation,
##' citations, impact metrics, fields of study, and homepage
##' 
##' @examples {
##'    ## Gets profiles of some famous physicists
##'    ids <- c("xJaxiEEAAAAJ", "qj74uXkAAAAJ")
##'    profiles <- lapply(ids, get_profile)
##' }
##' @export
##' @importFrom stringr str_trim str_split
##' @importFrom xml2 read_html
##' @importFrom rvest html_table html_nodes html_text
##' @importFrom dplyr "%>%"
##' @importFrom httr GET
get_profile <- function(id) {

  id <- tidy_id(id)
  
  url_template <- "http://scholar.google.com/citations?hl=en&user=%s"
  url <- sprintf(url_template, id)

  ## Generate a list of all the tables identified by the scholar ID
  page <- GET(url, handle=getOption("scholar_handle")) %>% read_html()
  tables <- page %>% html_table()

  ## The citation stats are in tables[[1]]$tables$stats
  ## but the number of rows seems to vary by OS
  stats <- tables[[1]]
  rows <- nrow(stats)
  
  ## The personal info is in
  name <- page %>% html_nodes(xpath="//*/div[@id='gsc_prf_in']") %>% html_text()
  bio_info <- page %>% html_nodes(xpath="//*/div[@class='gsc_prf_il']") %>% html_text()
  affiliation <- bio_info[1]
 
  ## Specialities (trim out HTML non-breaking space)
  specs <- iconv(bio_info[2], from="UTF8", to="ASCII")
  specs <- str_trim(tolower(str_split(specs, ",")[[1]]))

  ## Extract the homepage
  homepage <- page %>% html_nodes(xpath="//*/div[@id='gsc_prf_ivh']//a/@href") %>% html_text() 

  return(list(id=id, name=name, affiliation=affiliation,
              total_cites=as.numeric(as.character(stats[rows-2,2])),
              h_index=as.numeric(as.character(stats[rows-1,2])),
              i10_index=as.numeric(as.character(stats[rows,2])),
              fields=specs,
              homepage=homepage))
}

##' Get historical citation data for a scholar
##'
##' Gets the number of citations to a scholar's articles over the past
##' nine years.
##'
##' @param id a character string specifying the Google Scholar ID.  If
##' multiple ids are specified, only the first value is used and a
##' warning is generated.
##' @details This information is displayed as a bar plot at the top of
##' a standard Google Scholar page and only covers the past nine
##' years.
##' @return a data frame giving the number of citations per year to
##' work by the given scholar
##' @export
##' @importFrom xml2 read_html
##' @importFrom rvest html_nodes html_text
##' @importFrom dplyr "%>%"
##' @importFrom httr GET
get_citation_history <- function(id) {

    ## Ensure only one ID
    id <- tidy_id(id)

    ## Read the page and parse the key data
    url_template <- "http://scholar.google.com/citations?hl=en&user=%s&pagesize=100&view_op=list_works"
    url <- sprintf(url_template, id)
  
    ## A better way would actually be to read out the plot of citations
    page <- GET(url, handle=getOption("scholar_handle")) %>% read_html()
    years <- page %>% html_nodes(xpath="//*/span[@class='gsc_g_t']") %>%
        html_text() %>% as.numeric()
    vals <- page %>% html_nodes(xpath="//*/span[@class='gsc_g_al']") %>%
        html_text() %>% as.numeric()

    df <- data.frame(year=years, cites=vals)
  
    return(df)
}


##' Gets the number of distinct journals in which a scholar has
##' published
##'
##' Gets the number of distinct journals in which a scholar has
##' published.  Note that Google Scholar doesn't provide information
##' on journals \emph{per se}, but instead gives a title for the
##' containing publication where applicable.  So a \emph{journal} here
##' might actually be a journal, a book, a report, or some other
##' publication outlet.
##' 
##' @param id 	a character string giving the Google Scholar id
##' @return the number of distinct journals
##' @export 
get_num_distinct_journals <- function(id) {
  id <- tidy_id(id)
  papers <- get_publications(id)
  return(length(unique(papers$journal)))
}

##' Gets the number of top journals in which a scholar has published
##'
##' Gets the number of top journals in which a scholar has published.
##' The definition of a 'top journal' comes from Acuna et al. and the
##' original list was based on the field of neuroscience.  This
##' function allows users to specify that list for themselves, or use
##' the default Acuna et al. list.  
##'
##' @source DE Acuna, S Allesina, KP Kording (2012) Future impact:
##' Predicting scientific success.  Nature 489,
##' 201-202. \url{http://dx.doi.org/10.1038/489201a}.
##'
##' @param id 	a character string giving a Google Scholar ID
##' @param journals a character vector giving the names of the top
##' journals.  Defaults to Nature, Science, Nature Neuroscience,
##' Proceedings of the National Academy of Sciences, and Neuron.
##' @export
get_num_top_journals <- function(id, journals) {
  id <- tidy_id(id)
  papers <- get_publications(id)

  if (missing(journals)) {
    journals <-c("Nature", "Science", "Nature Neuroscience",
                 "Proceedings of the National Academy of Sciences", "Neuron")
  }
  
  return(length(which(is.element(papers$journal, journals))))
}

