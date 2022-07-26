# Ugly hack for CRAN checks
utils::globalVariables(c("name"))

## Originally started on 14 May 2013

##' Gets profile information for a scholar
##'
##' Gets profile information for a researcher from Google Scholar.
##' Each scholar profile page gives the researcher's name,
##' affiliation, their homepage (if specified), and a summary of their
##' key citation and publication availability metrics. The scholar
##' ID can be found by searching Google Scholar at 
##' \url{http://scholar.google.com}.
##'
##' @param id 	a character string specifying the Google Scholar ID.
##' If multiple ids are specified, only the first value is used and a
##' warning is generated. See the example below for how to profile
##' multiple scholars.
##'
##' @return 	a list containing the scholar's name, affiliation,
##' citations, impact and publication availability metrics,
##' research interests, homepage and coauthors.
##' 
##' Metrics include:
##' \itemize{
##'  \item {total_cites}   {combined citations to all publications}
##'  \item {h_index}       {the largest number h such that h publications each have at least h citations}
##'  \item {i10_index}     {the number of publications that each have at least 10 citations}
##'  \item {available}     {the number of publications that have a version online that can be read for free (though not necessarily reusable under an open access license)}
##'  \item {not_available} {the number of publications only available behind a paywall}
##' }
##'
##' @examples {
##'    ## Gets profiles of some famous physicists
##'    ids <- c("xJaxiEEAAAAJ", "qj74uXkAAAAJ")
##'    profiles <- lapply(ids, get_profile)
##' }
##' @export
##' @importFrom stringr str_trim str_split
##' @importFrom xml2 read_html
##' @importFrom rvest html_table html_nodes html_text html_children
##' @importFrom dplyr "%>%"
get_profile <- function(id) {
    site <- getOption("scholar_site")
    url_template <- paste0(site, "/citations?hl=en&user=%s")
    url <- compose_url(id, url_template)

    ## Generate a list of all the tables identified by the scholar ID
    page <- get_scholar_resp(url)
    if (is.null(page)) return(NA)

    page <- page %>% read_html()
    tables <- page %>% html_table()

    
  ## The citation stats are in tables[[1]]$tables$stats
  ## but the number of rows seems to vary by OS
  stats <- tables[[1]]
  rows <- nrow(stats)

  ## The personal info is in
  name <- page %>% html_nodes(xpath="//*/div[@id='gsc_prf_in']") %>% html_text()
  bio_info <- page %>% html_nodes(xpath = "//*/div[@class='gsc_prf_il']")
  affiliation <- html_text(bio_info)[1]

  ## Specialities (leave capitalisation as is)
  specs <- html_nodes(bio_info[3],".gsc_prf_inta") %>% html_text()
  specs <- str_trim(iconv(specs, from = "UTF8", to = "ASCII"))

  ## Extract the homepage
  homepage <- page %>% html_nodes(xpath="//*/div[@id='gsc_prf_ivh']//a/@href") %>% html_text()

  ## Grab all coauthors
  coauthors <- list_coauthors(id, n_coauthors = 20) # maximum availabe in profile

  ## Check 'publicly available' vs 'not publicly available' statistics
  ## (note, not actually detecting open access, just free-to view) 
  available <- page %>% html_nodes(xpath = "//*/div[@class='gsc_rsb_m_a']") %>% html_text()
  if(!identical(available, character(0))){
    available <- as.numeric(str_split(available," ")[[1]][1])
  }else{
    available <- NA
  }
  not_available <- page %>% html_nodes(xpath = "//*/div[@class='gsc_rsb_m_na']") %>% html_text()
  if(!identical(not_available, character(0))){
    not_available <- as.numeric(str_split(not_available," ")[[1]][1])  
  }else{
    not_available <- NA
  }

  return(list(id = id,
              name = name,
              affiliation = affiliation, 
              total_cites = as.numeric(as.character(stats[rows - 2,2])),
              h_index = as.numeric(as.character(stats[rows - 1, 2])),
              i10_index = as.numeric(as.character(stats[rows, 2])),
              fields = specs,
              homepage = homepage,
              coauthors = coauthors$coauthors,
              available = available,
              not_available = not_available))
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
get_citation_history <- function(id) {
    site <- getOption("scholar_site")
    url_template <- paste0(site, "/citations?hl=en&user=%s&pagesize=100&view_op=list_works")
    url <- compose_url(id, url_template)

    ## A better way would actually be to read out the plot of citations
    page <- get_scholar_resp(url)
    if (is.null(page)) return(page)

    page <- page %>% read_html()
    years <- page %>% html_nodes(xpath="//*/span[@class='gsc_g_t']") %>%
        html_text() %>% as.numeric()
    vals <- page %>% html_nodes(xpath="//*/span[@class='gsc_g_al']") %>%
        html_text() %>% as.numeric()
    if(length(years)>length(vals)){
      # Some years don't have citations.
      # We need to match the citation counts and years
      # <a href="javascript:void(0)" class="gsc_g_a" style="left:8px;height:5px;z-index:9">\n  <span class="gsc_g_al">2</span>\n</a>
      style_tags=page %>% html_nodes(css = '.gsc_g_a') %>%
        html_attr('style')
      # these z indices seem to be the indices starting with the last year
      zindices=as.integer(stringr::str_match(style_tags, 'z-index:([0-9]+)')[,2])
      # empty vector of 0s
      allvals=integer(length=length(years))
      # fill in
      allvals[zindices]=vals
      # and then reverse
      vals=rev(allvals)
    }
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
##' 201-202. \doi{10.1038/489201a}.
##'
##' @param id 	a character string giving a Google Scholar ID
##' @param journals a character vector giving the names of the top
##' journals.  Defaults to Nature, Science, Nature Neuroscience,
##' Proceedings of the National Academy of Sciences, and Neuron.
##' @export
get_num_top_journals <- function(id, journals) {
  papers <- get_publications(id)

  if (missing(journals)) {
    journals <-c("Nature", "Science", "Nature Neuroscience",
                 "Proceedings of the National Academy of Sciences", "Neuron")
  }

  return(length(which(is.element(papers$journal, journals))))
}


##' Get author order.
##'
##' Get author rank in authors list.
##'
##' @examples
##' library(scholar)
##'
##' id <- "bg0BZ-QAAAAJ&hl"
##'
##' authorlist <- scholar::get_publications(id)$author
##' author <- scholar::get_profile(id)$name
##'
##' author_position(authorlist, author)
##'
##' @param authorlist list of publication authors
##' @param author author's name to look for
##'
##' @return dataframe with author's position and normalized position (a normalized index, with 0 corresponding, 1 to last and 0.5 to the middle. Note that single authorship will be considered as last, i.e., 1).
##'
##'
##' @export
##' @importFrom utils tail
##' @author Dominique Makowski
author_position <- function(authorlist, author){
  author <- sapply(strsplit(author, " "), tail, 1)
  authors <- strsplit(as.character(authorlist), ", ")

  positions <- c()
  percentages <- c()
  n <- c()
  for(publication in authors){
    names <- sapply(strsplit(publication, " "), tail, 1)
    position <- grep(author, names, ignore.case = TRUE)
    current_n <- length(names)

    # Catch when not in list
    if(length(position) != 1){
      position <- NA
      percentage <- NA
    }

    # Catch unknown number of authors
    if("..." %in% names){
      percentage <- NA
      current_n <- NA
    }

    # Compute position percentage
    if(!is.na(position)){
      if(!is.na(current_n)){
        if(current_n == 1){
          percentage <- 1
        } else{
          percentage <- (position-1)/(current_n-1)
        }
      } else{
        pectentage <- NA
      }
    } else{
      pectentage <- NA
    }


    positions <- c(positions, position)
    percentages <- c(percentages, percentage)
    n <- c(n, current_n)
  }

  order <- data.frame(Authors = authorlist,
                      Position = positions,
                      n_Authors = n,
                      Position_Normalized = percentages)
  return(order)

}

#' Search for Google Scholar ID by name and affiliation
#'
#' @param last_name Researcher last name.
#' @param first_name Researcher first name.
#' @param affiliation Researcher affiliation.
#'
#' @return Google Scholar ID as a character string.
#' @export
#' @importFrom httr content
#'
#' @examples
#' get_scholar_id(first_name = "kristopher", last_name = "mcneill")
#' \donttest{
#' get_scholar_id(first_name = "michael", last_name = "sander", affiliation = NA)
#' get_scholar_id(first_name = "michael", last_name = "sander", affiliation = "eth")
#' get_scholar_id(first_name = "michael", last_name = "sander", affiliation = "ETH Zurich")
#' get_scholar_id(first_name = "michael", last_name = "sander", affiliation = "Mines")
#' get_scholar_id(first_name = "james", last_name = "babler")
#' }
get_scholar_id <- function(last_name="", first_name="", affiliation = NA) {
  if(!any(nzchar(c(first_name, last_name))))
    stop("At least one of first and last name must be specified!")

  site <- getOption("scholar_site")
  url <- paste0(
      site,
      '/citations?view_op=search_authors&mauthors=',
      first_name,
      '+',
      last_name,
      '&hl=en&oi=ao'
  )
  page <- get_scholar_resp(url)
  if (is.null(page)) return(NA)

  aa <- content(page, as='text')
  ids <- stringr::str_extract_all(
      string = aa, 
      pattern = ";user=[0-9a-zA-Z_\\-]+"
      )
    #stringr::str_extract_all(string = aa, pattern = ";user=[[:alnum:]]+[[:punct:]]")
  # maybe pattern = ";user=[^&|\"]+[&|\"]") is more safe
  # see also https://github.com/jkeirstead/scholar/issues/111


  if (length(unlist(ids)) == 0) {
    message("No Scholar ID found.")
    return(NA)
  }
  
  ids <- ids %>%
    unlist %>%
    gsub(";user=|[[:punct:]]$", "", .) %>%
    unique
  
  if (length(ids) > 1) {
    profiles <- lapply(ids, scholar::get_profile)
    if (is.na(affiliation)) {
      x_profile <- profiles[[1]]
      warning("Selecting first out of ", length(profiles), " candidate matches.")
    } else {
      which_profile <- sapply(profiles, function(x) {
        stringr::str_count(
          string = x$affiliation,
          pattern = stringr::coll(affiliation, ignore_case = TRUE)
        )
      })
      if(all(which_profile == 0)){
        warning("No researcher found at the indicated affiliation.")
        return(NA)
      } else {
        x_profile <- profiles[[which(which_profile != 0)[1]]]
      }
    }
  } else {
    x_profile <- scholar::get_profile(id = ids)
  }
  return(x_profile$id)
}

