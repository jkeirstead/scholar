# Ugly hack for CRAN checks
utils::globalVariables(c("name"))

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
##' citations, impact metrics, fields of study, homepage and
##' the author's list of coauthors provided by Google Scholar.
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
get_profile <- function(id) {

  url_template <- "http://scholar.google.com/citations?hl=en&user=%s"
  url <- compose_url(id, url_template)

  ## Generate a list of all the tables identified by the scholar ID
  page <- get_resp(url) %>% read_html()
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
  
  ## Grab all coauthors
  coauthors <- list_coauthors(id, n_coauthors = 20) # maximum availabe in profile

  return(list(id=id, name=name, affiliation=affiliation,
              total_cites=as.numeric(as.character(stats[rows-2,2])),
              h_index=as.numeric(as.character(stats[rows-1,2])),
              i10_index=as.numeric(as.character(stats[rows,2])),
              fields=specs,
              homepage=homepage,
              coauthors=coauthors$coauthors))
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

    url_template <- "http://scholar.google.com/citations?hl=en&user=%s&pagesize=100&view_op=list_works"
    url <- compose_url(id, url_template)

    ## A better way would actually be to read out the plot of citations
    page <- get_resp(url) %>% read_html()
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

#' Gets the network of coauthors of a scholar
#'
##' @param id a character string specifying the Google Scholar ID.
##' If multiple ids are specified, only the first value is used and a
##' warning is generated.
#' @param n_coauthors Number of coauthors to explore. This number should usually be between 1 and 10 as
#' choosing many coauthors can make the network graph too messy.
#' @param n_deep The number of degrees that you want to go down the network. When \code{n_deep} is equal to \code{1}
#' then \code{grab_coauthor} will only grab the coauthors of Joe and Mary, so Joe -- > Mary --> All coauthors. This can get
#' out of control very quickly if \code{n_deep} is set to \code{2} or above. The preferred number is \code{1}, the default.
#' 
#' @details Considering that scraping each publication for all coauthors is error prone, \code{get_coauthors}
#' grabs only the coauthors listed on the google scholar profile (on the bottom right of the profile),
#' not from all publications.
#'
#' @return A data frame with two columns showing all authors and coauthors.
#' 
#' @seealso \code{\link{plot_coauthors}}
#' @export
#'
#' @examples
#'
#' \dontrun{
#' 
#' library(scholar)
#' coauthor_network <- get_coauthors('amYIKXQAAAAJ&hl')
#' plot_coauthors(coauthor_network)
#' }
#'
#'
get_coauthors <- function(id, n_coauthors = 5, n_deep = 1) {
  stopifnot(is.numeric(n_deep), length(n_deep) >= 1, n_deep != 0)
  stopifnot(is.numeric(n_coauthors), length(n_coauthors) >= 1, n_coauthors != 0)
  
  all_coauthors <- list_coauthors(id, n_coauthors)
  
  empty_network <- replicate(n_deep, list())
  
  # Here I grab the id of the coauthors url because list_coauthors
  # needs to accept ids and not urls
  for (i in seq_len(n_deep)) {
    if (i == 1)  {
      empty_network[[i]] <- clean_network(grab_id(all_coauthors$coauthors_url),
                                          n_coauthors)
    } else {
      empty_network[[i]] <- clean_network(grab_id(empty_network[[i - 1]]$coauthors_url),
                                          n_coauthors)
    }
  }
  
  final_network <- rbind(all_coauthors, Reduce(rbind, empty_network))
  final_network$author <- stringr::str_to_title(final_network$author)
  final_network$coauthors <- stringr::str_to_title(final_network$coauthors)
  final_network[c("author", "coauthors")]
}


#' Plot a network of coauthors
#'
#' @param network A data frame given by \code{\link{get_coauthors}}
#' @param size_labels Size of the label names
#'
#' @return a \code{ggplot2} object but prints a plot as a side effect.
#' @export
#' @importFrom dplyr "%>%"
#' 
#' @seealso \code{\link{get_coauthors}}
#' 
#' @examples
#' \dontrun{
#' library(scholar)
#' coauthor_network <- get_coauthors('amYIKXQAAAAJ&hl')
#' plot_coauthors(coauthor_network)
#' }
plot_coauthors <- function(network, size_labels = 5) {
  graph <- tidygraph::as_tbl_graph(network) %>%
    mutate(closeness = suppressWarnings(tidygraph::centrality_closeness())) %>% 
    filter(name != "")
  # to delete authors who have **no coauthors**, that is ""

  ggraph::ggraph(graph, layout = 'kk') +
    ggraph::geom_edge_link(ggplot2::aes_string(alpha = 1/2, color = as.character('from')), alpha = 1/3, show.legend = FALSE) +
    ggraph::geom_node_point(ggplot2::aes_string(size = 'closeness'), alpha = 1/2, show.legend = FALSE) +
    ggraph::geom_node_text(ggplot2::aes_string(label = 'name'), size = size_labels, repel = TRUE, check_overlap = TRUE) +
    ggplot2::labs(title = paste0("Network of coauthorship of ", network$author[1])) +
    ggraph::theme_graph(title_size = 16, base_family = "sans")
}
