##' Gets the publications for a scholar
##'
##' Gets the publications of a specified scholar.
##'
##' @param id a character string specifying the Google Scholar ID.  If
##' multiple IDs are specified, only the publications of the first
##' scholar will be retrieved.
##' @param cstart an integer specifying the first article to start
##' counting.  To get all publications for an author, omit this
##' parameter.
##' @param pagesize an integer specifying the number of articles to
##' fetch
##' @param flush should the cache be flushed?  Search results are
##' cached by default to speed up repeated queries.  If this argument
##' is TRUE, the cache will be cleared and the data reloaded from
##' Google.
##' @details Google uses two id codes to uniquely reference a
##' publication.  The results of this method includes \code{id} which
##' can be used to link to a publication's full citation history
##' (i.e. if you click on the number of citations in the main scholar
##' profile page), and \code{pubid} which links to the details of the
##' publication (i.e. if you click on the title of the publication in
##' the main scholar profile page.)
##' @return a data frame listing the publications and their details.
##' These include the publication title, author, journal, number,
##' cites, year, and two id codes (see details).
##' @import stringr plyr R.cache XML
##' @export
get_publications <- function(id, cstart = 0, pagesize=100, flush=FALSE) {

  ## Ensure we're only getting one scholar's publications
  id <- tidy_id(id)

  ## Define the cache path 
  cache.dir <- file.path(tempdir(), "r-scholar")
  setCacheRootPath(cache.dir)

  ## Clear the cache if requested
  if (flush) saveCache(NULL, key=list(id, cstart))

  ## Check if we've cached it already
  data <- loadCache(list(id, cstart))

  ## If not, get the data and save it to cache
  if (is.null(data)) {
  
    ## Build the URL
    url_template <- "http://scholar.google.com/citations?hl=en&user=%s&cstart=%d&pagesize=%d"
    url <- sprintf(url_template, id, cstart, pagesize)

    ## Load the page
    doc <- htmlParse(url, encoding="UTF-8")
    cites <- xpathApply(doc, '//tr[@class="gsc_a_tr"]')

    ## Works on a list element
    parse_cites <- function(l) {
      ## Basic info
      td <- l[[1]]
      title <- xmlValue(td[[1]])
      pubid  <- str_split(xmlAttrs(td[[1]])[[1]],":")[[1]][2]
      author <- xmlValue(td[[2]])
      year <- as.numeric(xmlValue(l[[3]]))
      
      ## Citation info
      src <- l[[2]][[1]]
      if(!is.null(src)){
        cited_by <- suppressWarnings(as.numeric(xmlValue(src)))
        ## NA citations mean 0 citations
        cited_by <- ifelse(is.na(cited_by), 0, cited_by)
        s <- xmlAttrs(src)[[1]]
        doc_id <- strsplit(s, "cites=")[[1]][2]
      } else {
        cited_by <- 0
        doc_id <- ""
      }

      ## Parse the source information
      src <- xmlValue(td[[3]])
      ## Find the first digit (hopefully not in the journal title)
      first_digit <- as.numeric(regexpr("[\\[\\(]?\\d", src)) - 1
      ids <- which(first_digit<0)
      first_digit <- replace(first_digit, ids, str_length(src)[ids])

      ## Clean up the journal part
      journals <- str_trim(str_sub(src, 1, first_digit))
      trailing_commas <- as.numeric(regexpr(",$", journals)) - 1
      ids <- which(trailing_commas<0)
      trailing_commas <- replace(trailing_commas, ids, str_length(journals)[ids])
      journals <- str_sub(journals, 1, trailing_commas)

      ## Clean up the number part
      numbers <- str_trim(str_sub(src, first_digit+1, str_length(src)-6))

      return(data.frame(title=title, author=author, journal=journals, number=numbers, cites=cited_by, year=year, id=doc_id, pubid=pubid))

    }

    tmp <- lapply(cites, parse_cites)
    data <- ldply(tmp)

    ## Check if we've reached pagesize articles. Might need
    ## to search the next page
    if (nrow(data) > 0 && nrow(data)==pagesize) {
      data <- rbind(data, get_publications(id, cstart=cstart+pagesize, pagesize=pagesize))
    }
    
    ## Save it after everything has been retrieved.
    if (cstart == 0) {
      saveCache(data, key=list(id, cstart))
    }
  }
  
  return(data)
}

##' Gets the citation history of a single article
##'
##' @param id a character string giving the id of the scholar
##' @param article a character string giving the article id.
##' @return a data frame giving the year, citations per year, and
##' publication id
##' @import XML stringr
##' @export
get_article_cite_history <- function (id, article) {
    id <- tidy_id(id)
    url_base <- paste0("http://scholar.google.com/citations?", 
                       "view_op=view_citation&hl=en&citation_for_view=")
    url_tail <- paste(id, article, sep=":")
    url <- paste0(url_base, url_tail)
    doc <- htmlTreeParse(url, useInternalNodes = TRUE)

    ## Inspect the bar chart to retrieve the citation values and years
    years <- xpathSApply(doc, "//*/div[@id='gsc_graph_bars']/a",
                              xmlGetAttr, 'href')
    years <- as.numeric(str_replace(years, ".*as_yhi=(.*)$", "\\1"))
    
    vals <- as.numeric(xpathSApply(doc, "//*/span[@class='gsc_g_al']", 
                                   xmlValue))
    df <- data.frame(year = years, cites = vals)

    ## There may be undefined years in the sequence so fill in these gaps
    tmp <- merge(data.frame(year=min(years):max(years)),
                 df, all.x=TRUE)
    tmp[is.na(tmp)] <- 0

    tmp$pubid <- article
    return(tmp)
}

##' Calculates how many articles a scholar has published
##'
##' Calculate how many articles a scholar has published.
##'
##' @param id a character string giving the Google Scholar ID
##' @return an integer value (max 100)
##' @export
get_num_articles <- function(id) {  
  papers <- get_publications(id)
  return(nrow(papers))
}

##' Gets the year of the oldest article for a scholar
##'
##' Gets the year of the oldest article published by a given scholar.
##' 
##' @param id 	a character string giving the Google Scholar ID
##' @return the year of the oldest article
##' @export
get_oldest_article <- function(id) {
  papers <- get_publications(id)
  return(min(papers$year, na.rm=TRUE))
}


