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
get_publications <- function(id, cstart = 0) {

  ## Ensure we're only getting one scholar's publications
  id <- tidy_id(id)

  ## Check if we've cached it already
  setCacheRootPath(tempdir())
  data <- loadCache(list(id, cstart))

  ## If not, get the data and save it to cache
  if (is.null(data)) {
  
    ## Build the URL
    url_template <- "http://scholar.google.com/citations?hl=en&user=%s&cstart=%d"
    url <- sprintf(url_template, id, cstart)

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

    ## @jaumebonet reports that not all the UTF-8 characters are
    ## captured correctly.  I haven't been able to reproduce this on
    ## my (Windows) machine, so have commented this out for now.
    ##    data <- as.data.frame(lapply(data,function(x) if(is.character(x)|is.factor(x)) gsub("\xc1","Á",x) else x))
    ##    data <- as.data.frame(lapply(data,function(x) if(is.character(x)|is.factor(x)) gsub(" ","-",x) else x))
    ##    data <- as.data.frame(lapply(data,function(x) if(is.character(x)|is.factor(x)) gsub("\u0096","-",x) else x))

    ## Check if we've reached a multiple of 100 articles. Might need
    ## to search the next page
    if (nrow(data) > 0 && nrow(data)%%100 == 0) {
      data <- rbind(data, get_publications(id, nrow(data)))
    }
    
    ## Save it after everything has been retrieved.
    if (cstart == 0) {
      saveCache(data, key=list(id, cstart))
    }
  }
  
  return(data)
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


