##' Gets the publications for a scholar
##'
##' Gets the publications of a scholar.

##' @param id a character string specifying the Google Scholar ID.  If
##' multiple IDs are specified, only the publications of the first
##' scholar will be retrieved.
##' @param cstart an integer specifying the first article to start counting.
##' this should not be changed if expecting to get all article. Needed only for recursive search.
##' in Google Scholar's result page. 
##' @return a data frame listing the publications and their details
##' @import stringr plyr R.cache XML
##' @export
get_publications <- function(id, cstart = 0) {

  ## Ensure we're only getting one scholar's publications
  id <- tidy_id(id)

  ## Check if we've cached it already
  setCacheRootPath(tempdir())
  data <- loadCache(list(id))

  ## If not, get the data and save it to cache
  if (is.null(data)) {
  
    ## Build the URL
    url_template <- "http://scholar.google.com/citations?hl=en&user=%s&pagesize=100&view_op=list_works&cstart=%d"
    url <- sprintf(url_template, id, cstart)

    ## Load the page
    doc <- htmlParse(url, encoding="UTF-8")
    cites <- xpathApply(doc, '//tr[@class="cit-table item"]')

    ## Works on a list element
    parse_cites <- function(l) {
      ## Basic info
      td <- l[[1]]
      title <- xmlValue(td[[1]])
      pubid  <- str_split(xmlAttrs(td[[1]])[[1]],":")[[1]][2]
      author <- xmlValue(td[[3]])
      
      ## Citation info
      ## NA citations mean 0 citations
      cited_by <- ifelse(is.na(as.numeric(xmlValue(l[[2]][[1]]))), 0, as.numeric(xmlValue(l[[2]][[1]])))
      year <- as.numeric(xmlValue(l[[4]]))
      
      ## Parse the source information
      src <- xmlValue(td[[5]])
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
      numbers <- str_trim(str_sub(src, first_digit+1, str_length(src)))
      
      return(data.frame(title=title, author=author, journal=journals, number=numbers, cites=cited_by, year=year, pubid=pubid))
    }

    tmp <- lapply(cites, parse_cites)
    data <- ldply(tmp)

    ## Not all the characters UTF-8 are correctly capture (for some reason). 
    ## Those here are the ones I've found so far...
    data <- as.data.frame(lapply(data,function(x) if(is.character(x)|is.factor(x)) gsub("\xc1","Á",x) else x))
    data <- as.data.frame(lapply(data,function(x) if(is.character(x)|is.factor(x)) gsub(" ","-",x) else x))
    data <- as.data.frame(lapply(data,function(x) if(is.character(x)|is.factor(x)) gsub("\u0096","-",x) else x))

    ## Check if we've reached a multiple of 100 articles. Might need to search the next page
    if (nrow(data) > 0 && nrow(data)%%100 == 0) {
      data <- rbind(data, get_publications(id, nrow(data)))
    }
    ## Save it after everything has been retrieved. (no recursivity left)
    if (cstart == 0) {
      # saveCache(data, key=list(id))
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


