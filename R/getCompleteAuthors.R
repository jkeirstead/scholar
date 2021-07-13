##' Get the Complete list of authors for a Publication
##'
##' Found as Muhammad Qasim Pasta's solution here https://github.com/jkeirstead/scholar/issues/21
##' @author Muhammad Qasim Pasta
##' @author Abram B. Fleishman
##' @author James H. Conigrave
##'
##' @param id a Google Scholar ID
##' @param pubid a Publication ID from a given google Scholar ID
##' @param delay average delay between requests. A delay is needed to stop Google identifying you as a bot
##' @param initials if TRUE (default), first and middle names will be abbreviated
##' @return a string containing the complete list of authors
##' @export

get_complete_authors = function(id, pubid, delay = .4, initials = TRUE) {
  get_author = function(id, pubid) {
      auths = ""
      site <- getOption("scholar_site")
      url_template = paste0(site, "/citations?view_op=view_citation&citation_for_view=%s:%s")
      url = sprintf(url_template, id, pubid)

      page <- get_scholar_resp(url[1])
      if (is.null(page)) return(NA)

      url1 <- page %>%
          read_html

      auths = as.character(rvest::html_node(url1, ".gsc_oci_value") %>% rvest::html_text())

      return(auths)
  }

  if (length(pubid) == 1) {
    auths <- get_author(id, pubid)
  } else{

    if(length(pubid) > 50){
      stop("Requesting author lists for more than 50 publications risks google identifying you as a bot and blocking your ip range (429 errors).")
    }

    min_delay = delay - .5
    max_delay = delay + .5
    if(min_delay < 0) min_delay <- 0
    if(delay == 0) max_delay <- 0

    pb = utils::txtProgressBar(min = 1, max = length(pubid), style = 3)
    i = 1

    auths <- sapply(pubid, function(x) {
      delay <- sample(seq(min_delay, max_delay, by = .001), 1)
      Sys.sleep(delay)
      auth <- get_author(id, x)
      i <<- i + 1
      utils::setTxtProgressBar(pb, i)

      return(auth)
    })
    close(pb)
  }

  if (initials) {
    auths = sapply(auths, format_authors)
  }

  auths

}

##' format_authors
##'
##' This function convers first and middle names to initials
##' @param string a character vector of names

format_authors = function(string)
  {
  authors = trimws(unlist(strsplit(string, ",")))

  format_author = function(author){

    words = trimws(unlist(strsplit(author, " ")))
    lastname = words[length(words)]
    first = words[!words %in% lastname]
    first = gsub("\\B[a-z]","",first, perl = TRUE) #select characters not at the start of a word
    first = paste(first, collapse = "")
    trimws(paste(first, lastname))

  }

  authors = sapply(authors, format_author)
  paste(authors, collapse = ", ")

}

