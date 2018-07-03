.onLoad <- function(libname, pkgname) {
  options("scholar_call_home"=TRUE)
}

# Recursively try to GET Google Scholar Page
get_resp <- function(url, attempts_left = 5) {
  
  stopifnot(attempts_left > 0)
  
  resp <- httr::GET(url, handle = getOption("scholar_handle"))
  
  # On a successful GET, return the response
  if (httr::status_code(resp) == 200) {
    resp
  } else if (attempts_left == 1) { # When attempts run out, stop with an error
    stop("Cannot connect to Google Scholar. Is the ID you provided correct?")
  } else { # Otherwise, sleep a second and try again
    Sys.sleep(1)
    get_resp(url, attempts_left - 1)
  }
}

## We can use this function through the package to compose
## a url by only providing the id
compose_url <- function(id, url_template) {
  if (is.na(id)) return(NA_character_)
  id <- tidy_id(id)
  url <- sprintf(url_template, id)
  
  url
}

# Extract the google scholar id of a url
grab_id <- function(url) {
  stringr::str_extract(url, "(?<=user=)[^=]*")
}

# Extract the coauthors of an id and
# only return the names of the author and coauthors
list_coauthors <- function(id, n_coauthors) {
  url_template <- "http://scholar.google.com/citations?hl=en&user=%s"
  url <- compose_url(id, url_template)
  
  if (id == "" | is.na(id)) {
    return(
      data.frame(author = character(),
                 author_href = character(),
                 coauthors = character(),
                 coauthors_url = character()
      )
    )
  }
  
  resp <- get_resp(url, 5)
  
  google_scholar <- httr::content(resp)
  
  author_name <-
    xml2::xml_text(
      xml2::xml_find_all(google_scholar,
                         xpath = "//div[@id = 'gsc_prf_in']")
    )
  
  # Do no grab the text of the node yet because I need to grab the
  # href below.
  coauthors <- xml2::xml_find_all(google_scholar,
                                  xpath = "//a[@tabindex = '-1']")
  
  subset_coauthors <- if (n_coauthors > length(coauthors)) TRUE else seq_len(n_coauthors)
  
  coauthor_href <- xml2::xml_attr(coauthors[subset_coauthors], "href")
  
  coauthors <- xml2::xml_text(coauthors)[subset_coauthors]
  
  # If the person has no coauthors, return empty
  if (length(coauthor_href) == 0) {
    coauthors <- ""
    coauthor_href <- ""
  }
  
  coauthor_urls <- 
    vapply(
      grab_id(coauthor_href),
      compose_url,
      url_template,
      FUN.VALUE = character(1)
    )
  
  data.frame(
    author = author_name,
    author_url = url,
    coauthors = coauthors,
    coauthors_url = coauthor_urls,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# Iteratively search down the network of coauthors
clean_network <- function(network, n_coauthors) {
  Reduce(rbind, lapply(network, list_coauthors, n_coauth = n_coauthors))
}
