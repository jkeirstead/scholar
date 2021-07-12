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
    
    stopifnot(is.numeric(n_coauthors), length(n_coauthors) >= 1, n_coauthors != 0)
    
    all_coauthors <- list_coauthors(id, n_coauthors)
    
    # remove false entries containing "Sort by *" 
    all_coauthors <- all_coauthors[setdiff(1:nrow(all_coauthors),
                                           grep("Sort by ", all_coauthors$coauthors)),] 
    
    empty_network <- replicate(n_deep, list())
    
    # Here I grab the id of the coauthors url because list_coauthors
    # needs to accept ids and not urls
    if(n_deep == 0){            
        empty_network[[1]] <- clean_network(grab_id(all_coauthors$coauthors_url),
                                                                    25) # grab up to 25 coauthors of coauthors
        }else{
            for (i in seq_len(n_deep)) {
                if (i == 1)  {
                    empty_network[[i]] <- clean_network(grab_id(all_coauthors$coauthors_url),
                                                        n_coauthors)
                } else {
                    empty_network[[i]] <- clean_network(grab_id(empty_network[[i - 1]]$coauthors_url),
                                                        n_coauthors)
                }
            }
        }
    final_network <- rbind(all_coauthors, Reduce(rbind, empty_network))
    final_network <- final_network[setdiff(1:nrow(final_network),
            grep("Sort by ", final_network$coauthors)),]  # remove false entries containing "Sort by *" 
    final_network$author <- stringr::str_to_title(final_network$author)
    final_network$coauthors <- stringr::str_to_title(final_network$coauthors)
    if(n_deep == 0) {
        final_network <- final_network[final_network$coauthors %in% final_network$author,]
    }
    res <- final_network[c("author", "coauthors")]
    res <- res[!res$coauthors %in% c("Sort By Year", "Sort By Title", "Sort By Citations"), ]
    return(res)
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


# Extract the coauthors of an id and
# only return the names of the author and coauthors
list_coauthors <- function(id, n_coauthors) {
    site <- getOption("scholar_site")
    url_template <- paste0(site, "/citations?hl=en&user=%s")
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
    
    resp <- get_scholar_resp(url, 5)
    
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
