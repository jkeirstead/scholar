##' Format publication list
##' 
##'
##' @title format_publications
##' @param scholar.profile scholar profile ID
##' @param author.name name of author to be highlighted using bold font
##' @examples
##' \dontrun{
##'  library(scholar)
##'  format_publications("DO5oG40AAAAJ")    
##' }
##' @return a vector of formated publications
##' @importFrom rlang .data
##' @export
##' @author R Thériault and modified by Guangchuang Yu
format_publications <- function(scholar.profile, author.name = NULL) {
  pubs <- get_publications(scholar.profile)
  pubs2 <- pubs %>% 
    strsplit(x = .$author, split = ",") 
  
  pubs$author <- lapply(pubs2, function(x) {
    x <- swap_initials(x)
    x[length(x)] <- paste0("& ", x[length(x)])
    x <- paste0(x, collapse = ", ")
    ifelse(startsWith(x, "& "), sub("& ", "", x), x)
    })

  author.name2 <- swap_initials(author.name)
  
  res <- pubs %>% 
    arrange(desc(.data$year)) %>%
    mutate(journal = paste0("*", .data$journal, "*"),
           Publications = paste0(.data$author, " (", .data$year, "). ", 
                                 .data$title, ". ", .data$journal, ". ", 
                                 .data$number)
    ) %>% 
    pull(.data$Publications)

    if (is.null(author.name2)) return(res)
    gsub(author.name2, paste0("**", author.name2, "**"), res)
}


swap_initials <- function(author.name) {
    if (is.null(author.name)) return (NULL)
    sub("(.*) (.*)", "\\2, \\1.", trimws(author.name))
}

