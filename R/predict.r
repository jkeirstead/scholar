##' Predicts the h-index for a researcher
##'
##' Predicts the h-index for a research one year, five years, and ten
##' years ahead using Acuna et al's method (see source).  The model
##' was fit to data from neuroscience researchers with an h-index
##' greater than 5 and between 5 to 12 years since publishing their
##' first article.  So naturally, if this isn't you, then the results
##' should be taken with a large pinch of salt.  For more caveats, see
##' \url{http://simplystatistics.org/2012/10/10/whats-wrong-with-the-predicting-h-index-paper/}.
##'
##' @note A scientist has an h-index of n if he or she publishes n
##' papers with at least n citations each.  Values returned are
##' fractional so it's up to your own vanity whether you want to round
##' up or down.  
##'
##' @source DE Acuna, S Allesina, KP Kording (2012) Future impact:
##' Predicting scientific success.  Nature 489,
##' 201-202. \url{http://dx.doi.org/10.1038/489201a}.
##' @param id a character string giving the Google Scholar ID
##' @param journals optional character vector of top
##' journals. See \code{\link{get_num_top_journals}} for more details.
##' @return a data frame giving predicted h-index values in future
##' @export
##' @examples {
##'    ## Predict h-index of original method author
##'   id <- "GAi23ssAAAAJ"
##'   df <- predict_h_index(id)
##' }
predict_h_index <- function(id, journals) {
  id <- tidy_id(id)
  n <- get_num_articles(id) # number of articles written
  h <- get_profile(id)$h_index
  y <- as.numeric(format(Sys.Date(), "%Y")) - get_oldest_article(id)
  j <- get_num_distinct_journals(id)

  if (missing(journals)) {
    q <- get_num_top_journals(id)
  } else {
    q <- get_num_top_journals(id, journals)
  }

  h1 <- 0.76 + 0.37*sqrt(n) + 0.97*h - 0.07*y + 0.02*j + 0.03*q
  h5 <- 4 + 1.58*sqrt(n) + 0.86*h - 0.35*y + 0.06*j + 0.2*q
  h10 <- 8.73 + 1.33*sqrt(n) + 0.48*h - 0.41*y + 0.52*j + 0.82*q
  return(data.frame(years_ahead=c(0,1,5,10),
                    h_index=c(h, h1, h5, h10)))
}
  
