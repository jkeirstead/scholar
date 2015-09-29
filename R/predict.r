##' Predicts the h-index for a researcher
##'
##' Predicts the h-index for a researcher each year for ten years into
##' the future using Acuna et al's method (see source).  The model was
##' fit to data from neuroscience researchers with an h-index greater
##' than 5 and between 5 to 12 years since publishing their first
##' article.  So naturally if this isn't you, then the results should
##' be taken with a large pinch of salt.  For more caveats, see
##' \url{http://simplystatistics.org/2012/10/10/whats-wrong-with-the-predicting-h-index-paper/}.
##'
##' @note A scientist has an h-index of n if he or she publishes n
##' papers with at least n citations each.  Values returned are
##' fractional so it's up to your own vanity whether you want to round
##' up or down.  
##'
##' @source DE Acuna, S Allesina, KP Kording (2012) Future impact:
##' Predicting scientific success.  Nature 489,
##' 201-202. \url{http://dx.doi.org/10.1038/489201a}.  Thanks to DE
##' Acuna for providing the full regression coefficients for each year
##' ahead prediction.
##' @param id a character string giving the Google Scholar ID
##' @param journals optional character vector of top
##' journals. See \code{\link{get_num_top_journals}} for more details.
##' @return a data frame giving predicted h-index values in future
##' @details Since the model is calibrated to neuroscience
##' researchers, it is entirely possible that very strange
##' (e.g. negative) h-indices will be predicted if you are a
##' researcher in another field.  A warning will be displayed if the
##' sequence of predicted h-indices contains a negative value or is
##' non-increasing.
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

  ## Regression coefficients, courtesy of Daniel Acuna
  coefs <- c(
             1,0.760,0.373,0.967,-0.069,0.018,0.033,
             2,1.413,0.781,0.936,-0.132,0.018,0.064,
             3,2.227,1.105,0.903,-0.193,0.027,0.096,
             4,3.196,1.386,0.871,-0.274,0.039,0.145,
             5,3.997,1.578,0.858,-0.345,0.063,0.198,
             6,4.752,1.671,0.817,-0.377,0.117,0.282,
             7,5.741,1.761,0.761,-0.420,0.170,0.394,
             8,6.531,1.796,0.669,-0.420,0.252,0.508,
             9,7.482,1.653,0.561,-0.415,0.383,0.629,
             10,8.734,1.326,0.478,-0.411,0.522,0.823)
  coefs.m <- matrix(coefs, nrow=10, byrow=TRUE)
  coefs <- coefs.m[,-1]
  vals <- c(1, sqrt(n), h, y, j, q)

  ## Calculate the h-index predictions
  h.pred <- coefs %*% vals
  h.vals <- c(h, h.pred)

  ## Check for sensible values
  standard.warning <- "You're probably not a neuroscientist.  Please read the documentation for information on the limitations of this function."
  
  if (any(diff(h.vals)<0))
      warning(paste0("Decreasing h-values predicted. ", standard.warning))

  if (any(h.vals<0))
      warning("Negative h-values predicted. ", standard.warning)
  
  return(data.frame(years_ahead=c(0:10), h_index=h.vals))

}
  
