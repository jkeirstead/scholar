\name{predict_h_index}
\alias{predict_h_index}
\title{Predicts the h-index for a researcher}
\source{
  DE Acuna, S Allesina, KP Kording (2012) Future impact:
  Predicting scientific success.  Nature 489, 201-202.
  \link{http://dx.doi.org/10.1038/489201a}.
}
\usage{
  predict_h_index(id, journals)
}
\arguments{
  \item{id}{a character string giving the Google Scholar
  ID}

  \item{journals}{optional character vector of top
  journals. See \code{\link{get_num_top_journals}} for more
  details.}
}
\value{
  a data frame giving predicted h-index values in future
}
\description{
  Predicts the h-index for a research one year, five years,
  and ten years ahead using Acuna et al's method (see
  source).  The model was fit to data from neuroscience
  researchers with an h-index greater than 5 and between 5
  to 12 years since publishing their first article.  So
  naturally, if this isn't you, then the results should be
  taken with a large pinch of salt.  For more caveats, see
  \link{http://simplystatistics.org/2012/10/10/whats-wrong-with-the-predicting-h-index-paper/}.
}
\note{
  A scientist has an h-index of n if he or she publishes n
  papers with at least n citations each.  Values returned
  are fractional so it's up to your own vanity whether you
  want to round up or down.
}
\examples{
{
   ## Predict h-index of original method author
  id <- "GAi23ssAAAAJ"
  df <- predict_h_index(id)
}
}

