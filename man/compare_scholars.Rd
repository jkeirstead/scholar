\name{compare_scholars}
\alias{compare_scholars}
\title{Compare the citation records of multiple scholars}
\usage{
  compare_scholars(ids)
}
\arguments{
  \item{ids}{a vector of Google Scholar IDs}
}
\value{
  a data frame giving the ID of each scholar and the total
  number of citations received by work published in a year.
}
\description{
  Compares the citation records of multiple scholars.  This
  function compiles a data frame comparing the citations
  received by each of the scholar's publications by year of
  publication (currently limited to just the top 100).
}
\examples{
{
    ## How do Richard Feynmann and Stephen Hawking compare?
	ids <- c("B7vSqZsAAAAJ", "qj74uXkAAAAJ")
    df <- compare_scholars(ids)
}
}

