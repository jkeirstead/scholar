\name{get_publications}
\alias{get_publications}
\title{Gets the publications for a scholar}
\usage{
get_publications(id)
}
\arguments{
  \item{id}{a character string specifying the Google
  Scholar ID.  If multiple IDs are specified, only the
  publications of the first scholar will be retrieved.}
}
\value{
a data frame listing the top 100 publications and their
details
}
\description{
Gets the publications of a scholar.  The method is
currently limited to the top 100 publications only.
}

