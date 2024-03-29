% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/publications.r
\name{get_publication_url}
\alias{get_publication_url}
\title{Gets the PDF URL for a publication id.}
\usage{
get_publication_url(id, pub_id, flush = FALSE)
}
\arguments{
\item{id}{a character string specifying the Google Scholar ID.}

\item{pub_id}{a character string specifying the publication id.}

\item{flush}{Whether or not to clear the cache}
}
\value{
a String that contains the URL to the PDF of the publication.
}
\description{
Gets the PDF URL for a publication id.
}
