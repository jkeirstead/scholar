#' scholar
#'
#' The \code{scholar} package provides functions to extract citation data from
#' Google Scholar.  There are also convenience functions for comparing multiple
#' scholars and predicting h-index scores based on past publication records.
#'
#' @section Google Scholar cookies: Google does not encourage people to use the
#'   scholar website as an API. You will find a number of issues related to this
#'   if you follow:
#'   \url{https://github.com/jkeirstead/scholar/issues?q=is:issue+cookie}
#'
#'   If you try to fetch too many pages, you can expect to see your access to
#'   scholar blocked. One step that they do take is to provide a session cookie.
#'   Reusing these seems to help. If you wish to write your own code to interact
#'   with Google Scholar, we recommend that you use the
#'   \code{\link{get_scholar_resp}} function to carry out the query.
#'
#' @note A complementary set of Google Scholar functions can be found at
#'   \url{https://www.biostat.jhsph.edu/~jleek/code/googleCite.r}.  The
#'   \code{scholar} package was developed independently.
#'
#' @source The package reads data from \url{https://scholar.google.com}.  Dates
#'   and citation counts are estimated and are determined automatically by a
#'   computer program. Use at your own risk.
#'
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
