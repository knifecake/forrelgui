#' Get a list of marker names from a pedigree
#'
#' @param x a \code{\link[pedtools]{ped}} object or a list of such
#'
#' @return a character vector containing the names of the markers attached to
#'   \code{x}. If \code{x} is a list of pedigrees, only the markers attached to
#'   the first component are used.
#' @export
get_marker_names <- function(x) {
  if (pedtools::is.pedList(x)) {
    x = x[[1]]
  }

  unlist(lapply(x$MARKERS, pedtools::name))
}
