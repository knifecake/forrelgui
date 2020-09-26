#' File chooser UI component
#'
#' @param label text in the label
#' @param text text inside the button
#' @param container where to put this button
#' @param handler what to do when the chosen file changes
#' @param ... further parameters passed to gfile
#'
#' @return a gWidgets2::ggroup object.
#'
file_chooser <- function(label = "", text = "", container = NULL, handler = print, ...) {
  chooser <- gWidgets2::ggroup(container = container)

  g_label <- gWidgets2::glabel(label, container = chooser, width = 100)
  gWidgets2::size(g_label) <- list(width = 30)

  gWidgets2::gbutton(text, handler = function(h) {
    filename <- gWidgets2::gfile(label, ...)
    
    # only call the handler if the user actually chose a file
    if (nchar(filename) > 0)
      handler(filename)
  }, container = chooser)

  chooser
}
