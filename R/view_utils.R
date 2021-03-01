df_modal_view <- function(df, title = "Preview") {
  if (!is.data.frame(df)) return()
  
  # round frequencies for better display
  df <- df_round(df, digits = 3)
  
  # substitute NA values for a string, since gtable does not understand them
  df <- df_nas_to_string(df)
  
  # add row names as the first column
  df <- cbind(Name = rownames(df), data.frame(df, row.names=NULL))
  
  w <- gWidgets2::gwindow(title = title,
                          width = 700,
                          height = 400,
                          visible = FALSE,
                          parent = gui$main_window)
  t <- gWidgets2::gtable(df, container = w)
  
  gWidgets2::size(t) <- list(
    column.widths = sapply(colnames(df), function(item) { min(100, max(50, 30 * nchar(item))) })
  )
  
  gWidgets2::visible(w) <- TRUE
}

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
    if (isTruthy(filename))
      handler(filename)
  }, container = chooser)
  
  chooser
}
