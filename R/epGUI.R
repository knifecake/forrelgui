#' Exclusion power GUI
#'
#' Launches a graphical user interface for the exclusion power functionality
#' included in the forrel package.
#'
#' @export
#'
epGUI <- function(save_file = NULL) {
  # gWidgets2::guiToolkit('tcltk')
  options('guiToolkit' = 'tcltk')

  mk_env()
  
  # if we were called with a save_file, load it
  if (isTruthy(save_file)) load_project_file(save_file)

  # create the GUI
  main_view()
}

mk_env <- function() {
  gui  <<- new.env(parent = globalenv())
  model <<- new.env(parent = globalenv())
  
  model <- empty_model()
  return()
}

load_project_file <- function(path) {
  tryCatch({
    load(path, envir = globalenv())
  }, error = function(e) {
    gWidgets2::gmessage('It looks like the provided file was not an epGUI project file.',
                        title = 'Could not load project',
                        icon = 'error', parent = gui$main_window)
  })
}
