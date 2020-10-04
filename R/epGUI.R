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
  if (!is.null(save_file)) load(save_file, envir = globalenv())

  # create the GUI
  main_view()
}

calculate_ep <- function() {
  if (is.null(model$claim_ped) || is.null(model$true_ped)) return()
  
  model$result <- NULL
  
  model$result <- forrel::exclusionPower(
    claimPed = model$claim_ped,
    truePed = model$true_ped, 
    ids = model$available,
    verbose = FALSE
  )
  
  results_tab_view()
}

mk_env <- function() {
  gui  <<- new.env(parent = globalenv())
  model <<- new.env(parent = globalenv())
  
  model <- empty_model()
  return()
}
