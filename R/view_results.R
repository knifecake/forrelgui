results_tab_view <- function() {
  # if there is already a results tab, dispose of it
  if (!is.null(gui$results_tab)) {
    gWidgets2::svalue(gui$tabs) <- 3
    gWidgets2::dispose(gui$tabs)
  }
  
  if (is.null(model$result)) {
    # if there are no results, do not add the tab and make
    # sure we're in the data entry tab
    gWidgets2::svalue(gui$tabs) <- 1
    return()
  }
  
  gui$results_tab <- gWidgets2::gvbox(container = gui$tabs, label = 'Results')
  
  # per marker table
  df <- data.frame(
    Marker = get_selected_markers(),
    'Exclusion Power' = model$result$EPperMarker,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  df <- df_round(df)
  df <- df_nas_to_string(df)
  
  table <- gWidgets2::gtable(df, container = gui$results_tab, expand = TRUE, fill = TRUE)
  gWidgets2::size(table) <- list(column.widths = c(100, 50))
  
  gWidgets2::glabel(
    text = paste0('Total exclusion power: ', round(model$result$EPtotal, digits = 3)),
    container = gui$results_tab
  )
  
  gWidgets2::glabel(
    text = paste0('Time used: ', round(as.numeric(model$result$time), digits = 2), ' seconds'),
    container = gui$results_tab
  )
  
  # switch to the results tab
  gWidgets2::svalue(gui$tabs) <- 3
}


calculate_ep <- function() {
  if (is.null(model$claim_ped) || is.null(model$true_ped)) return()
  
  model$result <- NULL
  
  model$result <- forrel::exclusionPower(
    claimPed = model$claim_ped,
    truePed = model$true_ped, 
    ids = model$available,
    verbose = FALSE,
    markers = get_selected_markers()
  )
  
  results_tab_view()
}