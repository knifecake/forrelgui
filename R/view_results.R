results_tab_view <- function() {
  # if there is already a results tab, dispose of it
  if (!is.null(gui$results_tab)) {
    gWidgets2::svalue(gui$tabs) <- 3
    gWidgets2::dispose(gui$tabs)
  }
  
  print(model$result)
  if (is.null(model$result)) {
    # if there are no results, do not add the tab and make
    # sure we're in the data entry tab
    gWidgets2::svalue(gui$tabs) <- 1
    return()
  }
  
  gui$results_tab <- gWidgets2::gvbox(container = gui$tabs, label = 'Results')
  
  # per marker table
  ep_per_marker = simplify2array(lapply(model$result, function(x) { x$EPtotal }))
  df <- data.frame(
    Marker = get_selected_markers(),
    'Exclusion Power' = ep_per_marker,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  total_EP <- 1 - prod(1 - df$`Exclusion Power`)
  
  df <- df_round(df)
  df <- df_nas_to_string(df)
  
  table <- gWidgets2::gtable(df, container = gui$results_tab, expand = TRUE, fill = TRUE)
  gWidgets2::size(table) <- list(column.widths = c(100, 50))
  
  gWidgets2::glabel(
    text = paste0('Total exclusion power: ', round(total_EP, digits = 3)),
    container = gui$results_tab
  )
  
  # switch to the results tab
  gWidgets2::svalue(gui$tabs) <- 3
}


calculate_ep <- function() {
  if (is.null(model$claim_ped) || is.null(model$true_ped)) return()
  
  markers <- get_selected_markers()
  settings <- get_settings()
  
  svalue(gui$status_bar) <- paste0(
    'Calculating exclusion power: 0 of ', length(markers), ' markers done.')
  
  # get marker settings and include
  marker_settings_df <- data.frame(get_marker_settings())
  marker_settings_df$Mutations[marker_settings_df$Mutations == 'Off']   <- TRUE
  marker_settings_df$Mutations[marker_settings_df$Mutations == 'On']    <- FALSE
  marker_settings_df$Mutations[marker_settings_df$Mutations == 'Auto']  <- NA
  
  model$result <- list()
  for (i in 1:length(markers)) {
    model$result[[markers[i]]] <- forrel::exclusionPower(
      claimPed = model$claim_ped,
      truePed = model$true_ped,
      ids = model$available,
      markers = c(markers[i]),
      verbose = FALSE,
      disableMutations = marker_settings_df[markers[i], 2][[1]],
      exactMaxL = settings$exactMaxL,
      nsim = settings$nsim
    )
    print(2)
    svalue(gui$status_bar) <- paste0('Calculating exclusion power: ', i, ' of ', length(markers), ' done.')
  }

  results_tab_view()
}