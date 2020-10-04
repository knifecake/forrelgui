main_view  <- function() {
  gui$main_window <- gWidgets2::gwindow('forrel - Exclusion Power',
                                        width = 900,
                                        height = 600,
                                        visible = FALSE)
  
  mk_menu_bar()
  
  gui$layout <- gWidgets2::ggroup(container = gui$main_window)
  
  gui$tabs <- gWidgets2::gnotebook(container = gui$layout)
  gui$peds <- gWidgets2::ggroup(container = gui$layout, width = 200)
  
  # pedigree plots
  pedigree_plots_view()
  
  # data input tab
  data_input_tab_view()
  
  # marker settings tab
  markers_tab_view()
  
  # results tab
  results_tab_view()
  
  # the first tab is open by default
  gWidgets2::svalue(gui$tabs) <- 1
  
  gWidgets2::visible(gui$main_window) <- TRUE
}

data_input_tab_view <- function() {
  data_tab <- gWidgets2::gvbox(label = 'Pedigrees', container = gui$tabs)
  
  # pedigree input
  ped_input <- pedigree_input_view(container = data_tab)
  
  # case data and frequency database input
  allele_input <- allele_input_view(container = data_tab)
  
  # launch calculation
  gWidgets2::gbutton('Calculate exclusion power',
                     handler = function(h) calculate_ep(),
                     container = data_tab)
  
  data_tab
}

markers_tab_view <- function() {
  gWidgets2::ggroup(label = 'Markers', container = gui$tabs)
}

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
    Marker = get_marker_names(model$claim_ped),
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