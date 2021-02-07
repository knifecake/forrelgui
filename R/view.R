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
  
  # status bar
  gui$status_bar <- gWidgets2::gstatusbar(container = gui$main_window)
  
  gWidgets2::visible(gui$main_window) <- TRUE
}
