
markers_tab_view <- function() {
  gui$markers_tab <- gWidgets2::ggroup(label = 'Markers', container = gui$tabs)
  gui$mtable <- gWidgets2::glayout(container = gui$markers_tab)
  
  update_markers_tab()
  
  gui$markers_tab
}

update_markers_tab <- function() {
  # get settings from the model
  settings <- get_marker_settings()
  
  if (!isTruthy(settings) || nrow(settings) < 1 || ncol(settings) < 1) return();
  
  # clear table
  gWidgets2::delete(gui$markers_tab, gui$mtable)
  gui$mtable <- gWidgets2::glayout(container = gui$markers_tab)
  
  # make heading
  cols <- colnames(settings)
  gui$mtable[1, 1] <- 'Locus'
  for (j in 1:length(cols)) {
    gui$mtable[1, j + 1] <- cols[j]
  }
  
  # make a row for each marker
  mutation_opts <- c('Auto', 'None', 'All')
  rows <- rownames(settings)
  for (i in 1:nrow(settings)) {
    gui$mtable[i + 1, 1] <- rows[i]
    gui$mtable[i + 1, 2] <- gWidgets2::gcheckbox(checked = settings[i, 1], handler = function(h) {
      set_marker_settings(gWidgets2::svalue(h$obj), h$action, cols[1]) # set "Use in calculation?"
    }, action = rows[i], container = gui$mtable)
    gui$mtable[i + 1, 3] <- gWidgets2::gcombobox(mutation_opts, handler = function(h) {
      set_marker_settings(gWidgets2::svalue(h$obj), h$action, cols[2]) # set "Mutations"
    }, action = rows[i], selected = match(settings[i, 2], mutation_opts), container = gui$mtable)
  }
  
  NULL
}