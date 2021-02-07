
markers_tab_view <- function() {
  gui$markers_tab <- gWidgets2::gvbox(label = 'Markers', container = gui$tabs)
  button_group <- gWidgets2::ggroup(container = gui$markers_tab)
  gWidgets2::gbutton(
    text = 'Disable mutations',
    container = button_group,
    action = 'Off',
    handler = handle_global_mutation_change
  )
  gWidgets2::gbutton(
    text = 'Enable mutations',
    container = button_group,
    action = 'On',
    handler = handle_global_mutation_change
  )
  gWidgets2::gbutton(
    text = 'Reset mutations',
    container = button_group,
    action = 'Auto',
    handler = handle_global_mutation_change
  )
  
  gui$mtable <- gWidgets2::glayout(container = gui$markers_tab)
  
  update_markers_tab()
  
  gui$markers_tab
}

handle_global_mutation_change = function(h) {
  set_all_mutations(h$action)
  update_markers_tab()
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
  mutation_opts <- c('Auto', 'Off', 'On')
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