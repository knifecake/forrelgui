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
  
  if (!is.null(save_file)) load(save_file, envir = globalenv())

  gui$main_window <- gWidgets2::gwindow('forrel - Exclusion Power',
                                        width = 900,
                                        height = 600,
                                        visible = FALSE)

  mk_menu_bar()

  gui$layout <- gWidgets2::ggroup(container = gui$main_window)

  gui$tabs <- gWidgets2::gnotebook(container = gui$layout)
  gui$peds <- gWidgets2::ggroup(container = gui$layout, width = 200)

  # pedigree plots
  gui$claim_plot <- tkrplot::tkrplot(
    gWidgets2::getToolkitWidget(gui$peds),
    function() custom_ped_plot(model$claim_ped, model$available, get_genotyped_ids(model$claim_ped))
  )
  gWidgets2::add(gui$peds, gui$claim_plot)

  gui$true_plot <- tkrplot::tkrplot(
    gWidgets2::getToolkitWidget(gui$peds),
    function() custom_ped_plot(model$true_ped, model$available, get_genotyped_ids(model$claim_ped))
  )
  gWidgets2::add(gui$peds, gui$true_plot)

  # data input tab
  data_tab <- gWidgets2::gvbox(label = 'Pedigrees', container = gui$tabs)
  ped_frame <- gWidgets2::gframe("Pedigrees", horizontal = FALSE, container = data_tab)
  file_chooser(label = "Claim pedigree", text = "Choose...", container = ped_frame,
               handler = mk_pedfile_handler('claim_ped', gui$claim_plot))

  file_chooser("True pedigree", "Choose...", container = ped_frame,
               handler = mk_pedfile_handler('true_ped', gui$true_plot))

  genetics_frame <- gWidgets2::gframe("Data", horizontal = FALSE, container = data_tab)
  
  database_group <- gWidgets2::gvbox(container = genetics_frame)

  file_chooser("Frequency database", "Choose...",
               container = database_group,
               handler = database_handler)
  gui$database_description <- gWidgets2::glabel("No database loaded",
                                                handler = function(h, ...) show_df_modal(model$database, "Allele frequency database"),
                                                container = database_group)

  file_chooser("Reference profiles", "Choose...",
               container = genetics_frame,
               handler = reference_handler)

  gWidgets2::glabel("Individuals available for genotyping:", container = genetics_frame)
  gui$available_checkboxes <- gWidgets2::gcheckboxgroup(
    custom_ped_labels(model$claim_ped),
    container = genetics_frame,
    horizontal = TRUE,
    handler = function(h) {
      model$available <- gWidgets2::svalue(h$obj)
      tkrplot::tkrreplot(gui$claim_plot)
      tkrplot::tkrreplot(gui$true_plot)
    }
  )
  gWidgets2::svalue(gui$available_checkboxes) <- model$available
  
  gWidgets2::gbutton('Calculate exclusion power',
                     handler = function(h) calculate_ep(),
                     container = data_tab)


  # marker settings tab
  gWidgets2::ggroup(label = 'Markers', container = gui$tabs)

  # the first tab is open by default
  gWidgets2::svalue(gui$tabs) <- 1

  gWidgets2::visible(gui$main_window) <- TRUE
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
  
  display_results()
}

display_results <- function() {
  if (!is.null(gui$results_tab)) {
    gWidgets2::svalue(gui$tabs) <- 3
    gWidgets2::dispose(gui$tabs)
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
  
  # switch to the results tabs
  gWidgets2::svalue(gui$tabs) <- 3
}

mk_pedfile_handler <- function(dest_field, plot) {
  function(f) {
    tryCatch({
      model[[dest_field]] <<- pedtools::readPed(f)
      refresh_pedigrees()
    }, error = function(e) {
      print(e)
      gWidgets2::gmessage("The pedigree file you supplied does not appear to be a valid .ped file.",
                          title = "Invalid .ped file",
                          icon = "error")
    })
  }
}

database_handler <- function(filename) {
  model$database <- read_database(filename, format = 'ladder')
  apply_database()
}

reference_handler <- function(filename) {
  res <- read_case_data(model$claim_ped, filename, format = 'pedtools')
  
  model$claim_ped <- res$ped
  model$references <- res$df
  
  # redraw plots
  tkrplot::tkrreplot(gui$claim_plot)
  tkrplot::tkrreplot(gui$true_plot)
}

show_df_modal <- function(df, title = "Preview") {
  if (!is.data.frame(df)) return()
  
  # round frequencies for better display
  df <- df_round(df, digits = 3)
  
  # substitute NA values for a string, since gtable does not understand them
  df <- df_nas_to_string(df)
  
  w <- gWidgets2::gwindow(title = title,
                          width = 700,
                          height = 400,
                          visible = FALSE,
                          parent = gui$main_window)
  t <- gWidgets2::gtable(df, container = w)
  
  gWidgets2::size(t) <- list(column.widths = replicate(length(colnames(df)), 10 * max(sapply(colnames(df), nchar))))
  
  gWidgets2::visible(w) <- TRUE
}
