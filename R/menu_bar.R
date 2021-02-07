mk_menu_bar <- function() {
  menu_items <- list(
    File = list(
      gWidgets2::gaction('About epGUI',
                         handler = about_epgui_handler),
      gWidgets2::gseparator(),
      gWidgets2::gaction('Open project...',
                         handler = open_project_handler),
      gWidgets2::gaction('Save project...',
                         handler = save_project_handler),
      gWidgets2::gseparator(),
      gWidgets2::gaction('Open Familias .fam file...',
                         handler = open_familias_handler),
      gWidgets2::gseparator(),
      gWidgets2::gaction('Project settings', handler = open_settings_handler),
      gWidgets2::gseparator(),
      gWidgets2::gaction('Quit',
                         handler = function(h) gWidgets2::dispose(gui$main_window))
    ),
    Help = list(
      gWidgets2::gaction('Examples', handler = help_examples_handler)
    )
  )
  
  gWidgets2::gmenu(menu_items, container = gui$main_window)
}

about_epgui_handler <- function(h) {
  version <- packageVersion('forrelgui')
  
  about <- gWidgets2::gwindow("About epGUI", visible = FALSE, parent = gui$main_window)
  layout <- gWidgets2::gvbox(container = about)
  
  gWidgets2::glabel(paste0("epGUI v", version), container = layout)
  gWidgets2::glabel('epGUI is a graphical user interface for the forrel package which is part of the pedsuite', container = layout())
  gWidgets2::gbutton(
    "Close",
    handler = function(h)
      gWidgets2::dispose(about),
    container = layout
  )
  
  gWidgets2::visible(about) <- TRUE
}

save_project_handler <- function(h) {
  filename <- gWidgets2::gfile(text = "Save project to...",
                               type = "save",
                               initial.filename = "untitled.Rdata")
  
  if (!isTruthy(filename)) {
    return();
  }
  
  save(
    model,
    file = filename
  )
}

open_project_handler <- function(h) {
  filename <- gWidgets2::gfile(text = "Choose project file...",
                               type = "open", filter=c("Rdata"))
  if (!isTruthy(filename)) {
    return();
  }
  
  load_project_file(filename)
  redraw_pedigrees()
  update_database_description(get_database())
  update_genotypes_description(get_genotypes())
  update_available_checkboxes(options = get_candidate_available_ids(),
                              selected = get_available())
  update_markers_tab()
}

open_familias_handler <- function(h) {
  filename <- gWidgets2::gfile(text = 'Choose Familias .fam file...',
                               type = 'open')
  
  if (!isTruthy(filename)) {
    return();
  }
  
  # read Familias .fam file and extract data
  tryCatch({
    out <- forrel::readFam(filename, verbose = FALSE)
    claim_ped <- out[[1]]
    true_ped <- out[[2]]
    db <- pedtools::getFreqDatabase(claim_ped, format = 'list')
    genotypes <- pedtools::getAlleles(claim_ped)
    
    # update the model
    set_claim_ped(claim_ped)
    set_true_ped(true_ped)
    set_database(db)
    set_genotypes(genotypes)
  }, error = function(e) {
    gWidgets2::gmessage('It looks like the provided file was not a valid Familias .fam file.',
                        title = 'Could not open file',
                        icon = 'error',
                        parent = gui$main_window)
  })
  
  # update the UI
  redraw_pedigrees()
  update_database_description(get_database())
  update_genotypes_description(get_genotypes())
  update_available_checkboxes(options = get_candidate_available_ids(),
                              selected = get_available())
  update_markers_tab()
}

open_settings_handler <- function(h) {
  settings_view(parent=gui$main_window)
}

help_examples_handler <- function(h) {
  filename = gWidgets2::gfile(
    text = 'Choose example...',
    type='open',
    filter = c('.Rdata'),
    initial.dir = system.file('extdata', package = 'forrelgui')
  )
  if (!isTruthy(filename)) {
    return();
  }
  
  load_project_file(filename)
  redraw_pedigrees()
  update_database_description(get_database())
  update_genotypes_description(get_genotypes())
  update_available_checkboxes(options = get_candidate_available_ids(),
                              selected = get_available())
  update_markers_tab()
}