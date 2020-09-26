mk_menu_bar <- function() {
  menu_items <- list(
    File = list(
      gWidgets2::gaction('About epGUI',
                         handler = about_epgui_handler),
      gWidgets2::gaction('Open project...',
                         handler = open_project_handler),
      gWidgets2::gaction('Save project...',
                         handler = save_project_handler),
      gWidgets2::gaction('Quit',
                         handler = function(h) gWidgets2::dispose(gui$main_window))
    )
  )

  gWidgets2::gmenu(menu_items, container = gui$main_window)
}

about_epgui_handler <- function(h) {
  version <- packageVersion('forrelgui')

  about <- gWidgets2::gwindow("About epGUI", visible = FALSE, parent = gui$main_window)
  layout <- gWidgets2::gvbox(container = about)

  gWidgets2::glabel(paste0("epGUI v", version), container = layout)
  gWidgets2::gbutton("Close",
                     handler = function(h) gWidgets2::dispose(about),
                     container = layout)

  gWidgets2::visible(about) <- TRUE
}

save_project_handler <- function(h) {
  filename <- gWidgets2::gfile(text = "Save project to...",
                               type = "save",
                               initial.filename = "untitled.Rdata")

  save(
    model,
    file = filename
  )
}

open_project_handler <- function(h) {
  filename <- gWidgets2::gfile(text = "Choose project file...",
                              type = "open")
  load(filename, envir = globalenv())
  refresh_pedigrees()
  apply_database()
}
