settings_view <- function(parent = NULL) {
  w <- gWidgets2::gwindow(title = 'Settings', parent = parent, visible = FALSE)
  rows <- gWidgets2::gvbox(container = w)

  # allele number threshold for exact calculations
  grp <- gWidgets2::ggroup(container = rows)
  gWidgets2::glabel('Maximum number of alleles for exact calculation', container = grp)
  gui$exactMaxL <- gWidgets2::gedit(container = grp)
  
  # number of simultaions
  grp <- gWidgets2::ggroup(container = rows)
  gWidgets2::glabel('Number of simulations', container = grp)
  gui$nsim <- gWidgets2::gedit(container = grp)
  
  grp <- gWidgets2::ggroup(container = rows)
  gWidgets2::gbutton('Apply', container = grp, handler = function(h) {
    set_settings(
      exactMaxL = gWidgets2::svalue(gui$exactMaxL),
      nsim = gWidgets2::svalue(gui$nsim)
    )
    gWidgets2::dispose(w)
  })
  
  update_settings_view()
  
  gWidgets2::visible(w) <- TRUE
}

update_settings_view <- function() {
  settings <- get_settings()
  gWidgets2::svalue(gui$exactMaxL) <- settings$exactMaxL
  gWidgets2::svalue(gui$nsim) <- settings$nsim
}