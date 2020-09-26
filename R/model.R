mk_env <- function() {
  gui  <<- new.env(parent = globalenv())
  model <<- new.env(parent = globalenv())

  model <- list(
    claim_ped = NULL,
    true_ped  = NULL,
    available = c(),
    result = NULL
  )
}

refresh_pedigrees <- function() {
  # redraw plots
  tkrplot::tkrreplot(gui$claim_plot)
  tkrplot::tkrreplot(gui$true_plot)
  
  # update "Available for genotyping" checkboxes
  gui$available_checkboxes[] <- custom_ped_labels(model$claim_ped)
  gWidgets2::svalue(gui$available_checkboxes) <- model$available
}

apply_database <- function() {
  gWidgets2::svalue(gui$database_description) <- paste0(database_description(model$database), '. Click to view.')
  
  if (is.null(model$database)) return()
  
  la <- database_to_pedtools_locusAttributes(model$database)
  
  if (!is.null(model$claim_ped))
    model$claim_ped <- custom_ped_set_markers(model$claim_ped, la)
}
