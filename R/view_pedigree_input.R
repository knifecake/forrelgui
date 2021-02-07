pedigree_input_view <- function(container = NULL) {
  ped_frame <- gWidgets2::gframe("Pedigrees", horizontal = FALSE, container = container)
  
  file_chooser(label = "Claim pedigree", text = "Choose...", container = ped_frame,
               handler = mk_pedfile_handler('claim'))
  
  file_chooser("True pedigree", "Choose...", container = ped_frame,
               handler = mk_pedfile_handler('true'))
  
  # individuals available for genotyping
  gWidgets2::gseparator(container = ped_frame)
  gui$available_description <- gWidgets2::glabel(
    text = '',
    container = ped_frame
  )
  gui$available_checkboxes <- gWidgets2::gcheckboxgroup(
    items = list(),
    container = ped_frame,
    horizontal = TRUE,
    handler = function(h) {
      set_available(gWidgets2::svalue(h$obj))
      tkrplot::tkrreplot(gui$claim_plot)
      tkrplot::tkrreplot(gui$true_plot)
    }
  )
  update_available_checkboxes(get_candidate_available_ids(), get_available())
}

mk_pedfile_handler <- function(dest = c('claim', 'true')) {
  setter <- NULL
  
  if (dest == 'claim')
    setter <- set_claim_ped
  else if (dest == 'true')
    setter <- set_true_ped
  else
    stop('Invalid parameter dest. Must be one of `claim` or `true`.')
  
  function(filename) {
    tryCatch({
      setter(pedtools::readPed(filename))
      redraw_pedigrees()
      update_database_description(get_database())
      update_genotypes_description(get_genotypes())
      update_available_checkboxes(options = get_candidate_available_ids(),
                                  selected = get_available())
    }, error = function(e) {
      print(e)
      gWidgets2::gmessage(
        "The pedigree file you supplied does not appear to be a valid .ped file.",
        title = "Invalid .ped file",
        icon = "error")
    })
  }
}

update_available_checkboxes <- function(options, selected = list()) {
  if (isTruthy(options)) {
    gWidgets2::svalue(gui$available_description) <- 'Individuals available for genotyping:'
    
    gui$available_checkboxes[] <- options
    gWidgets2::svalue(gui$available_checkboxes) <- selected
  } else {
    gWidgets2::svalue(gui$available_description) <- 'No individuals available for genotyping. Load pedigrees first.'
  }
}