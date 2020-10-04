allele_input_view <- function(container = NULL) {
  genetics_frame <- gWidgets2::gframe("Data", horizontal = FALSE, container = container)
  
  # allele frequency database
  database_group <- gWidgets2::gvbox(container = genetics_frame)
  file_chooser('Frequency database', 'Choose...',
               container = database_group,
               handler = database_handler)
  gui$database_description <- gWidgets2::glabel(
    text = 'X',
    handler = function(h, ...) df_modal_view(get_database(), 'Allele frequency database'),
    container = database_group)
  update_database_description(get_database())
  
  
  # reference profiles
  gWidgets2::gseparator(container = genetics_frame)
  genotypes_group <- gWidgets2::gvbox(container = genetics_frame)
  file_chooser('Reference profiles', 'Choose...',
               container = genotypes_group,
               handler = genotypes_handler)
  gui$genotypes_description <- gWidgets2::glabel(
    text = 'Y',
    handler = function(h, ...) df_modal_view(get_genotypes(), 'Reference profiles'),
    container = genotypes_group
  )
  update_genotypes_description(get_genotypes())
  
  genetics_frame
}

database_handler <- function(filename) {
  db <- read_database(filename, format = 'ladder')
  set_database(db)
  update_database_description(get_database())
}

update_database_description <- function(db) {
  if (!isTruthy(db)) {
    gWidgets2::svalue(gui$database_description) <- 'No frequency database loaded.'
  } else {
    gWidgets2::svalue(gui$database_description) <-
      paste0('Frequency database with ', ncol(db), ' markers. Click to view.')
  }
}

genotypes_handler <- function(filename) {
  genotypes <- read_genotypes(filename)
  
  set_genotypes(genotypes)
  
  redraw_pedigrees()
  update_genotypes_description(genotypes)
  update_available_checkboxes(get_candidate_available_ids(), get_available())
}

update_genotypes_description <- function(genotypes) {
  if (!isTruthy(genotypes)) {
    gWidgets2::svalue(gui$genotypes_description) <- 'No reference profiles loaded.'
  } else {
    print(genotypes)
    gWidgets2::svalue(gui$genotypes_description) <-
      paste0('Reference profiles loaded for ',
             paste0(get_genotyped_labels(), collapse = ', '),
             '. Click to view.'
            )
  }
}