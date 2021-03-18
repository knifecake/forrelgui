empty_model <- function() {
  list(
    # a pedtools::ped object, or a list of such
    claim_ped = NULL,
    
    # a pedtools::ped object, or a list of such
    true_ped  = NULL,
    
    # a character vector containing the IDs of the individuals which are marked
    # as available for genotyping. this is indented to be passed as the ids
    # parameter to forrel::exclusionPower
    available = c(),
    
    # an allele frequency database, in the format returned by
    # pedtools::getFreqDatabase(x, format = 'list')
    database = NULL,
    
    # a list containing global settings
    settings = NULL,
    
    # a data frame with three columns and one row per marker
    marker_settings = NULL,
    
    # an allele matrix, in the format returned by pedtools::getAlleles
    genotypes = NULL,
    
    # the result of a computation
    result = NULL
  )
}

set_true_ped <- function(x) {
  if (isTruthy(x)) {
    model$true_ped <- x
    clean_available()
  }
  
  NULL
}

set_claim_ped <- function(x) {
  if (!isTruthy(x)) return(NULL)
  
  model$claim_ped <- x
  clean_available()
  
  # apply locus attributes
  apply_locus_attributes()
  
  # apply genotypes
  apply_genotypes()
}

apply_locus_attributes <- function() {
  if (!isTruthy(model$claim_ped) || !isTruthy(model$database)) return(NULL)
  
  la <- lapply(names(model$database), function(marker_name) {
    list(
      alleles = names(model$database[[marker_name]]),
      afreq   = as.numeric(model$database[[marker_name]]),
      name    = marker_name
      
      # TODO: add mutation model and rate
    )
  })
  
  names(la) <- NULL
  
  model$claim_ped <- pedtools::setMarkers(model$claim_ped, locusAttributes = la)
  clean_available()
  
  NULL
}

apply_genotypes <- function() {
  if (!isTruthy(model$claim_ped)) return(NULL)
  
  if (!isTruthy(model$genotypes)) {
    # remove genotypes
    model$claim_ped <- pedtools::setAlleles(model$claim_ped, alleles = 0)
  } else {
    model$claim_ped <- pedtools::setAlleles(model$claim_ped, alleles = model$genotypes)
  }
}

get_database <- function(mode = 'ladder') {
  if (!isTruthy(model$database)) return(NULL)
  
  if (mode == 'ladder')
    list_to_ladder(model$database)
  else
    model$database
}

set_database <- function(db) {
  if (!isTruthy(db)) return(NULL)
  
  model$database <- db
  
  apply_locus_attributes()
  init_marker_settings()
  
  NULL
}

get_genotyped_labels <- function() {
  get_genotyped_ids(model$claim_ped)
}

get_genotypes <- function() {
  if (!isTruthy(model$genotypes)) return(NULL);
  
  data.frame(model$genotypes,
             row.names = rownames(model$genotypes),
             stringsAsFactors = FALSE,
             check.names = FALSE)
}

set_genotypes <- function(genotypes) {
  if (!isTruthy(genotypes)) return(NULL)
  
  model$genotypes <- genotypes

  
  apply_genotypes()
  
  # remove genotyped IDs from available list
  model$available <- setdiff(model$available, get_genotyped_labels())
}

remove_genotypes <- function() {
  model$genotypes <- NULL
  
  apply_genotypes()
}


get_available <- function() {
  model$available
}

set_available <- function(available) {
  if (!isTruthy(available)) return(NULL);
  
  model$available <- available

  NULL  
}

init_marker_settings <- function() {
  markers <- names(get_database(mode = 'list'))
  
  model$marker_settings <- data.frame(t(sapply(markers, function(m) list(
    'Use in calculation?' = TRUE,
    'Mutations' = 'Auto'
  ))), check.names = FALSE)
  
  NULL
}

get_marker_settings <- function() {
  if (!isTruthy(model$marker_settings)) init_marker_settings()
  
  model$marker_settings
}

set_marker_settings <- function(settings, marker = NULL, name = NULL) {
  # apply just one setting to one marker
  if (!is.null(marker) && !is.null(name)) {
    model$marker_settings[marker, name] <- settings
    return(NULL);
  }
  
  if (!isTruthy(settings)) return(NULL);
  
  model$marker_settings <- settings
  
  NULL
}

set_all_mutations <- function(value) {
  if (!isTruthy(value)) return();
  
  model$marker_settings$Mutations <- rep(value)
}

clean_available <- function() {
  model$available <- intersect(get_available(), get_candidate_available_ids())
}

get_settings <- function() {
  if (!isTruthy(model$settings)) {
    model$settings = list(exactMaxL = Inf, nsim = 1000, seed = NULL)
  }
  
  model$settings
}

set_settings <- function(exactMaxL = NULL, nsim = NULL, seed = NULL) {
  # make sure settings are initialized
  get_settings()
  
  if (isTruthy(exactMaxL)) {
    model$settings$exactMaxL <- as.numeric(exactMaxL)
  }
  
  if (isTruthy(nsim)) {
    model$settings$nsim <- as.numeric(nsim)
  }
  
  if (isTruthy(seed)) {
    model$settings$seed <- as.numeric(seed)
  }
}

#' Determines whether the exclusion power can be calculated
#'
#' @return TRUE if all the ingredients for calculating EP are present, otherwise
#'   returns an error message
#'   
can_calculate_ep <- function() {
  if (!isTruthy(model$claim_ped))
    return('Missing claim pedigree')
  if (!isTruthy(model$true_ped))
    return('Missing true pedigree')
  if (!isTruthy(model$available) || length(model$available) == 0)
    return('No individuals were marked as available for genotyping')
  if (!isTruthy(model$database))
    return('No allele frequency database provided')
  
  return(TRUE)
}


#' Calculate IDs that could be available for genotyping
#'
#' An ID can be defined as available for genotyping if: 1. It is present in both
#' the claim pedigree and the true pedigree, and 2. it has not been already
#' genotyped.
#'
#' @return a character vector of IDs that the user may choose to define as
#'   available for genotyping
#'   
get_candidate_available_ids <- function() {
  setdiff(
    intersect(custom_ped_labels(model$claim_ped), custom_ped_labels(model$true_ped)),
    get_genotyped_labels()
  )
}

get_selected_markers <- function() {
  ms = get_marker_settings()
  
  rownames(ms[ms[, 1] == TRUE, ])
}
