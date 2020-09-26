read_case_data <- function(x, filename, format = c('generic', 'familias'), ...) {
  if (format == 'generic') {
    read_generic_case_data(x, filename, ...)
  } else if (format == 'familias') {
    read_familias_case_data(x, filename)
  } else if (format == 'pedtools') {
    read_pedtools_case_data(x, filename, ...)
  } else {
    stop('Reference data format is not supported')
  }
}

read_pedtools_case_data <- function(x, filename, ...) {
  if (pedtools::is.pedList(x)) {
    return(lapply(x, function(p) {
      read_pedtools_case_data(p, filename, ...)
    }))
  }
  
  df <- read.table(filename,
                   header = TRUE,
                   sep = '\t',
                   stringsAsFactors = FALSE,
                   check.names = FALSE)
  
  print(df)
  
  x <- pedtools::setAlleles(x, alleles = df)
  
  list(ped = x, df = df)
}

#' Import Familias case data
#'
#' @param x a \code{\link[pedtools]{ped}} object or a list of such
#' @param file a path to a Familias case data file
#'
#' @return an updated version of \code{x} with provided genotypes attached
#' @export
read_familias_case_data <- function(x, file) {
  if (pedtools::is.pedList(x)) {
    return(lapply(x, function(p) {
      read_familias_case_data(p, file)
    }))
  }

  df <- read.table(file,
                   sep = '\t',
                   header = T,
                   row.names = 1,
                   as.is = T,
                   check.names = F)


  ids = intersect(rownames(df), labels(x))
  markers = intersect(rtrim(colnames(df), 2), get_marker_names(x))
  relevant_alleles <- df[ids, rtrim(colnames(df), 2) %in% markers]

  pedtools::setAlleles(x,
                       ids = ids,
                       markers = markers,
                       alleles = relevant_alleles)
}

#' Read case data (in the generic format)
#'
#' @param x a \code{\link[pedtools]{ped}} object or a list of such
#' @param file file name of an appropriate CSV file
#' @param ... further parameters passed to \code{read.table}
#'
#' @return an updated version of \code{x} with the provided genotypes attached
#'
#' @importFrom utils read.table
#'
#' @export
read_generic_case_data <- function(x, file, ...) {
  if (pedtools::is.pedList(x)) {
    return(lapply(x, function(p) {
      read_generic_case_data(p, file, ...)
    }))
  }

  df <- read.table(file, ...)

  markers <- unique(df[,2])

  for (mname in markers) {
    # find df rows concerning the current marker
    relevant = df[df[, 2] == mname, ]
    for (person in relevant[, 1]) {
      if (person %in% labels(x)) {
        alleles <- c(relevant[relevant[, 1] == person, 3],
                     relevant[relevant[, 1] == person, 4])
        x <- pedtools::setAlleles(x,
                                  ids = person,
                                  markers = mname,
                                  alleles = alleles)
      }
    }
  }

  x
}
