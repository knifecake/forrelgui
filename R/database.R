read_database <- function(filename, format = c('ladder', 'list'), ...) {
  if (format == 'list') {
    read_database_list(filename, ...)
  } else if (format == 'ladder') {
    ladder_to_list(read_database_ladder(filename, ...))
  } else {
    stop("Allele database format not supported")
  }
}

read_database_list <- function(filename, ...) {
  conn <- file(description = filename, open = "r")
  lines <- readLines(conn)
  
  tmp_markers <- list()
  
  i <- 1
  j <- 1
  while (i <= length(lines)) {
    
    marker_name <- lines[i]
    i <- i + 1
    
    afreq <- list()
    
    while (i <= length(lines)) {
      if (lines[i] == "") break
      
      unpacked <- unlist(strsplit(lines[i], "\t", fixed = TRUE))
      al <- unpacked[1]
      freq <- as.numeric(unpacked[2])
      
      afreq[al] <- freq
      
      i <- i + 1
    }
    
    tmp_markers[[j]] <- list(name = marker_name,
                             afreq = afreq)
    
    i <- i + 1
    j <- j + 1
  }
  
  close(conn)
    
  marker_names <- unlist(lapply(tmp_markers, function(m) { m$name }))

  
  all_alleles <- unique(unlist(lapply(tmp_markers, function(m) { names(m$afreq) })))
  
  wide_freq <- lapply(tmp_markers, function(m) {
    lapply(all_alleles, function(a) {
      f <- list()
      if (a %in% names(m$afreq)) {
        f[a] <- m$afreq[a]
      } else {
        f[a] <- NA
      }
      f
    })
  })
  
  
  df <- as.data.frame(lapply(wide_freq, unlist))
  colnames(df) <- marker_names
  
  df
}

#' Read allele frequency database in ladder format
#'
#' @param filename a filepath
#' @param ... further parameters to \code{read.csv}
#'
#' @return a \code{\link{data.frame}}
#' @export
read_database_ladder <- function(filename, ...) {
  df <- read.csv(filename,
                 header = TRUE,
                 sep = '\t',
                 stringsAsFactors = FALSE, ...)
  
  df
}

alleles <- function(df, ms = NULL) {
  if (is.null(ms)) ms <- colnames(df)
  
  if (length(ms) == 1) {
    return(rownames(df[!is.na(df[, ms[1]]), ]))
  }
  
  rownames(df[apply(!is.na(df[, ms]), 1, any), ms])
}

markers <- colnames


#' Normalize a frequency database
#'
#' Scales a frequency database so that allele frequencies sum up to one.
#'
#' @param x a data frame in allelic ladder format
#' 
#' @return a scaled version of the given frequency database
normalize <- function(df) {
  data.frame(
    lapply(df, function(x) scale(x, center = FALSE, scale = sum(x, na.rm = TRUE))),
    row.names = rownames(df))
}


list_to_ladder <- function(lst) {
  all_alleles <- unique(unlist(lapply(lst, function(m) names(m))))
  
  as.data.frame(lapply(lst, function(marker) {
    unlist(lapply(all_alleles, function(allele) {
      f <- c()
      if (allele %in% names(marker)) {
        f[allele] <- marker[[allele]]
      } else {
        f[allele] <- NA
      }
      f
    }))
  }))
}

ladder_to_list <- function(ladder) {
  lapply(as.list(ladder), function(m) { 
    names(m) <- rownames(ladder)
    m[!is.na(m)]
  })
}