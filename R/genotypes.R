read_genotypes <- function(filename, ...) {
  read.table(filename,
             sep = '\t',
             check.names = FALSE,
             stringsAsFactors = FALSE)
}