xlsx_export <- function(file, model) {
  if (!requireNamespace("writexl", quietly = TRUE)) return();
  
  writexl::write_xlsx(list('Results' = mtcars), path = file)
}