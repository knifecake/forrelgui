#' Round numeric values in a data frame
#'
#' @param df a data.frame
#' @param digits number of digits to round to
#'
#' @return a copy of df with numeric fields rounded accordingly
#'
#' @examples
#' 
#' df <- data.frame(A = c(0.1234, 0.123))
#' round_df(df, digits = 2)
df_round <- function(df, digits = 3) {
  numeric_columns <- sapply(df, mode) == 'numeric'
  df[numeric_columns] <- round(df[numeric_columns], digits = 3)
  df
}

#' Substitute NA values for a string in a data frame
#'
#' @param df a data.frame
#' @param na_string the string to put in place of NA values
#'
#' @return a copy of df without NA values
df_nas_to_string <- function(df, na_string = '-') {
  df[is.na(df)] <- '-'
  df
}


#' Trim a string from the right
#'
#' @param xs a string or character vector
#' @param nchars the number of characters to remove (must be non-negative)
#' @export
#'
#' @examples
#' rtrim("hello", 2) == "hel"
#'
#' rtrim("hello", 10) == ""
#'
#' rtrim("hello", 0) == "hello"
#'
#' rtrim(c("hello", "bye"), 1) == c("hell", "by")
rtrim <- function(xs, nchars) {
  lapply(xs, function(x) { substr(x, 1, nchar(x) - nchars) })
}


#' Check if something is truthy
#'
#' Returns true if the parameter is not false, or similar to false in some way.
#'
#' Adapted from
#' https://github.com/rstudio/shiny/blob/5e7447886405392f6397c7e7b8fbb705050ebab4/R/utils.R#L1430
#'
#' @param x anything
#'
#' @return a logical
isTruthy <- function(x) {
  if (inherits(x, 'try-error'))
    return(FALSE)
  
  if (!is.atomic(x))
    return(TRUE)
  
  if (is.null(x))
    return(FALSE)
  if (length(x) == 0)
    return(FALSE)
  if (all(is.na(x)))
    return(FALSE)
  if (is.character(x) && !any(nzchar(stats::na.omit(x))))
    return(FALSE)
  if (is.logical(x) && !any(stats::na.omit(x)))
    return(FALSE)
  
  return(TRUE)
}
