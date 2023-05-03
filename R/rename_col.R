#' Rename any columns of a data.frame (from 1 to as much columns as you want)
#'
#' @description
#' Renames any columns of a `data.frame` (from 1 to as much columns as you 
#' want).
#' 
#' @param df a `data.frame`
#' @param old.col.names a `character` vector of column names to rename
#' @param new.col.names a `character` vector of column new names
#'
#' @return The same `data.frame` as `df`.
#' 
#' @export
#'
#' @examples
#' df <- cbind.data.frame(a = 1:10, b = 1:10, c = 1:10, d = 1:10)
#' df
#' 
#' rename_col(df, 
#'            old.col.names = c("a", "d"), 
#'            new.col.names = c("aa", "dd"))
#' 

rename_col <- function(df, old.col.names, new.col.names) {
  
  if (missing(df)) {
    stop("Argument 'df' is required", call. = FALSE)
  }
  
  if (!is.data.frame(df)) {
    stop("Argument 'df' must be a data.frame", call. = FALSE)
  } 
  
  if (missing(old.col.names)) {
    stop("Argument 'old.col.names' is required", call. = FALSE)
  }
  
  if (!is.character(old.col.names)) {
    stop("Argument 'old.col.names' must be a character", call. = FALSE)
  }
  
  if (missing(new.col.names)) {
    stop("Argument 'new.col.names' is required", call. = FALSE)
  }
  
  if (!is.character(new.col.names)) {
    stop("Argument 'new.col.names' must be a character", call. = FALSE)
  }
  
  if (length(new.col.names) != length(old.col.names)) {
    stop("Arguments 'old.col.names' and 'new.col.names' must be of the same ", 
         "length", call. = FALSE)
  }
  
  if (any(!(old.col.names %in% colnames(df)))) {
    stop("Some names in 'old.col.names' are absent from 'df'", call. = FALSE)
  }
  
  for (i in 1:length(old.col.names)) {
    
    names(df)[names(df) == old.col.names[i]] <- new.col.names[i]
  }
  
  df
}
