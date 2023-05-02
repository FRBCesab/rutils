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
  
  for (i in 1:length(old.col.names)) {
    
    names(df)[names(df) == old.col.names[i]] <- new.col.names[i]
  }
  
  df
}
