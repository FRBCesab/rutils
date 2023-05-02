#' Rename any columns of a dataframe (from 1 to as much columns as you want)
#'
#' @param df a dataframe.
#' @param old.col.names a vector of columns names to rename
#' @param new.col.names a vector of columns new names
#'
#' @return a dataframe.
#' 
#' @export
#'
#' @examples
#' df <- cbind.data.frame(a=1:10,b=1:10,c=1:10,d=1:10)
#' df <- rename_col(df,old.col.names=c("a","d"),new.col.names=c("aa","dd"))
#' 

rename_col <- function(df, old.col.names, new.col.names){
  for (i in 1:length(old.col.names)) names(df)[names(df) == old.col.names[i]] <- new.col.names[i]
  df
}
