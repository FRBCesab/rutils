#' Produce produces significance symbols for the values of p (ns., *, **, ***)
#'
#' @param p a p value
#' 
#' @return a character "ns", "*", "**", "***" 
#' 
#' @export
#'
#' @examples
#' signi(p=0.004)
#' 

signi <- function(p){
  if (p>0.05) symbol="ns."
  if ((p<=0.05) & (p>0.01)) symbol="*"
  if ((p<=0.01) & (p>0.001)) symbol="**"
  if (p<=0.001) symbol="***"
  return(symbol)
}
