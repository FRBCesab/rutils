#' Correct species binomial name case
#'
#' @description 
#' Corrects species binomial name case.
#' 
#' @param x a `character` vector with misspelled binomial names.
#'
#' @return A `character` vector of same length as `x`.
#' 
#' @export
#'
#' @examples
#' x <- c("sAlMO saLAR (Linnee, 1765)", "tyranosurus_REX rex.1")
#' to_binomial_name(x)

to_binomial_name <- function(x) {
  
  if (missing(x)) {
    stop("Argument 'x' is required", call. = FALSE)  
  }
  
  if (!is.character(x)) {
    stop("Argument 'x' must be a character", call. = FALSE)
  }
  
  
  ## Remove author and year ----
  
  x <- gsub("\\(.+\\)", "", x)
  
  
  ## Clean string ----
  
  x <- gsub("[[:punct:]]|[[:digit:]]", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  
  
  ## Correct case ----
  
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  
  x
}
