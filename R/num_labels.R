#' Add leading zeros to a vector
#'
#' @description 
#' Adds n leading zeros to a vector so that each element of the vector has the 
#' same number of characters. See examples below.
#'
#' @param x a `character` or a `numeric` vector.
#'
#' @return A `character` vector of the same length as `x`.
#' 
#' @export
#'
#' @examples
#' ## On numeric vector ----
#' x <- 1:10
#' leading_zero(x)
#' 
#' x <- 50:100
#' leading_zero(x)
#' 
#' ## On floating vector ----
#' x <- c(1, 9.5, 10, 10.2)
#' leading_zero(x)
#' 
#' ## On character vector ----
#' x <- as.character(1:10)
#' leading_zero(x)
#' 
#' ## Creating sortable identifiers ----
#' x1 <- as.character(10:1)
#' x2 <- leading_zero(x)
#' 
#' sort(x1)
#' sort(x2)
#' 
#' x2 <- paste0("ID", x2)
#' sort(x2)


leading_zero <- function(x) {
  
  if (missing(x)) {
    stop("Argument 'x' is required", call. = FALSE)
  }
  
  if (!is.numeric(x) && !is.character(x)) {
    stop("Argument 'x' must be a numeric or a character vector", call. = FALSE)
  }
  
  if (any(is.na(x))) {
    stop("Argument 'x' cannot contain any NA values", call. = FALSE)
  }
  
  if (is.character(x)) {
    x <- tryCatch(as.numeric(x), 
                  warning = function(w) {
                    stop("Unable to convert 'x' to a numeric", call. = FALSE) })
  }
 
  gsub("\\s", "0", format(x))
}
