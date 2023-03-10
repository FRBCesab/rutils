#' Combine different matrices by row names and column names
#'
#' @description 
#' This function combines different matrices by row names and column names by
#' performing a 2-dimension full join. Gaps are filled with `NA` (default) or
#' `0` (argument `na_to_zero`).
#'
#' @param ... one or several `matrix` objects.
#' 
#' @param na_to_zero a `logical` value. If `TRUE` gaps (i.e. unknown edges) are 
#'   coded as `0`. Otherwise they are coded as `NA` (default).
#'
#' @return A `matrix` object.
#' 
#' @export
#'
#' @examples
#' mat1 <- matrix(rep(1, 9), nrow = 3)
#' colnames(mat1) <- c("A", "B", "C")
#' rownames(mat1) <- c("A", "B", "C")
#' 
#' mat2 <- matrix(rep(1, 9), nrow = 3)
#' colnames(mat2) <- c("D", "E", "F")
#' rownames(mat2) <- c("D", "E", "F")
#' 
#' mat3 <- matrix(rep(1, 9), nrow = 3)
#' colnames(mat3) <- c("F", "G", "H")
#' rownames(mat3) <- c("G", "A", "H")
#' 
#' multi_merge(mat1, mat2, mat3)
#' 
#' multi_merge(mat1, mat2, mat3, na_to_zero = TRUE)

multi_merge <- function(..., na_to_zero = FALSE) {
  
  ## Catch arguments ----
  
  matrices <- list(...)
  
  
  ## Check structures ----
  
  if (length(matrices) == 0) {
    stop("Please provide at least one matrix", call. = FALSE) 
  }
  
  
  check_str <- unlist(lapply(matrices, function(x) {
    ifelse(!is.matrix(x), TRUE, FALSE)
  }))
  
  if (sum(check_str) > 0) {
    stop("This function only works with 'matrix' objects", call. = FALSE)
  }
  
  
  ## Convert list of matrix to a single data.frame ----
  
  list_of_dfs <- lapply(matrices, function(x) {
    
    mat <- as.data.frame(x)
    mat <- data.frame(from = rownames(mat), mat)
    
    tidyr::pivot_longer(mat, cols = -1, names_to = "to", values_to = "edge")
  })
  
  df <- do.call(rbind.data.frame, list_of_dfs)
  
  
  ## Check for false duplicates (different values for a same cell) ----
  
  keys <- paste(df$"from", df$"to", sep = "__")
  
  if (any(duplicated(keys))) {
    
    conflicts <- unlist(lapply(keys[which(duplicated(keys))], function(x) {
      from_to <- strsplit(x, "__")[[1]]
      vals <- df[which(df$"from" == from_to[1] & df$"to" == from_to[2]), "edge", 
                 drop = TRUE]
      ifelse(length(unique(vals)) == 1, FALSE, TRUE)
    }))
    
    if (sum(conflicts) > 0) {
      stop("Some identical cells have different values", call. = FALSE)
    }
    
    df <- df[!duplicated(keys), ]
  }
  
  
  ## Order labels ----
  
  row_s <- sort(unique(df$"from"))
  col_s <- sort(unique(df$"to"))
  
  
  ## Convert to wide format ----
  
  mat <- tidyr::pivot_wider(df, names_from = "to", values_from = "edge", 
                            values_fn = ~.x)
  
  ## Convert to matrix ----
  
  row_names <- mat[ , 1, drop = TRUE]
  mat <- data.matrix(mat[ , -1])
  rownames(mat) <- row_names
  
  
  ## Replace NA by 0 (if required) ----
  
  if (na_to_zero) {
    mat <- ifelse(is.na(mat), 0, mat)
  }
  
  
  ## Order matrix ----
  
  mat[row_s, col_s]
}
