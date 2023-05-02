to_string <- function(x, sep = ", ") {
  paste0(x, collapse = sep)
}

x <- c("a", "b", "c")

x
to_string(x)
to_string(x, sep = " + ")
