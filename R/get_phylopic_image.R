#' Download a PhyloPic silhouette
#'
#' @description 
#' This function downloads a silhouette from the 
#' [PhyloPic](https://www.phylopic.org/) database. With the new API, some 
#' functions of the package
#' [`rphylopic`](https://github.com/palaeoverse-community/rphylopic) don't 
#' work anymore. This function aims to temporary replace the function 
#' `rphylopic::image_data()` as this package seems to be looking for a new 
#' maintainer.
#'
#' @param uuid a `character` of length 1. The UUID (unique taxa identifier in 
#'   the Phylopic database).
#'   
#' @param size an `integer` of length 1. The size of the PNG to download. An 
#'   error will be raised in the size is not available.
#'
#' @return An array of dimensions red x green x blue x alpha.
#' 
#' @export
#'
#' @examples
#' \dontrun{
## Download mammal silhouette ----
#' mammals_pic <- get_phylopic_image("8cad2b22-30d3-4cbd-86a3-a6d2d004b201")
#' 
#' ## Plot the silhouette ----
#' ggplot2::ggplot(x = Sepal.Length, y = Sepal.Width, data = iris, 
#'                 geom = "point") +
#' rphylopic::add_phylopic(mammals_pic, alpha = 1)
#' }

get_phylopic_image <- function(uuid, size = 512) {
  
  
  ## Check args ----
  
  if (missing(uuid)) {
    stop("You must provide an UUID (argument 'uuid')", call. = FALSE)
  }
  
  if (!is.character(uuid) || length(uuid) != 1) {
    stop("Argument 'uuid' must be a character of length 1", call. = FALSE)
  }
  
  if (!is.vector(size) || length(size) != 1) {
    stop("Argument 'size' must be a vector ('character' or 'integer') of ", 
         "length 1", call. = FALSE)
  }
  
  
  ## Send request ----
  
  base_url <- "https://api.phylopic.org/images/"
  
  page <- httr::GET(paste0(base_url, uuid))
  
  if (page$"status_code" != 200) {
    stop("The resource cannot be found", call. = FALSE)
  }
  
  
  ## Extract response ----
  
  content <- httr::content(page, as = "text", encoding = "UTF-8")
  content <- jsonlite::fromJSON(content)
  content <- content[[1]]
  
  images <- content$"rasterFiles"
  
  
  ## Get available sizes ----
  
  sizes <- unlist(lapply(strsplit(images$"sizes", "x"), function(x) x[1]))
  
  if (!(size %in% sizes)) {
    stop("Size not found. Available sizes are: '", 
         paste(sizes, collapse = "', '"), "'.", call. = FALSE)
  }
  
  
  ## Get image link ----
  
  link <- images[which(sizes == size), "href"]
  
  
  ## Download png ----
  
  filename <- paste0(tempfile(), ".png")
  utils::download.file(url = link, destfile = filename, quiet = TRUE)
  
  
  ## Import raster ----
  
  img <- png::readPNG(filename, native = FALSE)
  
  
  ## Convert to RGBA (without background color) ----
  
  if (dim(img)[3] == 2) {
    
    img_rgb <- array(1, dim = c(dim(img)[1], dim(img)[2], 4))
    
    img_rgb[ , , 1] <- ifelse(img[ , , 2] == 0, 1, 0) # red
    img_rgb[ , , 2] <- ifelse(img[ , , 2] == 0, 1, 0) # green
    img_rgb[ , , 3] <- ifelse(img[ , , 2] == 0, 1, 0) # blue
    img_rgb[ , , 4] <- ifelse(img[ , , 2] == 0, 0, 1) # alpha
    
  } else {
    
    img_rgb <- img
  }
  
  img_rgb
}
