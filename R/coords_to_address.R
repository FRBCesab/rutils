#' Retrieve OSM address from coordinates
#'
#' @description 
#' Retrieves any location name in the World from geographic coordinates 
#' (longitude and latitude) using the OpenStreetMap Data Search Engine Nominatim
#' \url{http://nominatim.openstreetmap.org}.
#'
#' @param coords a `numeric` of length 2. The first element is the `latitude` 
#'   and the second the `longitude`.
#'
#' @export
#'
#' @return A `character` of length 1 with the OSM name of the location.
#'
#' @examples
#' coords_to_address(c(43.61277, 3.873486))
#' coords_to_address(c(29.75894, -95.3677))

coords_to_address <- function(coords) {
  
  ## Check args ----
  
  if (missing(coords)) {
    stop("Argument 'coords' is required", call. = FALSE)
  }
  
  if (is.null(coords)) { 
    stop("Argument 'coords' cannot be NULL", call. = FALSE)
  }
  
  if (!is.numeric(coords)) {
    stop("Argument 'coords' must be numeric", call. = FALSE)
  }
  
  if (length(coords) != 2) {
    stop("Argument 'coords' must be numeric of length 2", call. = FALSE)
  }
  
  if (coords[1] > 90 || coords[1] < -90) {
    stop("Latitude must be the first element of 'coords'", call. = FALSE)
  }
  
  
  ## API URL ----
  
  api_url <- "https://nominatim.openstreetmap.org/reverse"
  
  
  ## Build request ----
  
  full_url <- paste0(api_url, "?lat=", coords[1], "&lon=", coords[2], 
                     "&format=json&limit=1")
  
  
  ## Send request ----
  
  results <- httr::GET(full_url)
  
  
  ## Check status code -----
  
  if (results$"status_code" != 200) {
    stop("An error with the Nominatim API occurred", call. = FALSE)
  }
  
  
  ## Parse results ----
  
  results <- httr::content(results, as = "text")
  results <- jsonlite::fromJSON(results)
  
  
  ## Clean results ----
  
  if (length(results) == 0) {
    
    address <- NULL
    
  } else {
    
    address <- results$"display_name"
      
  }
  
  address
}
