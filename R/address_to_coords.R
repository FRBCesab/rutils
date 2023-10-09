#' Get coordinates of any location in the World
#'
#' @description 
#' Retrieves geographic coordinates (longitude and latitude) of any location in
#' the World using the OpenStreetMap Data Search Engine Nominatim
#' \url{http://nominatim.openstreetmap.org}.
#'
#' @param address a `character` of length 1. The location for which coordinates 
#'   will be found.
#'
#' @export
#'
#' @return A `data.frame` with two columns: `lon`, the longitude and `lat`, 
#' the latitude of the location.
#'
#' @examples
#' address_to_coords("Houston, TX, USA")
#' address_to_coords("5 rue de l'école de Médecine, Montpellier, France")


address_to_coords <- function(address) {
  
  ## Check args ----
  
  if (missing(address)) {
    stop("Argument 'address' is required", call. = FALSE)
  }
  
  if (is.null(address)) { 
    stop("Argument 'address' cannot be NULL", call. = FALSE)
  }
  
  if (!is.character(address)) {
    stop("Argument 'address' must be character", call. = FALSE)
  }
  
  if (length(address) != 1) {
    stop("Argument 'address' must be character of length 1", call. = FALSE)
  }
  
  
  ## API URL ----
  
  api_url <- "https://nominatim.openstreetmap.org/search"

  
  ## Encode search terms ----
  
  address  <- utils::URLencode(address)
  
  
  ## Build request ----
  
  full_url <- paste0(api_url, "?q=", address, "&format=json&limit=1")
  
  
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
    
    return(data.frame("lon" = numeric(0),
                      "lat" = numeric(0)))
    
  } else {
    
    return(data.frame("lon" = as.numeric(results$"lon"),
                      "lat" = as.numeric(results$"lat")))
  }
}
