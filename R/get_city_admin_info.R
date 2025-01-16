#' Retrieve French city INSEE code
#' 
#' @description
#' Uses the Nominatim API <https://nominatim.org/release-docs/develop/api/Search/> 
#' to retrieve the _Code officiel geographique (COG)_ and the _Code postal_ of a
#' French town.
#'
#' @param city a `character` of length 1.
#' 
#' @param county (optional) a `character` of length 1. The county (departement)
#'   of the city used to refine the query.
#'   
#' @param state (optional) a `character` of length 1. The state (region)
#'   of the city used to refine the query.
#' 
#' @param limit an `integer` of length 1. The number of results to return.
#'   Default is `1`.
#'
#' @returns A `data.frame` with the following variables:
#'   - `input`: the value of the argument `city`
#'   - `village`: the name of the `city` in Open Street Map database
#'   - `city`: the name of the `city` in Open Street Map database
#'   - `town`: the name of the `city` in Open Street Map database
#'   - `municipality`: the name of the `city` in Open Street Map database
#'   - `county`: the name of the French departement
#'   - `state`: the name of the French region
#'   - `region`: the name of the region of France
#'   - `country`: the name of the region of France
#'   - `postcode`: the _Code postal_
#'   - `code_insee`: the _Code officiel geographique_
#'   - `longitude`: the longitude of the administrative center of the city
#'   - `latitude`: the latitude of the administrative center of the city
#'
#' @export
#'
#' @examples
#' get_city_admin_info(city = "Florac")
#' get_city_admin_info(city = "La Salle-Prunet")
#' get_city_admin_info(city = "Florac-Trois-Rivieres")

get_city_admin_info <- function(city, county = NULL, state = NULL, limit = 1L) {
  
  # Check input ----
  
  if (missing(city)) {
    stop("Argument 'city' is required")
  }
  
  if (!is.character(city)) {
    stop("Argument 'city' must be a character")
  }
  
  if (length(city) != 1) {
    stop("Argument 'city' must be a character of length 1")
  }
  
  if (!is.null(county)) {
    
    if (!is.character(county)) {
      stop("Argument 'county' must be a character")
    }
    
    if (length(county) != 1) {
      stop("Argument 'county' must be a character of length 1")
    }
    
    if (is.na(county)) {
      stop("Argument 'county' cannot contain missing value")
    }
  }
  
  if (!is.null(state)) {
    
    if (!is.character(state)) {
      stop("Argument 'state' must be a character")
    }
    
    if (length(state) != 1) {
      stop("Argument 'state' must be a character of length 1")
    }
    
    if (is.na(state)) {
      stop("Argument 'state' cannot contain missing value")
    }
  }
  
  if (!is.numeric(limit)) {
    stop("Argument 'limit' must be an integer")
  }
  
  if (length(limit) != 1) {
    stop("Argument 'limit' must be an integer of length 1")
  }
  
  if (is.na(limit)) {
    stop("Argument 'limit' cannot contain NA value")
  }
  
  if (limit <= 0) {
    stop("Argument 'limit' must be strictly positive")
  }
  
  
  # Nominatim API endpoint ----
  
  endpoint <- "https://nominatim.openstreetmap.org/search"
  
  
  # Prepare the HTTP request ----
  
  http_request <- httr2::request(endpoint)
  
  
  # Append request parameters ----
  
  http_request <- http_request |> 
    httr2::req_url_query(city    = city) |> 
    httr2::req_url_query(country = "France")
  
  if (!is.null(county)) {
    
    http_request <- http_request |> 
      httr2::req_url_query(county = county)
  }
  
  if (!is.null(state)) {
    
    http_request <- http_request |> 
      httr2::req_url_query(state = state)
  }
  
  http_request <- http_request |> 
    httr2::req_url_query(format = "json") |> 
    httr2::req_url_query(addressdetails = 1) |> 
    httr2::req_url_query(extratags = 1) |> 
    httr2::req_url_query(limit = limit)
  
  
  # Send HTTP request  ----
  
  http_response <- httr2::req_perform(http_request)
  
  
  # Check response status ----
  
  httr2::resp_check_status(http_response)
  
  
  # Parse response content ----
  
  content <- httr2::resp_body_json(http_response)
  
  
  # Clean output ----
  
  template <- data.frame("input"        = city,
                         "village"      = NA,
                         "city"         = NA,
                         "town"         = NA,
                         "municipality" = NA,
                         "county"       = NA,
                         "state"        = NA,
                         "region"       = NA,
                         "country"      = NA,
                         "postcode"     = NA,
                         "code_insee"   = NA,
                         "longitude"    = NA,
                         "latitude"     = NA)
  
  osm_info <- data.frame()
  
  if (length(content) > 0) {
    
    results <- min(c(limit, length(content)))
    
    content <- content[1:results]
    
    for (i in 1:results) {
      
      tmp <- template
      
      for (j in 2:10) {
        
        if (!is.null(content[[i]]$"address"[[colnames(tmp)[j]]])) {
          tmp[1, j] <- content[[i]]$"address"[[colnames(tmp)[j]]]
        }
      }
      
      if (!is.null(content[[i]]$"extratags"$"ref:INSEE")) {
        tmp[1, "code_insee"] <- content[[i]]$"extratags"$"ref:INSEE"
      }
      
      if (!is.null(content[[i]]$"lon")) {
        tmp[1, "longitude"] <- content[[i]]$"lon"
      }
      
      if (!is.null(content[[i]]$"lat")) {
        tmp[1, "latitude"] <- content[[i]]$"lat"
      }
      
      osm_info <- rbind(osm_info, tmp)
    }
    
    osm_info$"longitude" <- as.numeric(osm_info$"longitude")
    osm_info$"latitude"  <- as.numeric(osm_info$"latitude")
    
  } else {
    
    osm_info <- template
  }
  
  osm_info
}
