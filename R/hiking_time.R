#' Hiking time calculation
#' 
#' @description
#' Estimates the hiking time for a planned route by considering horizontal 
#' distance, elevation gain and elevation loss.
#' 
#' This function implements the DIN 33466 of the German Alpine Club. Different 
#' speeds are assumed according to this standard:
#' - 4 kilometers per hour in the horizontal direction
#' - 300 vertical meters in the ascent per hour
#' - 500 vertical meters in the descent per hour
#'
#' @param distance a `numeric` of length 1. The total distance of the hike 
#'   (in kilometers). Must be strictly positive.
#' 
#' @param elevation_gain a `numeric` of length 1. The total elevation gain 
#'   (cumulative positive elevation). Must be positive or zero.
#' 
#' @param elevation_loss a `numeric` of length 1. The total elevation loss 
#'   (cumulative negative elevation). Must be positive or zero.
#'
#' @return A `character` of length 1 returning the hiking time as `"23h59"`.
#' 
#' @export
#'
#' @examples
#' ## Lac du Montagnon par le Col d’Iseye 
#' ## https://www.visorando.com/randonnee-lac-du-montagnon-par-le-col-d-iseye/
#' 
#' hiking_time(distance = 20, elevation_gain = 1460, elevation_loss = 1460)

hiking_time <- function(distance, elevation_gain, elevation_loss) {
  
  # Check args ----
  
  if (missing(distance)) {
    stop("Argument 'distance' is required", call. = FALSE)
  }
  
  if (!is.numeric(distance) || length(distance) != 1) {
    stop("Argument 'distance' must be a numeric of length 1", call. = FALSE)
  }
  
  if (distance <= 0) {
    stop("Argument 'distance' must be a positive number", call. = FALSE)
  }
  
  if (missing(elevation_gain)) {
    stop("Argument 'elevation_gain' is required", call. = FALSE)
  }
  
  if (!is.numeric(elevation_gain) || length(elevation_gain) != 1) {
    stop("Argument 'elevation_gain' must be a numeric of length 1", 
         call. = FALSE)
  }
  
  if (elevation_gain < 0) {
    stop("Argument 'elevation_gain' must be a positive number", call. = FALSE)
  }
  
  if (missing(elevation_loss)) {
    stop("Argument 'elevation_loss' is required", call. = FALSE)
  }
  
  if (!is.numeric(elevation_loss) || length(elevation_loss) != 1) {
    stop("Argument 'elevation_loss' must be a numeric of length 1", 
         call. = FALSE)
  }
  
  if (elevation_loss < 0) {
    stop("Argument 'elevation_loss' must be a positive number", call. = FALSE)
  }
  
  
  # Time required for horizontal distance ----
  
  time_distance <- distance / (4 / 60)
  
  
  # Times required for vertical distance ----
  
  time_slope_up <- elevation_gain * 60 / 300
  time_slope_do <- elevation_loss * 60 / 500
  
  time_slope <- time_slope_up + time_slope_do
  
  
  # Get minimum time ----
  
  times <- c(time_distance, time_slope)
  pos   <- which.min(times)
  
  
  # Compute total time (numeric) ----
  
  time_num <- round((times[pos] / 2 + times[-pos]) / 60, 2)
  time_num <- format(c(time_num, 1.11))[1]
  time_num <- as.numeric(strsplit(time_num, "\\.")[[1]])
  
  
  # Extract hours ----
  
  time_hr  <- time_num[1]
  
  
  # Extract minutes -----
  
  time_mn  <- time_num[2]
  time_mn  <- round(time_mn * 60 / 100)
  time_mn  <- format(c(time_mn, 11))[1]
  time_mn  <- gsub("\\s", "0", time_mn)
  
  
  # Output ----
  paste0(time_hr, "h", time_mn)
}
