#' @title to_daily
#'
#' @description Convert sub-daily data to daily periods
#'
#' @param mob data.frame, Combined mobility data.
#'
#' @return tibble
#' 
#' @export

to_daily <- function(mob){
  
  mob <- mob %>% 
    # Subset columns
    dplyr::select(date_time, 
           start_quadkey, 
           end_quadkey, 
           n_crisis, 
           n_baseline, 
           start_lat, 
           start_lon, 
           end_lat, 
           end_lon) %>% 
    # Remove hours from dates, concatenate start and end quadkeys to journeys
    dplyr::mutate(date = as.Date(date_time),
           journey = paste0(start_quadkey, '_', end_quadkey)) %>% 
    # Combine data by date and journey
    dplyr::group_by(date, journey) %>% 
    dplyr::summarise(start_quadkey = unique(start_quadkey),
              end_quadkey = unique(end_quadkey),
              n_crisis = sum(n_crisis, na.rm = TRUE),
              n_baseline = sum(n_baseline, na.rm = TRUE),
              start_lat = unique(start_lat),
              start_lon = unique(start_lon),
              end_lat = unique(end_lat),
              end_lon = unique(end_lon)) %>% 
    dplyr::ungroup() %>% 
    # Compute absolute and percent difference along each route
    dplyr::mutate(n_difference = n_crisis - n_baseline,
           perc_difference = (n_difference / n_baseline) * 100)
  
  return(mob)
  
}