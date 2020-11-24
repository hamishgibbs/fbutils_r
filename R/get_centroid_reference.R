#' @title get_centroid_reference
#'
#' @description Get a reference of all tile centroids in a mobility dataset
#'
#' @param mob data.frame, Combined mobility data.
#' @param sf boolean, Return an sf object? Defaults to FALSE
#'
#' @return tibble
#' 
#' @export

get_centroid_reference <- function(mob, sf = FALSE){
  
  start <- mob %>% 
    dplyr::select(start_quadkey, start_lat, start_lon) %>% 
    dplyr::rename(quadkey = start_quadkey,
           lat = start_lat, 
           lon = start_lon)
  
  end <- mob %>% 
    dplyr::select(end_quadkey, end_lat, end_lon) %>% 
    dplyr::rename(quadkey = end_quadkey,
           lat = end_lat, 
           lon = end_lon)
  
  ref <- rbind(start, end) %>% 
    distinct()
  
  if (sf){
    ref <- sf::st_as_sf(ref, coords = c("lon", "lat")) %>% 
      sf::st_set_crs(4326)
  }
  
  return(ref)
  
}