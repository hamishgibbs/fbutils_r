#' @title get_journey_lines
#'
#' @description Get a spatial reference of all joureys in a mobility dataset
#'
#' @param mob data.frame, Combined mobility data.
#'
#' @return tibble
#' 
#' @export

get_journey_lines <- function(mob){
  
  ref <- mob %>% 
    dplyr::mutate(journey = paste0(start_quadkey, '_', end_quadkey)) %>% 
    dplyr::select(journey, geometry) %>% 
    distinct()
  
  ref <- sf::st_as_sf(ref, wkt = 'geometry') %>% 
    sf::st_set_crs(4326)
  
  return(ref)
  
}
