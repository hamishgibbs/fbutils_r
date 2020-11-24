#' @title read_mob
#'
#' @description Read csv file of mobility data
#'
#' @param path string, path to mobility file
#' @param col_types function, column data type mapping. Defaults to dplyr::cols
#' @param zoom_level integer, dataset zoom level. Defaults to 12
#'
#' @return tibble
#' 
#' @export

read_mob <- function(path, col_types = NULL, zoom_level = 12){
  
  # Guess column types if none are provided
  if (is.null(col_types)){
    
    col_types <- readr::cols()
    
  }
  
  # Read mobility data
  mob <- readr::read_csv(path, col_types = col_types)

  # Pad quadkeys with leading zeroz  
  mob <- mob %>% 
    dplyr::mutate(start_quadkey = stringr::str_pad(start_quadkey, zoom_level, pad = "0"),
                  end_quadkey = stringr::str_pad(end_quadkey, zoom_level, pad = "0"))
  
  return(mob)
  
}