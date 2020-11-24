#' @title check_date_range
#'
#' @description Check that all dates are present in a dataset given a certain interval
#'
#' @param data data.frame, data. Must contain a `date` column.
#' @param interval integer, time interval between datasets in hours
#' @param error boolean, pass TRUE to raise an error for missing dates
#'
#'
#' @return tibble
#' 
#' @export

check_date_range <- function(data, interval = 8, error = FALSE){
  
  min_date <- min(data$date)
  max_date <- max(data$date)
  
  d <- min_date
  
  expected_dates <- c()
  
  while(d <= max_date){
    
    expected_dates <- append(expected_dates, d)
    
    d <- add_hrs(d, interval)
    
  }
  
  res <- data %>% 
    dplyr::filter(!date %in% expected_dates)
  
  if (nrow(res) > 0){
    
    message <- 
    
    if (error){
      
      stop('Some dates are missing: ', unique(res$date))
      
    } else {
      
      warning('Some dates are missing: ', unique(res$date))
      
    }
  }
  
}

add_hrs <- function(x, interval){
  return(x + (interval * 60 * 60))
}