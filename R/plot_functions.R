
prepare_ts_data <- function(price_df) {
  
  n_tkr <- dplyr::n_distinct(price_df$ticker)
  
  if(n_tkr > 1) {
    
    start_dts <- price_df %>% 
      dplyr::group_by(ticker) %>% 
      dplyr::summarise(min(date), .groups = "drop")
    
    min_dt <- max(start_dts[,2][[1]])
    
    end_dts <- price_df %>% 
      dplyr::group_by(ticker) %>% 
      dplyr::summarise(max(date), .groups = "drop")
    
    max_dt <- min(end_dts[,2][[1]])
    
    clean_dts_df <- price_df %>% 
      dplyr::group_by(ticker) %>% 
      dplyr::arrange(date) %>% 
      dplyr::filter(date >= min_dt,
                    date<= max_dt) %>% 
      dplyr::ungroup()
      
    result <- clean_dts_df %>% 
      dplyr::group_by(ticker) %>% 
      dplyr::mutate(start_val = ifelse(date == min(date), value, NA)) %>% 
      tidyr::fill(start_val) %>% 
      dplyr::mutate(plot_value = (value / start_val) * 100) %>% 
      dplyr::mutate(change = (value / dplyr::lag(value))-1) %>%
      dplyr::ungroup()  
    
  } else {
    
    result <- price_df %>% 
      dplyr::group_by(ticker) %>% 
      dplyr::mutate(change = (value / dplyr::lag(value))-1) %>%
      dplyr::ungroup() %>% 
      dplyr::rename(plot_value = value)
  }
  
  return(result)
}