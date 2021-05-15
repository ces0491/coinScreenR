
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

plot_top_n_words <- function(top_n_data) {
  
  ttl <- paste0("Top ", length(unique(top_n_data$word)), " words")
  
  p <- ggplot2::ggplot(data = top_n_data, ggplot2::aes(x = word, y = n))
  p <- p + ggplot2::geom_col()
  p <- p + ggplot2::xlab(NULL)
  p <- p + ggplot2::coord_flip()
  p <- p + ggplot2::theme_minimal()
  p <- p + ggplot2::labs(x = "",
                         y = "Unique Words",
                         title = ttl)
  
  p
}

plot_sentiment <- function(word_count_data) {
  
  p <- ggplot2::ggplot(data = word_count_data, ggplot2::aes(word, n, fill = sentiment))
  p <- p + ggplot2::geom_col(show.legend = FALSE)
  p <- p + ggplot2::facet_wrap(~sentiment, scales = "free_y")
  p <- p + ggplot2::labs(title = "Bing Classified Sentiment",
                         y = "Contribution to sentiment",
                         x = NULL)
  p <- p + ggplot2::coord_flip()
  
  p
}