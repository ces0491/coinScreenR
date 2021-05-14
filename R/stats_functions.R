#' calculate correlation using pairwise complete observations
#'
#' @param corr_data tbl_df - data for which you wish to determine correlations
#' @param corr_type string - one of total or roll
#' @param .roll_var_x string - x var in pairwise rolling correl
#' @param .roll_var_y string - y var in pairwise rolling correl
#' @param roll_period optional string indicating the number of periods to calculate the rolling correl
#'
#' @return cor_mx for corr_type = total or tbl_df for corr_type = roll
#' 
calc_correl <- function(corr_data, corr_type = c("total", "roll"), .roll_var_x = NULL, .roll_var_y = NULL, roll_period = NULL) {
  
  test_if_match <- c('variable', 'ticker', 'date') %in% names(corr_data)
  if (!all(test_if_match)) {
    stop("missing variable in corr_data")
  }
  
  reqd_data <- corr_data %>% 
    dplyr::filter(variable == 'close') %>% 
    dplyr::select(-variable) %>% 
    tidyr::spread(ticker, value)
  
  if (corr_type == "total") {
    
    corr_out <- reqd_data %>% 
      dplyr::select(-date) %>% 
      stats::cor(., use = "pairwise.complete.obs", method = "pearson")
    
  } else {
    if (is.null(roll_period)) {
      stop("You need to specify a rolling window to use corr_type 'roll'")
    }
    
    roll_var_x <- dplyr::sym(.roll_var_x)
    roll_var_y <- dplyr::sym(.roll_var_y)
    
    not_x <- setdiff(names(reqd_data)[-1], .roll_var_x)
    # not_x <- dplyr::enquos(.not_x)
  
    
    reqd_w_med <- reqd_data %>%
      tidyr::unite(value_vec, not_x, na.rm = TRUE, remove = FALSE, sep = ",") %>% 
      dplyr::group_by(date) %>% 
      dplyr::mutate(median = median(as.numeric(stringr::str_split(value_vec, ",")[[1]]))) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(-value_vec)
    
    roll_func <- tibbletime::rollify(
      ~stats::cor(.x, .y, use = "pairwise.complete.obs", method = "pearson"), 
      window = roll_period)
    
    corr_out <- reqd_w_med %>% 
      dplyr::mutate(roll_corr = roll_func(!!roll_var_x, !!roll_var_y)) %>%
      dplyr::mutate(roll_corr_v_median = roll_func(!!roll_var_x, median))
  }
  
  corr_out
}

