#' Get crypto data from Binance
#'
#' @param tickers a character vector of crypto crosses, e.g. c('BTCBUSD', 'ETHUSDT')
#' @param start_date start date
#' @param end_date end date
#' @param frequency string indicating the frequency to retrieve data
#'
#' @return tbl_df
#' 
get_crypto_data <- function(tickers, start_date, end_date, frequency = c("12h", "1d", "1w", "1M")) {
  
  # binancer supports high frequency data, up to minutely, however we limit to daily frequency here
  
  ticker_list <- list()
  
  for(ticker in tickers) {
    
    t <- which(tickers == ticker)
    progress <- round(t/length(tickers), 2) * 100
    print(glue::glue("Attempting to retrieve {ticker} data from Binance"))
    
    crypto_data <- try(
      binancer::binance_klines(symbol = ticker,
                               interval = frequency,
                               start_time = start_date,
                               end_time = end_date),
      silent = FALSE)
    
    if(any(class(crypto_data) == "try-error")) {
      
      crypto_data <- NULL
      
    } else {
      test_if_match <- c('symbol', 'close_time', 'open', 'high', 'low', 'close', 'volume') %in% names(crypto_data)
      if (!all(test_if_match)) {
        stop("missing variable in crypto data")
      }
    }
    
    print(glue::glue("{progress}% complete"))
    
    ticker_list[[ticker]] <- crypto_data
  }
  
  # convert list of data.frames to tibble and select required variables
  crypto_data_tbl <- ticker_list %>%
    tibble::enframe() %>%
    tidyr::unnest(value) %>%
    dplyr::select(symbol, close_time, open, high, low, close, volume) %>%
    dplyr::rename(ticker = symbol,
                  date = close_time) %>%
    dplyr::mutate(date = as.Date(date))
  
  # make tibble long and convert frequency
  crypto_data_long <- crypto_data_tbl %>%
    tidyr::gather(variable, value, -ticker, -date) %>%
    dplyr::group_by(ticker) %>%
    dplyr::ungroup() %>%
    tibble::as_tibble()
  
  crypto_data_long
}

# commented out to remove webScrapeR dependency
# uncomment and run to update ratings data. In theory ratings shouldn't change too frequently

# get_crypto_rating <- function() {
#   
#   conn <- webScrapeR::connect_session('https://tokeninsight.com/cryptocurrencies')
#   tokeninsight <- webScrapeR::scrape_table(conn$session, xpath = '//*[@id="container"]/div/div[3]/div[2]/div[1]/div[3]/div/div[2]/table')
#   
#   renamed_df <- tokeninsight %>% 
#     dplyr::select(-X11, -X12) %>% 
#     dplyr::rename(Rank = X1, 
#                   Name = X2, 
#                   Token = X3, 
#                   Rating = X4, 
#                   Price = X5, 
#                   Change_24H = X6, 
#                   Change_7D = X7, 
#                   Circulating_Supply = X8, 
#                   Volume_24h = X9,
#                   Market_Cap = X10)
#   
#   reqd_df <- renamed_df %>% 
#     dplyr::select(Token, Rating) %>% 
#     dplyr::rename(symbol = Token)
#   
#   saveRDS(reqd_df, './inst/extdata/tokeninsightRating.rds')
#   
#   return(reqd_df)
# }