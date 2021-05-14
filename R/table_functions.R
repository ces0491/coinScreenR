build_compare_tbl <- function(crypto_config, tickers) {
  
  tbl <- crypto_config %>% 
    dplyr::filter(symbol %in% tickers) %>% 
    dplyr::select(cmc_rank, name, symbol, USD_price, USD_market_cap, USD_volume_24h, 
                  USD_percent_change_24h, USD_percent_change_7d, USD_percent_change_30d,
    ) %>% 
    dplyr::mutate(USD_percent_change_24h = USD_percent_change_24h/100,
                  USD_percent_change_7d = USD_percent_change_7d/100,
                  USD_percent_change_30d = USD_percent_change_30d/100) %>% 
    dplyr::rename('Price' = 'USD_price', 
                  'Market Cap' = 'USD_market_cap', 
                  'Volume 24h' = 'USD_volume_24h',
                  'CMC Rank' = 'cmc_rank',
                  'Name' = 'name',
                  'Symbol' = 'symbol',
                  'Change 24h' = 'USD_percent_change_24h',
                  'Change 7d' = 'USD_percent_change_7d', 
                  'Change 30d' = 'USD_percent_change_30d')
  
  dt_tbl <- DT::datatable(tbl, 
                          options = list(scrollX = TRUE,
                                         paging = FALSE,
                                         pageLength = 10),
                          rownames = FALSE, 
                          filter = 'none') %>% 
    DT::formatCurrency(columns = c('Price'),
                       currency = "$",
                       digits = 2,
                       mark = ",",
                       dec.mark = ".") %>% 
    DT::formatCurrency(columns = c('Market Cap', 'Volume 24h'),
                       currency = "$",
                       digits = 0,
                       mark = ",",
                       dec.mark = ".") %>%
    DT::formatPercentage(columns = c('Change 24h', 'Change 7d', 'Change 30d'),
                         digits = 2)
  
  dt_tbl
  
}

build_overview_tbl <- function(crypto_config) {
  
}