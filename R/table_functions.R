build_summary_table <- function(crypto_config, tickers) {
  
  tbl <- crypto_config %>% 
    dplyr::filter(symbol %in% tickers) %>% 
    dplyr::select(cmc_rank, name, symbol, USD_price, USD_market_cap, USD_volume_24h, 
                  USD_percent_change_30d, USD_percent_change_60d, USD_percent_change_90d,
    ) %>% 
    dplyr::rename('Price' = 'USD_price', 
                  'Market Cap' = 'USD_market_cap', 
                  'Volume_24h' = 'USD_volume_24h',
                  'Rank_CMC' = 'cmc_rank',
                  'Name' = 'name',
                  'Symbol' = 'symbol',
                  'Change_30d' = 'USD_percent_change_30d',
                  'Change_60d' = 'USD_percent_change_60d', 
                  'Change_90d' = 'USD_percent_change_90d')
  
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
    DT::formatCurrency(columns = c('Market Cap', 'Volume_24h'),
                       currency = "$",
                       digits = 0,
                       mark = ",",
                       dec.mark = ".") %>%
    DT::formatPercentage(columns = c('Change_30d', 'Change_60d', 'Change_90d'),
                         digits = 2)
  
  dt_tbl
  
}