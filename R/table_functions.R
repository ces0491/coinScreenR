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

#################################################################################

format_pct <- function(value) {
  formatC(paste0(round(value * 100), "%"), width = 4)
}

format_usd <- function() {
  formatC(paste0("$", round(value)), width = 4)
}

make_color_pal <- function(colors, bias = 1) {
  get_color <- grDevices::colorRamp(colors, bias = bias)
  function(x) grDevices::rgb(get_color(x), maxColorValue = 255)
}

pct_color <- make_color_pal(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"), bias = 2)

build_overview_tbl <- function(crypto_config) {
  

  
  tbl <- reactable::reactable(
    reqd_data,
    pagination = TRUE,
    defaultSorted = "CMC Rank",
    defaultSortOrder = "asc",
    defaultColDef = reactable::colDef(class = "cell", headerClass = "header"),
    
    columns = list(
      Symbol = reactable::colDef(
        minWidth = 200,
        headerStyle = list(fontWeight = 700), 
        cell = function(value) {
          htmltools::div(
            class = "Symbol",
            htmltools::img(class = "logo", src = reqd_data[Symbol == value, logo]),
            htmltools::div(class = "Symbol-name", value)
          )
        }
      ),
      logo = reactable::colDef(show = FALSE)
    ),

    showSortIcon = FALSE,
    borderless = TRUE,
    class = "cryptoSumm-table"
  )
  
  tbl
  
}