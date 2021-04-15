server.coinScreenR <- function(input, output, session) {
  
  # don't render outputs initially. a relevant reactive event specified later should trigger rendering
  output$searchTickerTbl <- DT::renderDT(NULL)
  output$summaryTbl <- renderTable(NULL)
  output$descriptionTxt <- renderText(NULL)
  output$tickerTS <- plotly::renderPlotly(NULL)
  
  observeEvent(input$freqSelect, {
    
    forc_periods <- dplyr::case_when(input$freqSelect == "daily" ~ "Days",
                      input$freqSelect == "weekly" ~ "Weeks",
                      input$freqSelect == "monthly" ~ "Months",
                      input$freqSelect == "quarterly" ~ "Quarters")
    
    updateNumericInput(inputId = "forcHorizon", label = paste0("Forecast Horizon (", forc_periods, ")"))
  })
  
  # create a character vector of tickers
  tickers <- eventReactive(input$submitBtn, {
    c(input$tickerSelect, input$tickerCompare)
  })
  
  # get meta data
  crypto_meta <- eventReactive(input$submitBtn, {
    coinmarketcapr::get_crypto_meta(tickers())
  })
  
  
#################################### Sidebar ####################################
  
  # render description for the selected ticker
  desc_text <- reactive({
    crypto_meta() %>%
      dplyr::filter(symbol == input$tickerSelect) %>% 
      dplyr::pull(description)
  })
  output$descriptionTxt <- renderText({paste0('<p style="text-align:justify;font-size:14px;">',desc_text(),'</p>')})
  
  
#################################### Main Overview Page ####################################
  
  # retrieve live crypto price data
  data <- eventReactive(input$submitBtn, {
    
    ticker_xs <- paste0(tickers(), input$tickerBase) 
   
    start_dt <- input$dateRange[1]
    end_dt <- input$dateRange[2]
    freq <- input$freqSelect
    
    future_promise({fdoR::get_crypto_data(ticker = ticker_xs, 
                                          start_date = start_dt, 
                                          end_date = end_dt, 
                                          frequency = freq)
    })
  })
 
  # prepare data for timeseries plot
  ts_plot_data <- reactive({
    
    data() %...>%
      dplyr::filter(variable == "close") %...>%
      dplyr::select(ticker, date, variable, value) %...>%
      prepare_ts_data(., input$freqSelect)
    
  })
  
  # render timeseries plot of prices
  output$tickerTS <- plotly::renderPlotly({
    
    ttl <- "Historical Price Timeseries"
    x <- list(title = "")
    y <- list(title = "")
    
    ts_plot_data() %...>%
      plotly::plot_ly(., x = ~date, y = ~plot_value, mode = 'lines', linetype = ~ticker,
                      text = ~ticker,
                      hovertemplate = paste0("<b>%{text}</b><br>",
                                             "Date: %{x}<br>",
                                             "Price: %{y}<br>",
                                             "<extra></extra>")
      ) %...>% 
      plotly::layout(title = ttl, xaxis = x, yaxis = y) %...>%
      plotly::rangeslider(start = input$dateRange[1], end = input$dateRange[2])
    
  })
  
  summ_data <- eventReactive(input$submitBtn, {
    crypto_config %>% 
      dplyr::filter(symbol %in% c(input$tickerSelect, input$tickerCompare)) %>% 
      dplyr::select(name, symbol, USD_price, USD_market_cap, USD_volume_24h, 
                    USD_percent_change_90d, USD_percent_change_60d, USD_percent_change_30d,
                    cmc_rank)
  })
  
  # render summary table
  output$summaryTbl <- renderTable({summ_data()},
                                   striped = TRUE,
                                   hover = TRUE,
                                   bordered = TRUE, 
                                   width = '100%', 
                                   align = 'l',
                                   digits = 2,
                                   rownames = FALSE,
                                   colnames = TRUE)
  
  
#################################### Analysis Page ####################################
  
  # prepare data for forecasting
  reqd_analysis_data <- reactive({
    
    data() %...>% 
      dplyr::filter(ticker == input$tickerSelect) %...>% 
      dplyr::filter(variable == "close") %...>% 
      dplyr::select(ticker, date, value)
  })
  
  split_data <- reactive({
    
    reqd_analysis_data() %...>% 
      timetk::time_series_split(date_var = date, assess = input$forcHorizon, cumulative = TRUE)
  })
  
  recipe <- reactive({
    
    recipes::recipe(value ~ date, rsample::training(split_data())) %...>% 
      timetk::step_timeseries_signature(date) %...>% 
      recipes::step_rm(tidyselect::matches("(.iso$)|(.xts$)|(day)|(hour)|(minute)|(second)|(am.pm)")) %...>% 
      recipes::step_normalize(Date_index.num, Date_month) %...>%
      recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)
  })
  
  models <- eventReactive(input$submitBtn, {
    future_promise({fit_forecasting_models(split_data = split_data(), recipe = recipe())
    })
  })
  
  calibration_tbl <- reactive({
    models() %...>%
      modeltime::modeltime_calibrate(rsample::testing(split_data()))
  })
  
  # prepare data for fair value analysis
  
  # render  plot of forecast
  output$plotForcast <- plotly::renderPlotly({
    
    calibration_tbl() %>%
      modeltime::modeltime_forecast(actual_data = reqd_analysis_data) %>%
      modeltime::plot_modeltime_forecast(.interactive = TRUE)
    
  })
  
  # plot FV boxplot
  
  # render summary stat table
   
  
#################################### Crypto Lookup Page ####################################
  
  # render search table
  output$searchTickerTbl <- DT::renderDT({
    crypto_config %>% 
      dplyr::select(name, symbol, slug) %>% 
      dplyr::rename(Name = name,
                    Symbol = symbol,
                    Slug = slug) %>% 
      DT::datatable(., rownames = FALSE)
  })
   
}
