server.coinScreenR <- function(input, output, session) {
  
  # don't render outputs initially. a relevant reactive event specified later should trigger rendering
  output$searchTickerTbl <- DT::renderDT(NULL)
  output$summaryTbl <- renderTable(NULL)
  output$descriptionTxt <- renderText(NULL)
  output$tickerTS <- plotly::renderPlotly(NULL)
  output$recent_tweets <- DT::renderDataTable(NULL)
  output$most_popular_tweets <- DT::renderDataTable(NULL)
  output$most_retweeted <- DT::renderDataTable(NULL)
  output$cmcWidget <- renderUI(NULL)
  output$cmcWidget2 <- renderUI(NULL)
  
  output$plotForcast <- plotly::renderPlotly(NULL)
  output$analysisTbl <- renderDataTable(NULL)
  output$fvBox <- plotly::renderPlotly(NULL)
  
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
    
    ttl <- ""
    x <- list(title = "")
    y <- list(title = "")
    slider_min <- min(ts_plot_data()$date)
      
    ts_plot_data() %...>%
      plotly::plot_ly(., x = ~date, y = ~plot_value, mode = 'lines', linetype = ~ticker,
                      text = ~ticker,
                      hovertemplate = paste0("<b>%{text}</b><br>",
                                             "Date: %{x}<br>",
                                             "Price: %{y}<br>",
                                             "<extra></extra>")
      ) %...>% 
      plotly::layout(title = ttl, xaxis = x, yaxis = y) %...>%
      plotly::rangeslider(start = slider_min, end = input$dateRange[2])
    
  })
  
  summ_data <- eventReactive(input$submitBtn, {
    build_summary_table(crypto_config, tickers = c(input$tickerSelect, input$tickerCompare))
  })
  
  # render summary table
  output$summaryTbl <- DT::renderDataTable({summ_data()})
  
  # render coin market cap ticker widget
  html_string <- eventReactive(input$submitBtn, {
    
    ccy_id <- crypto_config %>% dplyr::filter(symbol == input$tickerSelect) %>% dplyr::pull(id)
    
    widget_str <- glue::glue('<script type="text/javascript" src="https://files.coinmarketcap.com/static/widget/currency.js">
                            </script><div class="coinmarketcap-currency-widget" 
                            data-currencyid={as.character(ccy_id)} 
                            data-base="ZAR" 
                            data-secondary="USD" 
                            data-ticker="true" 
                            data-rank="true" 
                            data-marketcap="true" 
                            data-volume="true" 
                            data-statsticker="true" 
                            data-stats="USD">
                            </div>')
    
    widget_str
  })
  
  output$cmcWidget <- renderUI({
    tags$div(HTML(html_string()))
  })
  
  html_string_w2 <- eventReactive(input$submitBtn, {
    
    ccy_id <- paste(
      crypto_config %>% dplyr::filter(symbol %in% c(input$tickerSelect, input$tickerCompare)) %>% dplyr::pull(id), 
      collapse = ",")
    
    widget_str2 <- glue::glue('<script type="text/javascript" src="https://files.coinmarketcap.com/static/widget/coinPriceBlock.js">
                             </script><div id="coinmarketcap-widget-coin-price-block" 
                             coins={ccy_id} 
                             currency="USD" 
                             theme="light" 
                             transparent="true" 
                             show-symbol-logo="true">
                             </div>')
    
    widget_str2
  })
  
  output$cmcWidget2 <- renderUI({
    tags$div(HTML(html_string_w2()))
  })
  
  ########### twitter stuff
  
  tweet_df <- eventReactive(input$submitBtn, {
    
    slug <- crypto_config %>%
      dplyr::filter(symbol == input$tickerSelect) %>% 
      dplyr::pull(slug)
      
    query <- paste0(input$tickerSelect, " OR ", slug)
    
    withProgress(min = 0, max = 1, value = 0.2, message = "updating tweets", {
      rtweet::search_tweets(q = query, 
                            n = 250, 
                            include_rts = TRUE,
                            filter = "verified", 
                            lang = "en",
                            token = readRDS(Sys.getenv("TWITTER_PAT")))
      })
    })
  
  # get ids of most liked, retweeted and top n
  most_popular_tweets <- reactive({most_popular(tweet_df())})
  most_retweeted_tweets <- reactive({most_retweeted(tweet_df())})
  recent_tweets <- reactive({most_recent(tweet_df())})
  
  tibblefy_tweet <- function(id) {
    n <- length(id)
    shiny::withProgress(min = 0, max = n, value = 0, message = "extract tweets", {

      tbl <- tibble::tibble(
        tweet = purrr::map(id, ~ {
          res <- embed_tweet(.)
          shiny::incProgress(amount = 1)
          res
        })
      )
    
    twt_dt <- DT::datatable(tbl, options = list(lengthMenu = c(1, 3, 5, 10)))
    })
    
    twt_dt
  }
  
  output$most_popular_tweets <- DT::renderDataTable(
    tibblefy_tweet(most_popular_tweets())
    )
  output$most_retweeted <- DT::renderDataTable(
    tibblefy_tweet(most_retweeted_tweets())
    )
  output$recent_tweets <- DT::renderDataTable(
    tibblefy_tweet(recent_tweets())
    )
  
#################################### Analysis Page ####################################
  
  # prepare data for forecasting
  reqd_analysis_data <- reactive({
    data() %...>% 
      dplyr::filter(ticker == input$tickerSelect) %...>% 
      dplyr::filter(variable == "close") %...>% 
      dplyr::select(ticker, date, value)
  })
  
  split_data <- eventReactive(input$submitBtn,{
    reqd_analysis_data() %...>% 
      timetk::time_series_split(date_var = date, assess = input$forcHorizon, cumulative = TRUE)
  })
  
  recipe <- reactive({
      recipes::recipe(value ~ date, rsample::training(split_data())) %...>% 
      timetk::step_timeseries_signature(date) %...>% 
      recipes::step_rm(tidyselect::matches("(.iso$)|(.xts$)|(day)|(hour)|(minute)|(second)|(am.pm)")) %...>% 
      recipes::step_normalize(date_index.num, date_month) %...>%
      recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)
  })
  
  models <- eventReactive(input$submitBtn,{
    future_promise({
      fit_forecasting_models(split_data = split_data(), recipe = recipe())
    })
  })
  
  calibration_tbl <- eventReactive(input$submitBtn,{
    models() %...>%
      modeltime::modeltime_calibrate(rsample::testing(split_data()))
  })
  
  
  mdl_forc <- reactive({
    calibration_tbl() %...>%
      modeltime::modeltime_forecast(actual_data = reqd_analysis_data())
  })
  # prepare data for fair value analysis
  
  # render  plot of forecast
  output$plotForcast <- plotly::renderPlotly({mdl_forc() %...>% modeltime::plot_modeltime_forecast(.)})
  
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
      DT::datatable(., rownames = FALSE,
                    options = list(lengthMenu = c(5, 10, 20, 50), pageLength = 20))
  })
   
}
