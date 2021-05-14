server.coinScreenR <- function(input, output, session) {
  
  # don't render outputs initially. a relevant reactive event specified later should trigger rendering
  output$searchTickerTbl <- DT::renderDT(NULL)
  output$summaryTbl <- renderTable(NULL)
  output$descriptionTxt <- renderText(NULL)
  output$tickerTS <- plotly::renderPlotly(NULL)
  
  output$recent_tweets <- DT::renderDataTable(NULL)
  output$most_popular_tweets <- DT::renderDataTable(NULL)
  output$most_retweeted <- DT::renderDataTable(NULL)
  output$most_used_words <- DT::renderDataTable(NULL)
  output$neg_pos_word_count <- DT::renderDataTable(NULL)
  
  output$cmcWidget <- renderUI(NULL)
  output$correl_mx <- plotly::renderPlotly(NULL)
  output$correl_ts <- plotly::renderPlotly(NULL)
  
  output$plotForcast <- plotly::renderPlotly(NULL)
  # output$analysisTbl <- renderDataTable(NULL)
  # output$fvBox <- plotly::renderPlotly(NULL)
  
  observeEvent(input$freqSelect, {
    
    forc_periods <- dplyr::case_when(input$freqSelect == "1d" ~ "Days",
                                     input$freqSelect == "1w" ~ "Weeks",
                                     input$freqSelect == "1M" ~ "Months")
    
    updateNumericInput(inputId = "forcHorizon", label = paste0("Forecast Horizon (", forc_periods, ")"))
    
  })
  
  # create a character vector of tickers
  tickers <- eventReactive(input$submitBtn, {
    c(input$tickerSelect, input$tickerCompare)
  })
  

########################################################## Sidebar ####################################
  
  # render description for the selected ticker
  desc_text <- eventReactive(input$submitBtn, {
    crypto_config %>%
      dplyr::filter(symbol == input$tickerSelect) %>% 
      dplyr::pull(description)
  })
  output$descriptionTxt <- renderText({paste0('<p style="text-align:justify;font-size:14px;">',desc_text(),'</p>')})
  

######################################################### Overview ####################################
  
  
    
####################################################### Compare Page ####################################
  
  # retrieve live crypto price data
  data <- eventReactive(input$submitBtn, {
    
    ticker_xs <- paste0(tickers(), input$tickerBase) 
   
    start_dt <- input$dateRange[1]
    end_dt <- input$dateRange[2]
    freq <- input$freqSelect
    
    future_promise({get_crypto_data(ticker = ticker_xs, 
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
      prepare_ts_data(.)
    
  })
  
  # render timeseries plot of prices
  output$tickerTS <- plotly::renderPlotly({
    
    ttl <- ""
    x <- list(title = "")
    y <- list(title = "")
    
    if(input$dateRange[1] < input$dateRange[2] - 90) {
      min_dt <- input$dateRange[2] - 90  # focus data on last 3m if request more than 3m history
    } else {
      min_dt <- NULL
    }
    
    if(length(input$tickerCompare) > 0) {
      tooltip <- paste0("<b>%{text}</b><br>",
                        "Date: %{x}<br>",
                        "Rel. Perf: %{y}<br>",
                        "<extra></extra>")
    } else {
      tooltip <- paste0("<b>%{text}</b><br>",
                        "Date: %{x}<br>",
                        "Price: %{y}<br>",
                        "<extra></extra>")
    }
    
    ts_plot_data() %...>%
      plotly::plot_ly(., x = ~date, y = ~plot_value, mode = 'lines', linetype = ~ticker,
                      text = ~ticker,
                      hovertemplate = tooltip) %...>% 
      plotly::layout(title = ttl, xaxis = x, yaxis = y) %...>%
      plotly::rangeslider(start = min_dt, end = input$dateRange[2])
    
  })
  
  summ_data <- eventReactive(input$submitBtn, {
    build_compare_tbl(crypto_config, tickers = c(input$tickerSelect, input$tickerCompare))
  })
  
  # render summary table
  output$summaryTbl <- DT::renderDataTable({summ_data()})
  
  # calc correl mx
  corr_data_mx <- eventReactive(input$submitBtn, {
   data() %...>%
     calc_correl(., corr_type = "total")
  })
  
  # render correl mx
  output$correl_mx <- plotly::renderPlotly({
  
    corr_data_mx() %...>%
      ggcorrplot::ggcorrplot(.,
                             method = "square",
                             type = "lower",
                             show.legend = TRUE,
                             show.diag = TRUE, 
                             lab = TRUE) %...>%
      plotly::ggplotly()
    
  })
  
  output$corrX <- renderUI({
    selectizeInput(inputId = "corr_x", 
                   label = "Rolling Corr Var X", 
                   choices = paste0(tickers(), input$tickerBase),
                   multiple = FALSE)
  })
  
  output$corrY <- renderUI({
    selectizeInput(inputId = "corr_y", 
                   label = "Rolling Corr Var Y", 
                   choices = c(paste0(tickers(), input$tickerBase), "median"),
                   multiple = FALSE)
  })
  
  corr_data_ts <- eventReactive(input$submitBtn, {
    
    x <- input$corr_x
    y <- input$corr_y
    
    data() %...>%
      calc_correl(., corr_type = "roll", .roll_var_x = x, .roll_var_y = y, roll_period = input$rollPeriod)
  })
  
  output$correl_ts <- plotly::renderPlotly({
    
    ttl <- "Rolling Correlation"
    x <- list(title = "")
    y <- list(title = "Correlation")
    
    corr_data_ts() %...>%
      plotly::plot_ly(., x = ~date, y = ~roll_corr, mode = 'lines') %...>% 
      plotly::layout(title = ttl, xaxis = x, yaxis = y)
  })
  
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

  
  ######################################################### twitter stuff ##############################
  
  tweet_df <- eventReactive(input$submitBtn, {
    
    slug <- crypto_config %>%
      dplyr::filter(symbol == input$tickerSelect) %>% 
      dplyr::pull(slug)
      
    query <- paste0(input$tickerSelect, " OR ", slug)
    
    withProgress(min = 0, max = 1, value = 0.2, message = "updating tweets", {
      rtweet::search_tweets(q = query, 
                            n = 500, 
                            include_rts = TRUE,
                            filter = "verified", 
                            lang = "en",
                            token = readRDS(Sys.getenv("TWITTER_PAT")))
      })
    })
  
  # get ids of most liked, retweeted and top n
  most_popular_tweets <- reactive({most_popular(tweet_df(), n = 20)})
  most_retweeted_tweets <- reactive({most_retweeted(tweet_df(), n = 20)})
  recent_tweets <- reactive({most_recent(tweet_df(), n = 20)})
  
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
    
    twt_dt <- DT::datatable(tbl, options = list(lengthMenu = c(3, 5, 10)))
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
  
  tidy_txt <- reactive({
    tokenize_txt(tweet_df())
  })
  
  top_n_df <- reactive({
    get_top_n(tidy_txt(), n_top = 10, searched_ticker = tolower(input$tickerSelect))
  })
  
  sentiment_df <- reactive({
    get_sentiment(tidy_txt(), n_top = 10)
  })
  
  output$most_used_words <- plotly::renderPlotly({
    plot_top_n_words(top_n_df())
  })
  
  output$neg_pos_word_count <- plotly::renderPlotly({
    plot_sentiment(sentiment_df())
  })
  
########################################################## Forecast Page ####################################
  
  # prepare data for forecasting
  mdl_forc_list <- reactive({
    
    ticker_x <- paste0(input$tickerSelect, input$tickerBase)
    forcHorizon <- input$forcHorizon
    
    data() %...>%
      dplyr::filter(ticker == ticker_x) %...>%
      dplyr::filter(variable == "close") %...>%
      dplyr::select(ticker, date, value) %...>% 
      fit_forecasting_models(., forcHorizon)

  })

  mdl_forc <- reactive({
    
    mdl_forc_list() %...>% (function(mdl_list) {
      
      calib_tbl <- mdl_list$calibrate
      reqd_data <- mdl_list$reqd_data
      
      calib_tbl %>%
        modeltime::modeltime_forecast(actual_data = reqd_data)
    })
      
  })
  
  # render  plot of forecast
  output$plotForcast <- plotly::renderPlotly({
    mdl_forc() %...>%
      modeltime::plot_modeltime_forecast(.interactive = FALSE)
  })
  
  # prepare data for fair value analysis
  
  # plot FV boxplot
  
  # render summary stat table
   
  
##################################################### Lookup Page ####################################
  
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
