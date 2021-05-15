
# define sidebar
sidebar <- shinydashboard::dashboardSidebar(
  
  shinydashboard::sidebarMenu(
    shinydashboard::menuItem("Menu", tabName = "overview", icon = icon("bars"), startExpanded = FALSE,
                             shinydashboard::menuSubItem("Overview", tabName = "overview", icon = icon("coins")),
                             shinydashboard::menuSubItem("Compare", tabName = "compare", icon = icon("list")),
                             shinydashboard::menuSubItem("Sentiment", tabName = "sentiment", icon = icon("commenting-o")),
                             shinydashboard::menuSubItem("Forecast", tabName = "forecast", icon = icon("chart-line")),
                             shinydashboard::menuSubItem("Breakeven", tabName = "breakeven", icon = icon("balance-scale")),
                             shinydashboard::menuSubItem("Fair Value", tabName = "fv", icon = icon("area-chart")),
                             shinydashboard::menuSubItem("Lookup", tabName = "lookup", icon = icon("search")),
                             shinydashboard::menuSubItem("Trade", tabName = "trade", icon = icon("briefcase"))
                             )
  ),
  
  shinyjs::useShinyjs(),
  
  width = 350,
  
  selectizeInput(inputId = "tickerSelect", 
                 label = "Enter Ticker", 
                 choices = crypto_config$symbol,
                 multiple = FALSE,
                 options = list(placeholder = 'BTC')),
  
  selectizeInput(inputId = "tickerCompare", 
                 label = "Enter Comparison Ticker(s)", 
                 choices = crypto_config$symbol,
                 multiple = TRUE,
                 options = list(placeholder = 'LTC, ETH')),
  
  selectizeInput(inputId = "tickerBase", 
                 label = "Enter Ticker Base Unit", 
                 choices = c('BUSD', 'USDT', 'BNB', 'ZAR'),
                 multiple = FALSE,
                 options = list(placeholder = 'USDT')),
  
  br(),
  
  dateRangeInput(inputId = "dateRange",
                 label = "Select Date Range",
                 start = Sys.Date() - 500, # binance API allows max 500 historical records
                 end = Sys.Date()),
  
  selectizeInput(inputId = "freqSelect",
                 label = "Select Frequency",
                 choices = list("Daily" = "1d", "Weekly" = "1w", "Monthly" = "1M"),
                 selected = "Daily",
                 multiple = FALSE),
  
  actionButton(inputId = "submitBtn", label = "Submit",
               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
  
  hr(),
  
  column(12, align = "left", offset = 0, div(tags$div(HTML('<h1 style="font-size:20px;">Description</h1>')))),
  column(12, align = "left", offset = 0, htmlOutput(outputId = "descriptionTxt") %>% withSpinner()), #use htmloutput so we can render the output text with html tags
  
  hr(),
  
  column(12, uiOutput("cmcWidget") %>% withSpinner()),
  
  br(),
  
  column(12, align = "left", offset = 0, 
         downloadButton(outputId = "downloadData", 
                        label = "Download", 
                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
  
)

# define body
body <- shinydashboard::dashboardBody(
  
  shinydashboard::tabItems(
    shinydashboard::tabItem(tabName = "overview", h2("Overview"),
                            
                            fluidPage(
                              fluidRow(
                                column(12, uiOutput("cmcWidgetMarquee") %>% withSpinner()),  
                              ),
                              
                              hr(),
                              
                              fluidRow(
                                column(12, reactable::reactableOutput("overviewTbl") %>% withSpinner())
                              )
                            )
    ),
    shinydashboard::tabItem(tabName = "compare", h2("Compare"),
                            
                            fluidPage(
                              
                              fluidRow(
                                column(12, plotly::plotlyOutput("tickerTS") %>% withSpinner()),  
                              ),
                              
                              hr(),
                              
                              fluidRow(
                                column(12, DT::dataTableOutput("summaryTbl") %>% withSpinner())
                              ),
                              
                              hr(),
                              
                              fluidRow(
                                column(6, plotly::plotlyOutput("correl_mx") %>% withSpinner()
                                       ),
                                column(6, 
                                       fluidRow(column(4, uiOutput("corrX")),
                                                column(4, uiOutput("corrY")),
                                                column(4, numericInput(inputId = "rollPeriod", label = "Rolling Window", value = 30, min = 0))
                                         ),
                                       fluidRow(plotly::plotlyOutput("correl_ts") %>% withSpinner())
                                       )
                                )
                              )
    ),
    shinydashboard::tabItem(tabName = "sentiment", h2("Sentiment"),
                            
                            fluidPage(
                                column(4, shinydashboard::tabBox(title = "Tweets", id = "tweets_tabbox", width = 12,
                                                                 tabPanel(icon("retweet"), DT::dataTableOutput("most_retweeted") %>% withSpinner()),
                                                                 tabPanel(icon("heart"), DT::dataTableOutput("most_popular_tweets") %>% withSpinner()),
                                                                 tabPanel(icon("calendar"), DT::dataTableOutput("recent_tweets") %>% withSpinner())
                                                                 )
                                       ),
                                column(8,
                                       fluidRow(
                                         column(12, plotly::plotlyOutput("most_used_words") %>% withSpinner())
                                         ),
                                       hr(),
                                       fluidRow(
                                         column(12, plotly::plotlyOutput("neg_pos_word_count") %>% withSpinner())
                                         )
                                       )
                                )
                              
    ),
    shinydashboard::tabItem(tabName = "forecast", h2("Forecast"),
                            
                            fluidPage(
                              
                              numericInput(
                                inputId = "forcHorizon",
                                label = "Forecast Horizon",
                                value = 30, 
                                min = 0, 
                                max = NA,
                                step = NA),
                              
                              br(),
                              
                              plotly::plotlyOutput("plotForcast") %>% withSpinner(),
                              
                              hr(),
                              
                              fluidRow(
                                column(6, tableOutput("analysisTbl") %>% withSpinner()),
                                column(6, plotly::plotlyOutput("fvBox") %>% withSpinner())
                              )
                            )
    ),
    
    shinydashboard::tabItem(tabName = "lookup", h2("Crypto Lookup"),
                            fluidPage(
                              div(tags$div(HTML('<i class="fa fa-info-circle"></i> 
                                                Use this table to lookup a ticker by name if you are unsure about the symbol'))),
                              br(),
                              DT::dataTableOutput(outputId = "searchTickerTbl") %>% withSpinner()
                            )
    ),
    
    shinydashboard::tabItem(tabName = "trade", h2("Trade"),
                            
                            fluidPage(
                              div(tags$div(HTML('<h1 style="font-size:60px;">Placeholder Page for potential portfolio overview 
                                                and order execution </h1>')))
                            )
    )
  )
)

# Put them together into a dashboardPage
ui.coinScreenR <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(
    title = "Coin Screener",
    titleWidth = 350),
  sidebar,
  body
)