library(shiny)
library(magrittr)
library(shinyWidgets)
library(promises)
library(future)
library(shinycssloaders)
library(modeltime) # need to specify otherwise can't access underlying prophet functions when fitting models

require(dateR)
require(assertR)

globalVariables(c("status_id", "id", "created_at", "retweet_count"))

# multisession: Resolves futures asynchronously (in parallel) in separate R sessions running in the background on the same machine.
future::plan(multisession)

source("../R/plot_functions.R")
source("../R/table_functions.R")
source("../R/forecasting_functions.R")
source("../R/twitter_functions.R")
# source("../R/sentiment_functions.R")


# Get a paginated list of all active cryptocurrencies with latest market data, sorted by CMC rank.
coinmarketcapr::setup(api_key = Sys.getenv("coinmarketcap_api_key"))
crypto_config <- coinmarketcapr::get_crypto_listings(currency = "USD", latest = TRUE)
