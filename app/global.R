library(shiny)
library(magrittr)
library(shinyWidgets)
library(promises)
library(future)
library(shinycssloaders)
library(modeltime) # need to specify otherwise can't access underlying prophet functions when fitting models

globalVariables(c("status_id", "id", "created_at", "retweet_count"))

# multisession: Resolves futures asynchronously (in parallel) in separate R sessions running in the background on the same machine.
future::plan(multisession)

source("../R/get_data.R")
source("../R/stats_functions.R")
source("../R/plot_functions.R")
source("../R/table_functions.R")
source("../R/forecasting_functions.R")
source("../R/twitter_functions.R")
source("../R/sentiment_functions.R")


# Get a paginated list of all active cryptocurrencies with latest market data, sorted by CMC rank.
coinmarketcapr::setup(api_key = Sys.getenv("coinmarketcap_api_key"))

crypto_listings <- coinmarketcapr::get_crypto_listings(currency = "USD", latest = TRUE)
crypto_meta <- coinmarketcapr::get_crypto_meta(crypto_listings$symbol) %>% dplyr::select(-name, -symbol, -slug, -date_added, -tags)
crypto_ratings <- readRDS('../inst/extdata/crypto_rating.rds')

crypto_config <- crypto_listings %>% 
  dplyr::left_join(crypto_meta, by = "id") %>% 
  dplyr::left_join(crypto_ratings, by = "symbol")
