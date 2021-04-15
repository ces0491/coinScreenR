library(shiny)
library(magrittr)
library(shinyWidgets)
library(promises)
library(future)
library(shinycssloaders)
library(modeltime)

require(dateR)
require(assertR)

# multisession: Resolves futures asynchronously (in parallel) in separate R sessions running in the background on the same machine.
future::plan(multisession)

source("../R/plot_functions.R")
source("../R/table_functions.R")
source("../R/forecasting.R")


#get crypto listings
crypto_config <- coinmarketcapr::get_crypto_listings()
