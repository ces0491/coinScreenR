library('magrittr')
library('modeltime') # need to specify otherwise can't access underlying prophet functions when fitting models

raw_coin_data <- readRDS('./inst/extdata/historical_coin_data.rds')

reqd_coin_data <- raw_coin_data %>% 
  dplyr::select(Symbol, Date, Close) %>%
  dplyr::filter(Symbol == 'BTC')

# visualise timeseries by group
coin_ts_plots <- reqd_coin_data %>%  
  dplyr::group_by(Symbol) %>% 
  timetk::plot_time_series(.date_var = Date, 
                           .value = Close, 
                           .facet_ncol = 3, 
                           .interactive = FALSE)

# 30 day (daily ts data) forecast period  
forecast_horizon <- 365

# full data set
full_data_tbl <- reqd_coin_data %>% 
  dplyr::group_by(Symbol) %>% 
  timetk::future_frame(.date_var = Date, 
                       .length_out = forecast_horizon, 
                       .bind_data = TRUE) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Symbol = forcats::fct_drop(Symbol))

# view data
data_view <- full_data_tbl %>% 
  dplyr::filter(!is.na(Close)) %>% 
  dplyr::group_by(Symbol) %>% 
  timetk::tk_summary_diagnostics() %>% 
  dplyr::ungroup()

# clean data
viable_tickers <- data_view %>% 
  dplyr::filter(n.obs > 730) %>% # assume we need at least 2 years of daily data
  dplyr::pull(Symbol)

clean_data <- full_data_tbl %>% 
  dplyr::filter(Symbol %in% viable_tickers) %>%
  dplyr::group_by(Symbol) %>% 
  dplyr::mutate(min_dt = min(Date)) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(max_min = max(min_dt)) %>% 
  dplyr::filter(Date >= max_min) %>% 
  dplyr::select(-min_dt, -max_min)

clean_data_view <- clean_data %>% 
  dplyr::group_by(Symbol) %>% 
  timetk::tk_summary_diagnostics(.date_var = Date) %>% 
  dplyr::ungroup()


# training data
data_prepared_tbl <- clean_data


# forecast data
future_tbl <- clean_data 

# panel data splitting
splits <- data_prepared_tbl %>% 
  timetk::time_series_split(date_var = Date, assess = forecast_horizon, cumulative = TRUE)

# splits %>%
#   timetk::tk_time_series_cv_plan() %>%
#   timetk::plot_time_series_cv_plan(Date, Close, .interactive = FALSE, .facet_ncol = 3)
  
# create pre-processor

recipe_spec_1 <- recipes::recipe(Close ~ Date, rsample::training(splits)) %>% 
  timetk::step_timeseries_signature(Date) %>% 
  recipes::step_rm(tidyselect::matches("(.iso$)|(.xts$)|(day)|(hour)|(minute)|(second)|(am.pm)")) %>% 
  recipes::step_normalize(Date_index.num, Date_month) %>%
  recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE) 

# view recipe
recipe_spec_1 %>% recipes::prep() %>% recipes::juice()

# for modeltime algos, we use recipe spec 1 where Date has role 'predictor'
# non modeltime algos use spec 2 in which the role of date is changed to 'ID'

recipe_spec_2 <- recipe_spec_1 %>% 
  recipes::update_role(Date, new_role = "ID")

recipe_spec_1 %>% recipes::prep() %>% summary()
recipe_spec_2 %>% recipes::prep() %>% summary()

# models

# prophet with regressors
wflw_fit_prophet <- workflows::workflow() %>% 
  workflows::add_model(
    modeltime::prophet_reg() %>% 
      parsnip::set_engine("prophet")
  ) %>% 
  workflows::add_recipe(recipe_spec_1) %>% 
  parsnip::fit(rsample::training(splits))


# XGBoost
wflw_fit_xgboost <- workflows::workflow() %>% 
  workflows::add_model(
    parsnip::boost_tree() %>% 
      parsnip::set_engine("xgboost")
  ) %>% 
  workflows::add_recipe(recipe_spec_2) %>% 
  parsnip::fit(rsample::training(splits))

# random forest
wflw_fit_rf <- workflows::workflow() %>% 
  workflows::add_model(
    parsnip::rand_forest() %>% 
      parsnip::set_engine("ranger")
  ) %>% 
  workflows::add_recipe(recipe_spec_2) %>% 
  parsnip::fit(rsample::training(splits))

# support vector machine
wflw_fit_svm <- workflows::workflow() %>% 
  workflows::add_model(
    parsnip::svm_rbf() %>% 
      parsnip::set_engine("kernlab")
  ) %>% 
  workflows::add_recipe(recipe_spec_2) %>% 
  parsnip::fit(rsample::training(splits))

# prophet boost
wflw_fit_prophet_boost <- workflows::workflow() %>% 
  workflows::add_model(
    modeltime::prophet_boost(
      seasonality_daily = FALSE,
      seasonality_weekly = FALSE,
      seasonality_yearly = FALSE
    ) %>% 
      parsnip::set_engine("prophet_xgboost")
  ) %>% 
  workflows::add_recipe(recipe_spec_1) %>% 
  parsnip::fit(rsample::training(splits))

# modeltime wf

submodels_tbl <- modeltime::modeltime_table(
  wflw_fit_prophet,
  wflw_fit_prophet_boost,
  wflw_fit_svm,
  wflw_fit_xgboost
) #wflw_fit_rf

# calibrate testing data
submodels_calibrated_tbl <- submodels_tbl %>% 
  modeltime::modeltime_calibrate(rsample::testing(splits))

submodels_calibrated_tbl %>% modeltime::modeltime_accuracy()

# visualise test forecasts
test_forc_viz <- submodels_calibrated_tbl %>% 
  modeltime::modeltime_forecast(
    new_data = rsample::testing(splits),
    actual_data = data_prepared_tbl,
    keep_data = TRUE
  ) %>% 
  dplyr::group_by(Symbol) %>% 
  modeltime::plot_modeltime_forecast(.facet_ncol = 3)

test_forc_viz

# Refit on full training data set

submodels_refit_tbl <- submodels_calibrated_tbl %>% 
  modeltime::modeltime_refit(data_prepared_tbl)

refit_forc_viz <- submodels_refit_tbl %>% 
  modeltime::modeltime_forecast(
    new_data = future_tbl,
    actual_data = data_prepared_tbl,
    keep_data = TRUE
  ) %>% 
  dplyr::group_by(Symbol) %>% 
  modeltime::plot_modeltime_forecast(.facet_ncol = 3)

refit_forc_viz

#### ensemble

# fit
ensemble_fit_mean <- submodels_tbl %>% 
  modeltime.ensemble::ensemble_average(type = 'mean')

# modeltime table
ensemble_tbl <- modeltime::modeltime_table(ensemble_fit_mean)

# test accuracy
ensemble_tbl %>% 
  modeltime::combine_modeltime_tables(submodels_tbl) %>% 
  modeltime::modeltime_accuracy(rsample::testing(splits))

ensemble_tbl %>% 
  modeltime::modeltime_forecast(
    new_data = rsample::testing(splits),
    actual_data = data_prepared_tbl,
    keep_data = TRUE
  ) %>% 
  dplyr::group_by(Symbol) %>% 
  modeltime::plot_modeltime_forecast()

ensemble_refit_tbl <- ensemble_tbl %>% 
  modeltime::modeltime_refit(data_prepared_tbl)

ensemble_refit_tbl %>% 
  modeltime::modeltime_forecast(
    new_data = future_tbl,
    actual_data = data_prepared_tbl,
    keep_data = TRUE
  ) %>% 
  dplyr::group_by(Symbol) %>% 
  modeltime::plot_modeltime_forecast()


##########################################################################################################################

forcHorizon <- 30

test_data <- fdoR::get_crypto_data(ticker = c('BTCBUSD', 'XRPBUSD', 'DOTBUSD'), 
                                   start_date = Sys.Date() - 500, 
                                   end_date = Sys.Date(), 
                                   frequency = 'daily')

reqd_analysis_data <- test_data %>% 
  dplyr::filter(variable == "close") %>% 
  dplyr::select(ticker, date, value)

split_data <-reqd_analysis_data %>% 
  timetk::time_series_split(date_var = date, assess = forcHorizon, cumulative = TRUE)


recipe <- recipes::recipe(value ~ date, rsample::training(split_data)) %>% 
  timetk::step_timeseries_signature(date) %>% 
  recipes::step_rm(tidyselect::matches("(.iso$)|(.xts$)|(day)|(hour)|(minute)|(second)|(am.pm)")) %>% 
  recipes::step_normalize(date_index.num, date_month) %>%
  recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)


models <- fit_forecasting_models(split_data, recipe)


calibration_tbl <- models %>%
  modeltime::modeltime_calibrate(rsample::testing(split_data))

forc_plot <- calibration_tbl %>%
  modeltime::modeltime_forecast(actual_data = reqd_analysis_data) %>%
  modeltime::plot_modeltime_forecast(.interactive = TRUE)

acc_tbl <- calibration_tbl %>%
  modeltime::modeltime_accuracy() %>%
  modeltime::table_modeltime_accuracy(.interactive = FALSE)

# Remove underperforming models
kp_models <- as.data.frame(acc_tbl) %>%
  dplyr::mutate(rsq = as.numeric(rsq)) %>% 
  dplyr::filter(!is.na(rsq),
                rsq >= 0.3) %>% 
  dplyr::pull(.model_id)

calibration_tbl_new <- calibration_tbl %>% 
  dplyr::filter(.model_id %in% kp_models)

# Refit and Forecast Forward
calibration_tbl_new %>% 
  modeltime::modeltime_refit(reqd_analysis_data) %>%
  modeltime::modeltime_forecast(h = forcHorizon, actual_data = reqd_analysis_data) %>%
  modeltime::plot_modeltime_forecast(.interactive = TRUE)

# ensemble

ensemble_fit_mean <- submodels_tbl %>% 
  modeltime.ensemble::ensemble_average(type = 'mean')

ensemble_tbl <- modeltime::modeltime_table(ensemble_fit_mean)

ensemble_tbl %>% modeltime::combine_modeltime_tables(submodels_tbl) %>% 
  modeltime::modeltime_accuracy(rsample::testing(splits))

