#' use modeltime workflows to setup ML models
#'
#' @param split_data list containing tbl_df of data split into test and training subsets 
#' @param recipe a data recipe for your ML models
#'
#' @return a modeltime tbl_df containing your fitted models
#' 
fit_forecasting_models <- function(split_data, recipe){
  
  # for modeltime algos, we use recipe spec 1 where Date has role 'predictor'
  # non modeltime algos use spec 2 in which the role of date is changed to 'ID'
  recipe_spec_2 <- recipe %>% 
    recipes::update_role(date, new_role = "ID")
  
  # auto arima
  model_fit_arima <- modeltime::arima_reg() %>%
    parsnip::set_engine("auto_arima") %>%
    parsnip::fit(value ~ date, rsample::training(split_data))
  
  # prophet with regressors
  wflw_fit_prophet <- workflows::workflow() %>% 
    workflows::add_model(
      modeltime::prophet_reg() %>% 
        parsnip::set_engine("prophet")
    ) %>% 
    workflows::add_recipe(recipe) %>% 
    parsnip::fit(rsample::training(split_data))
  
  
  # XGBoost
  wflw_fit_xgboost <- workflows::workflow() %>% 
    workflows::add_model(
      parsnip::boost_tree() %>% 
        parsnip::set_engine("xgboost")
    ) %>% 
    workflows::add_recipe(recipe_spec_2) %>% 
    parsnip::fit(rsample::training(split_data))
  
  # random forest
  wflw_fit_rf <- workflows::workflow() %>% 
    workflows::add_model(
      parsnip::rand_forest() %>% 
        parsnip::set_engine("ranger")
    ) %>% 
    workflows::add_recipe(recipe_spec_2) %>% 
    parsnip::fit(rsample::training(split_data))
  
  # support vector machine
  wflw_fit_svm <- workflows::workflow() %>% 
    workflows::add_model(
      parsnip::svm_rbf() %>% 
        parsnip::set_engine("kernlab")
    ) %>% 
    workflows::add_recipe(recipe_spec_2) %>% 
    parsnip::fit(rsample::training(split_data))
  
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
    workflows::add_recipe(recipe) %>% 
    parsnip::fit(rsample::training(split_data))
  
  # elastic net
  workflow_fit_glmnet <- workflows::workflow() %>%
    workflows::add_model(
      parsnip::linear_reg(penalty = 0.01, mixture = 0.5) %>%
        parsnip::set_engine("glmnet")
    ) %>%
    workflows::add_recipe(recipe_spec_2) %>%
    parsnip::fit(rsample::training(split_data))
  
  # modeltime wf
  submodels_tbl <- modeltime::modeltime_table(
    model_fit_arima,
    wflw_fit_prophet,
    wflw_fit_prophet_boost,
    wflw_fit_svm,
    wflw_fit_xgboost,
    wflw_fit_rf,
    workflow_fit_glmnet
  )
  
  submodels_tbl
}

