library(sknifedatar)
library(modeltime)
library(workflowsets)
library(tidymodels)
library(tidyverse)
library(timetk)
library(anomalize) 

data <-  USgas::us_residential %>% 
  rename(value=y) %>%  
  filter(state %in%  c('Nevada','Maine',
                       'Hawaii','West Virginia'))

nest_data <- data %>% nest(nested_column = -state)


# 1 modelo 1 serie --------------------------------------------------------
data_hawaii <- data %>% 
  filter(state=='Hawaii')

splits <- data_hawaii %>%  
  initial_time_split(prop = 0.9)


receta <- recipe(value ~ ., 
                 data = training(splits) %>% select(-state)) %>%
  step_date(date, features = c("month", "quarter", "year")) 

receta_num <- recipe(value ~ date, training(splits)) %>%
  step_timeseries_signature(date) %>%
  step_rm(contains("am.pm"), contains("hour"), contains("minute"),
          contains("second"), contains("xts")) %>%
  step_dummy(all_nominal())


# Modelo elastic net

linear_reg_lm_spec <- linear_reg() %>%
  set_engine('lm')


wf_lineal <- workflow() %>% 
  add_recipe(receta_num) %>% 
  add_model(linear_reg_lm_spec) %>% 
  fit(training(splits))

# Modelo Auto-ARIMA
autoarima_boost_reg <- workflow() %>% 
  add_recipe(receta) %>% 
  add_model(arima_boost() %>% 
              set_engine('auto_arima_xgboost')) %>% 
  fit(training(splits))

# Modelo exponential smoothing
exp_smoothing = exp_smoothing() %>% 
  set_engine('ets') %>% 
  fit(value~date, data=training(splits))

# Modelo prophet boosted
prophet_boost <- workflow() %>% 
  add_recipe(receta) %>% 
  add_model(prophet_boost(mode='regression') %>%
      set_engine("prophet_xgboost")) %>% 
  fit(data = training(splits))

# Modelo NNetar
nnetar <- workflow() %>%
  add_recipe(receta) %>% 
  add_model(nnetar_reg() %>%
      set_engine("nnetar")) %>% 
  fit(training(splits))

# Model table
modelos <- modeltime_table(
  autoarima_boost_reg,
  exp_smoothing,
  prophet_boost,
  nnetar
)

calibration_table <- modelos %>%
  modeltime_calibrate(new_data = testing(splits))

accuracy_table <- function(.accuracy, 
                           .subtitle=NULL,
                           .color1='#c5cde0', 
                           .color2='#6274a3',
                           .round_num=0.01){
  
  .accuracy %>%
    select(-.type) %>%
    mutate(across(where(is.numeric), round, 2)) %>%
    gt(rowname_col = c(".model_id")) %>%
    tab_header(title = 'Evaluación de modelos',
               subtitle = .subtitle) %>%
    cols_label(`.model_desc` = 'Modelo') %>%
    data_color(columns = vars(mae),
               colors = scales::col_numeric(
                 reverse = TRUE,
                 palette = c('white',.color1, .color2), 
                 domain = c(min(.accuracy$mae)-.round_num,
                            max(.accuracy$mae)+.round_num)
               )) %>% 
    data_color(columns = vars(mape),
               colors = scales::col_numeric(
                 reverse = TRUE,
                 palette = c('white',.color1, .color2), 
                 domain = c(min(.accuracy$mape)-.round_num,
                            max(.accuracy$mape)+.round_num)
               )) %>% 
    data_color(columns = vars(mase),
               colors = scales::col_numeric(
                 reverse = TRUE,
                 palette = c('white',.color1, .color2 ), 
                 domain = c(min(.accuracy$mase)-.round_num,
                            max(.accuracy$mase)+.round_num)
               )) %>% 
    data_color(columns = vars(smape),
               colors = scales::col_numeric(
                 reverse = TRUE,
                 palette = c('white',.color1, .color2), 
                 domain = c(min(.accuracy$smape)-.round_num,
                            max(.accuracy$smape)+.round_num)
               )) %>% 
    data_color(columns = vars(rmse),
               colors = scales::col_numeric(
                 reverse = TRUE,
                 palette = c('white',.color1, .color2), 
                 domain = c(min(.accuracy$rmse)-.round_num,
                            max(.accuracy$rmse)+.round_num)
               )) %>% 
    data_color(columns = vars(rsq),
               colors = scales::col_numeric(
                 reverse = FALSE,
                 palette = c('white',.color1, .color2), 
                 domain = c(min(.accuracy$rsq)-.round_num,
                            max(.accuracy$rsq)+.round_num)
               )) 
  
}

accuracy_table(
  calibration_table %>% modeltime_accuracy(), 
  .subtitle = 'Métricas')

forecast_transporte <- calibration_table %>%
  filter(.model_id %in% c(1,3)) %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = data_transporte %>% filter(date<'2020-01-02') 
  )

forecast_transporte %>% 
  plot_modeltime_forecast(
    .legend_max_width = 30, 
    .interactive      = FALSE,
    .conf_interval_alpha = 0.2, 
    .line_size=0.2
  )

refit_table <- calibration_table %>%
  filter(.model_id == 3) %>% 
  modeltime_refit(data_transporte %>% filter(date<'2020-01-02'))

proyeccion <- refit_table %>% 
  modeltime_forecast(
    actual_data = data_transporte %>% filter(date<'2020-01-02'),
    h='1 years'
  )

proyeccion %>% 
  plot_modeltime_forecast(
    .legend_max_width    = 30, 
    .interactive         = FALSE,
    .conf_interval_alpha = 0.2, 
    .line_size           = 0.2, 
  )
# Múltiples series y modelos ------------------------------------
receta <- recipe(value ~ ., data = data %>% select(-sector)) %>%
  step_date(date, features = c("month", "quarter", "year"))

# Modelo Auto-ARIMA
autoarima_boost_reg <- workflow() %>% 
  add_recipe(receta) %>% 
  add_model(arima_boost() %>% 
              set_engine('auto_arima_xgboost'))

# Modelo prophet boosted
prophet_boost <- workflow() %>% 
  add_recipe(receta) %>% 
  add_model(prophet_boost(mode='regression') %>%
              set_engine("prophet_xgboost")) 

# Modelo NNetar
nnetar <- workflow() %>%
  add_recipe(receta) %>% 
  add_model(nnetar_reg() %>%
              set_engine("nnetar")) 

# Model table
model_table_emae <- modeltime_multifit(
  serie = nest_data,
  .prop = 0.8,
  autoarima_boost_reg,
  prophet_boost,
  nnetar
) 

forecast_emae <- modeltime_multiforecast(
  model_table_emae$table_time,
  .prop = 0.8
)

best_model_emae <- modeltime_multibestmodel(
  .table = forecast_emae,
  .metric = 'rsq',
  .minimize = FALSE 
)

model_refit_emae <- modeltime_multirefit(best_model_emae)

model_refit_emae <- model_refit_emae %>% 
  bind_cols(data %>% 
              nest(nested_column=-sector) %>% 
              select(-sector) %>% 
              rename(actual_data=nested_column)
  )

modeltime_multiforecast_pandemia = function(models_table,
                                            .fecha = '2020-02-01', 
                                            .prop = 0.8) {
  
  models_table %>% mutate(
    nested_forecast = pmap(list(calibration, nested_column, actual_data), 
       function(calibration, nested_column, actual_data) {
         calibration %>% modeltime_forecast(
           new_data = actual_data  %>% filter(date >= .fecha), 
           actual_data = actual_data
         ) %>%
           mutate(
             .model_details = .model_desc,
             .model_desc = str_replace_all(.model_desc, "[[:punct:][:digit:][:cntrl:]]", "")
           ) %>%
           mutate(.model_desc = ifelse(str_detect(.model_desc, "ARIMA"), "ARIMA", .model_desc))
       }))
  
}


forecast_best_emae <- modeltime_multiforecast_pandemia(model_refit_emae,
                                                       .fecha = '2020-02-01',
                                                       .prop = 0.8)

forecast_best_emae %>% 
  select(sector, nested_forecast) %>% 
  unnest(nested_forecast) %>% 
  group_by(sector) %>% 
  plot_modeltime_forecast(
    .legend_max_width = 12,
    .facet_ncol = 2, 
    .line_size = 0.5,
    .interactive = FALSE,
    .facet_scales = 'free_y',
    .title='Proyecciones') 
# Workflowsets múltiple ---------------------------------------------------



