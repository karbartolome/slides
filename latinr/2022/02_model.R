library(sknifedatar)
library(modeltime)
library(workflowsets)
library(tidymodels)
library(tidyverse)
library(timetk)
library(anomalize) 
library(lubridate)
library(gt)

data <- read_csv('data/df_sube.csv') %>% 
  rename(date=fecha, value=n)


# EDA ---------------------------------------------------------------------
data %>% group_by(linea) %>%
  plot_time_series(date, value, .facet_ncol = 2, .interactive=FALSE) 


# Pre procesamiento (1 serie) ---------------------------------------------
data_ffcc_roca <- data %>% 
  filter(linea=='FFCC ROCA') %>% 
  select(-linea) %>% ungroup() %>% 
  mutate(fl_pandemia = ifelse(date >= '2020-03-01' & date<='2021-01-01', 1,0))




# Modelo (1 serie) --------------------------------------------------------

splits <- data_ffcc_roca %>%  
  initial_time_split(prop = 0.7)

splits %>% 
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, 
                           .title='Partición temporal') 


receta <- recipe(value ~ date+fl_pandemia, data = training(splits)) %>% 
  step_date(date, 
            features = c('week', 'month','year','quarter','semester'))


receta %>% prep() %>% juice() %>% head(2) %>% gt()

# Modelo: Auto-ARIMA
m_autoarima <- arima_reg() %>% 
  set_engine('auto_arima') %>%  
  fit(value~date, data=training(splits))

# Modelo: regresión lineal
m_reg_lineal <- linear_reg() %>%
  set_engine("lm") %>%
  fit(value ~ as.numeric(date) + 
        factor(month(date, label = TRUE), ordered = FALSE)+
        factor(wday(date, label=TRUE), ordered=FALSE) +
        fl_pandemia,
      data = training(splits))

# Workflow: prophet boosted
m_prophet_boost <- workflow() %>% 
  add_recipe(receta) %>% 
  add_model(
    prophet_boost(mode='regression') %>%
      set_engine("prophet_xgboost")
  ) %>% 
  fit(data = training(splits))


modelos <- modeltime_table(m_autoarima,
                           m_reg_lineal,
                           m_prophet_boost
)


calibration_table  <- modelos %>% 
  modeltime_calibrate(new_data = testing(splits))

calibration_table %>% 
  modeltime_accuracy() %>% gt() %>% 
  fmt_number(decimals=2)

forecast_series <- calibration_table %>% 
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = data_ffcc_roca)


forecast_series %>% 
  plot_modeltime_forecast(
    .legend_max_width     = 30, 
    .interactive          = TRUE,
    .conf_interval_alpha  = 0.2
  )


refit_tbl <- calibration_table %>%
  filter(.model_id %in% c(2)) %>% 
  modeltime_refit(data = data_ffcc_roca)


new_data = data.frame(
  date = seq(as.Date('2022-09-24'),as.Date('2023-03-24'), by=1)
) %>% 
  mutate(fl_pandemia=ifelse(date>='2023-01-01',1,0)) %>% 
  mutate(fl_pandemia=0)

forecast_final <- refit_tbl %>% 
  modeltime_forecast(
    actual_data = data_ffcc_roca %>% as.data.frame(),
    new_data = new_data 
  )

forecast_final %>% 
  plot_modeltime_forecast(
    .legend_max_width = 30, 
    .interactive      = FALSE,
    .conf_interval_alpha = 0.2
  )

# Múltiples series --------------------------------------------------------





