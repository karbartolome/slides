data <-  USgas::us_residential %>% rename(value=y) %>%  
  filter(state %in%  c('Nevada','Maine','Hawaii','West Virginia'))

data %>% 
  group_by(state) %>%
  plot_time_series(.date_var = date, 
                   .value=value, 
                   .facet_ncol = 2, 
                   .line_size = 0.2, 
                   .interactive = FALSE
  )

data_hawaii <- data %>%
  filter(state=='Hawaii') %>%
  select(-state)

# Recipe without steps
recipe_base <- recipe(value~date, data=data_hawaii)

# Some date variables
recipe_date_extrafeatures <- recipe_base %>% 
  step_date(date, features = c('month','year')) %>% 
  step_date(date, features = c('quarter','semester'))

# Lagged values: 1 to 6 months lag  
recipe_date_extrafeatures_lag <- recipe_date_extrafeatures %>% 
  step_lag(value, lag = 1:6) %>% 
  step_ts_impute(all_numeric(), period=365)

# Fourier transformation
recipe_date_extrafeatures_fourier <-recipe_date_extrafeatures  %>% 
  step_fourier(date, period = 365/12, K = 1)

# prophet_xgboost
prophet_boost <- prophet_boost(mode = 'regression') %>% 
  set_engine("prophet_xgboost")

# nnetar
nnetar <- nnetar_reg() %>% 
  set_engine("nnetar")

#auto_arima_xgboost
auto_arima_boost <- arima_boost() %>% 
  set_engine('auto_arima_xgboost')



wfsets <- workflow_set(
  preproc = list(
    base                  = recipe_base,
    extrafeatures         = recipe_date_extrafeatures,
    extrafeatures_lag     = recipe_date_extrafeatures_lag,
    extrafeatures_fourier = recipe_date_extrafeatures_fourier
  ),
  models  = list(
    M_arima_boost       = auto_arima_boost,
    M_prophet_boost     = prophet_boost,
    M_nnetar            = nnetar
  ),
  cross   = TRUE
) 
wfsets


# wfsets <- wfsets %>% 
#   anti_join(
#     tibble(wflow_id = c("extrafeatures_lag_M_nnetar")), 
#     by = "wflow_id")

nested_data <- data %>% 
  nest(nested_column=-state)

wfs_multifit <- modeltime_wfs_multifit(serie = nested_data,
                                       .prop = 0.8,
                                       .wfs  = wfsets)

# saveRDS(wfs_multifit, 'modelos/wfs_multifit.rds')
wfs_multifit <- readRDS('modelos/wfs_multifit.rds')



plots <- wfs_multifit$models_accuracy %>% 
  select(-.model_id) %>%  
  rename(.model_id=.model_names) %>% mutate(.fit_model = '') %>% 
  group_by(name_serie) %>% 
  nest() %>% 
  mutate(plot = map(data, ~ modeltime_wfs_heatmap(., metric = 'rsq',
                                                  low_color = '#ece2f0',
                                                  high_color = '#02818a'))) %>% 
  ungroup()

plots$plot[[1]]


wfs_multiforecast <- modeltime_wfs_multiforecast(
  wfs_multifit$table_time, .prop=0.8)


wfs_multiforecast %>% 
  select(state, nested_forecast) %>% 
  unnest(nested_forecast) %>% 
  group_by(state) %>% 
  plot_modeltime_forecast(
    .legend_max_width = 12,
    .facet_ncol = 2, 
    .line_size = 0.5,
    .interactive = FALSE,
    .facet_scales = 'free_y',
    .title='Proyecciones') +
  theme(legend.position='bottom')


wfs_bests<- modeltime_wfs_multibestmodel(
  .table = wfs_multiforecast,
  .metric = "rmse",
  .minimize = FALSE
)

wfs_refit <- modeltime_wfs_multirefit(wfs_bests)

wfs_forecast <- modeltime_wfs_multiforecast(wfs_refit, .h = "12 month")

wfs_forecast %>% 
  select(state, nested_forecast) %>% 
  unnest(nested_forecast) %>% 
  group_by(state) %>% 
  plot_modeltime_forecast(
    .legend_max_width = 12,
    .facet_ncol = 1, 
    .line_size = 0.5,
    .interactive = FALSE,
    .facet_scales = 'free_y',
    .title='Proyecciones')+
  theme(legend.position='right')
