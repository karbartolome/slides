library(gtrendsR)
library(tidyverse)
library(lubridate)



trends <- gtrends(
  keyword = c('stranger things'),
  geo = "",
  gprop = 'web',
  time = "all"
)

trends %>%
  .$interest_over_time %>% 
  mutate(hits = ifelse(str_detect(hits, "<"),0,hits)) %>% 
  mutate(hits = as.numeric(hits)) %>% 
  ggplot(aes(x = date, y = hits, color=keyword)) +
  geom_line(size = 1) +
  #facet_wrap(~keyword, nrow=2) +
  theme_minimal()


library(rvest)

pagina <- read_html('https://en.wikipedia.org/wiki/Stranger_Things') 

lanzamientos <- pagina %>%  
  rvest::html_element("table.wikitable.plainrowheaders") %>% 
  rvest::html_table() %>% select(1,2,4) %>% 
  separate(3, into=c('drop','fecha'), sep='\\(') %>% 
  select(-drop) %>% 
  mutate(fecha = as.Date(str_replace(fecha, '\\)',''))) %>% 
  mutate(date = floor_date(fecha, "month", week_start = 1))


df <- trends$interest_over_time %>% 
  left_join(lanzamientos, by='date') %>% 
  filter(date>'2016-01-01') %>% 
  mutate(
    hits = as.numeric(ifelse(str_detect(hits, "<"),0,hits)),
    lanzamiento = ifelse(!is.na(Episodes),'Estreno temp', 0)     
  ) 

df %>% 
  ggplot(aes(x = date, y = hits)) +
  geom_line(size = 1) +
  geom_text(aes(x=as.POSIXct(fecha), label=lanzamiento, y=80),
            angle=90, vjust=-0.5, color='red', size=4)+
  geom_vline(aes(xintercept=as.POSIXct(fecha)), color='red')+
  theme_minimal()+
  theme(legend.position='none')+
  labs(title='Fechas de lanzamientos de Stranger things', 
       x='Fecha', y='Búsquedas web')


library(sknifedatar)
library(modeltime)
library(tidymodels)

# Split Data 80/20
splits <- initial_time_split(
  df %>% select(date,hits, lanzamiento), prop = 0.7)

# Model 1: auto_arima ----
autoarima <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(hits ~ date, data = training(splits))

prophet <-
  modeltime::prophet_boost() %>%
  set_engine('prophet_xgboost') %>% 
  fit(hits ~ date, data = training(splits))

prophet_xgb <-
  modeltime::prophet_boost() %>%
  set_engine('prophet_xgboost') %>% 
  fit(hits ~ date+lanzamiento, data = training(splits))


modeltime_table <- modeltime_table(
  modelo_autoarima = autoarima, 
  modelo_prophet = prophet,
  modelo_prophet_xgb = prophet_xgb
)

calibration_tbl <- modeltime_table %>% 
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = df
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = TRUE
  )





