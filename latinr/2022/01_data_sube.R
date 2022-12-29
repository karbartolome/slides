library(tidyverse)

# Datos flujo vehicular ---------------------------------------------------

url = 'https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ausa/flujo-vehicular-por-unidades-peaje-ausa/flujo-vehicular-'
url2 = 'https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ausa/flujo-vehicular-por-unidades-de-peaje-ausa/flujo-vehicular-'

url = 'https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ausa/flujo-vehicular-por-radares-ausa/flujo-vehicular-por-radares-'
peajes_2020 <- read_csv(paste0(url,'2020.csv')) %>% 
  mutate(fecha = as.Date(fecha)) %>% 
  group_by(fecha, hora=hora_inicio, estacion, tipo_vehiculo) %>% 
  summarise(cantidad_pasos=sum(cantidad_pasos)) %>% 
  ungroup()

peajes_2021 <- read_csv(paste0(url,'2021.csv')) %>% 
  janitor::clean_names() %>% 
  filter(hora_hh != 'Total') %>% 
  mutate(
    fecha = paste(dia_de_fecha_operativa, 
                   mes_de_fecha_operativa,
                  ano_de_fecha_operativa)) %>% 
  group_by(fecha, hora = hora_hh, estacion = id_peaje, tipo_vehiculo=cat_cobrada) %>% 
  summarise(cantidad_pasos = sum(pasos)) %>% 
  ungroup() %>% 
  mutate(fecha = parse_date(fecha, "%d %B %Y",locale=locale("es")))
  
  
peajes_2022 <- read_csv2(paste0(url2,'2022.csv')) %>% 
    janitor::clean_names() %>% 
    filter(hora_hh != 'Total') %>% 
    mutate(
      hora=ifelse(ano_de_fecha_operativa>='2022-02-01',cat_cobrada,hora_hh),
      tipo_vehiculo=ifelse(ano_de_fecha_operativa>='2022-02-01',hora_hh,cat_cobrada)
    ) %>% 
    group_by(fecha = ano_de_fecha_operativa, hora, estacion = id_peaje, tipo_vehiculo) %>%
    summarise(cantidad_pasos = sum(pasos)) %>%
    ungroup() %>%
    mutate(fecha = as.Date(fecha))

df <- bind_rows(
  peajes_2020 %>% mutate(hora=as.integer(hora)), 
  peajes_2021  %>% mutate(hora=as.integer(hora)), 
  peajes_2022  %>% mutate(hora=as.integer(hora))
)


df %>% 
  mutate(
    tipo_vehiculo = ifelse(
      str_detect(tipo_vehiculo,'Pesados') & 
        tipo_vehiculo!='Pesados 2 Ejes','Pesados +2 Ejes',
      tipo_vehiculo)) %>%
  group_by(fecha, tipo_vehiculo) %>% 
  summarise(n=sum(cantidad_pasos)) %>% 
  ungroup() %>% 
  group_by(tipo_vehiculo) %>% 
  filter(
    tipo_vehiculo %in% c('Auto','Moto','Pesados 2 Ejes','Pesados +2 Ejes')) %>% 
  timetk::plot_time_series(
    .date_var = fecha, .value=n, .interactive = FALSE)







# Por radar ---------------------------------------------------------------

url = 'https://archivos-datos.transporte.gob.ar/upload/Dat_Ab_Usos/dat-ab-usos-'


sube_2020 <- read_csv(paste0(url,'2020.csv')) %>% 
  janitor::clean_names() %>% 
  mutate(fecha = as.Date(dia_transporte)) %>% 
  select(-dia_transporte) %>% 
  filter(provincia == 'BUENOS AIRES') %>% 
  group_by(fecha, municipio, tipo_transporte) %>% 
  summarise(n=sum(cantidad))
  

sube_2021 <- read_csv(paste0(url,'2021.csv')) %>% 
  janitor::clean_names() %>% 
  mutate(fecha = as.Date(dia_transporte)) %>% 
  select(-dia_transporte) %>% 
  filter(provincia == 'BUENOS AIRES') %>% 
  group_by(fecha, municipio, tipo_transporte) %>% 
  summarise(n=sum(cantidad))


sube_2022 <- read_csv(paste0(url,'2022.csv')) %>% 
  janitor::clean_names() %>% 
  mutate(fecha = as.Date(dia_transporte)) %>% 
  select(-dia_transporte) %>% 
  filter(provincia == 'BUENOS AIRES') %>% 
  group_by(fecha, municipio, tipo_transporte) %>% 
  summarise(n=sum(cantidad))


df <- bind_rows(
  sube_2020, 
  sube_2021,
  sube_2022
)


df %>% 
  group_by(fecha, municipio) %>% 
  summarise(n=sum(n)) %>% 
  ungroup() %>% 
  filter(municipio %in% c('BERAZATEGUI', 'QUILMES','LA PLATA','AVELLANEDA')) %>% 
  group_by(municipio) %>% 
  timetk::plot_time_series(
    .date_var = fecha, .value=n, .interactive = FALSE)





