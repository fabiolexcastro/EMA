
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, readxl, qdap, hablar, Hmisc, Rfast)
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
pth <- '../tbl/0826_total_wide.xlsx'
tbl <- read_excel(pth)
dte <- basename(pth) %>% 
  parse_number()
com <- shapefile('../shp/base/bcs_comunas_geo.shp')
brr <- st_read('../shp/base/bcs_barrios_geo.shp') %>% 
  mutate(BARRIO = as.character(BARRIO))
lim <- shapefile('../shp/base/lim_cali.shp')
dst <- st_read('../shp/geocode/8-26_Destino.shp')
pth.lfm <- 'D:/univalle/movilidad/data/shp/own/movilidad/lfm'

# Mapa 19. Lineas de deseo ------------------------------------------------
map19 <- function(dfm){
  dfm <- tbl
  lbl <- data.frame(
    tipo = c('Para ir a atención médica',
             'Para ir a trabajar',
             'Para asuntos personales',
             'Para ir a estudiar',
             'Para realizar actividades deportivas o culturales',
             'Para realizar actividades sociales',
             'Para ir de compras',
             'Otro'),
    label = c('medica',
              'trabajar',
              'asunt_per',
              'estudiar',
              'deporte',
              'sociales',
              'compras',
              'otro'))
  lbl <- lbl %>% mutate(tipo = as.character(tipo))
  
  # Univalle
  dfm <- dfm %>% 
    dplyr::select(a_5, e_4) %>% 
    inner_join(., as.data.frame(brr)[,c('BARRIO', 'COMUNA')], by = c('a_5' = 'BARRIO')) %>% 
    group_by(COMUNA, e_4) %>% 
    dplyr::summarize(count = n()) %>% 
    ungroup() %>% 
    drop_na() %>% 
    inner_join(., lbl, by = c('e_4' = 'tipo')) %>% 
    dplyr::select(-e_4) %>% 
    spread(label, count) %>% 
    NAer() %>% 
    retype() %>% 
    as_tibble()

  # LFM
  lfm <- read_csv('D:/univalle/movilidad/tbl/LFM/BASE DE DATOS VIAJES EDIT 2.csv') %>% 
    mutate(BARRIO = iconv(BARRIO, to = 'latin1'),
           Comuna = parse_number(Comuna)) %>% 
    dplyr::select(Comuna, Tr:O) %>% 
    setNames(c('COMUNA', 'trabajar', 'estudiar', 'deporte', 'medica', 'sociales', 'asunt_per', 'otro'))
    
  colnames(dfm) %>% sort()
  colnames(lfm) %>% sort()
  
  # Join
  rsl <- bind_rows(dfm, lfm) %>% 
    group_by(COMUNA) %>% 
    summarize_all(.funs = sum) %>% 
    ungroup() %>% 
    NAer()
  rsl <- inner_join(st_as_sf(com), rsl, by = c('com' = 'COMUNA'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = paste0('m19_lineasDeseo_', dte), driver = 'ESRI Shapefile')
  print('Done!')
}

# Mapa 20. Causas de discapacidad ------------------------------------------
map20 <- function(){
  dfm <- tbl
  lbl <- data.frame(tipo = c('Por edad avanzada, envejecimiento',
                             'Porque nació así',
                             'Por enfermedad profesional',
                             'Por un accidente',
                             'Por una enfermedad',
                             'Por hechos violentos',
                             'No sabe',
                             'Por otra causa'),
                    label = c('edad',
                              'nacim',
                              'enf_pro',
                              'accidnt',
                              'enferm',
                              'violenc',
                              'nosabe',
                              'otra'))
  lbl <- lbl %>% mutate(tipo = as.character(tipo))
  
  # Univalle
  dfm <- dfm %>% 
    dplyr::select(a_5, d_2) %>% 
    inner_join(., lbl, by = c('d_2' = 'tipo')) %>% 
    dplyr::select(-d_2) %>% 
    inner_join(., as.data.frame(brr)[,c('BARRIO', 'COMUNA')], by = c('a_5' = 'BARRIO')) %>% 
    group_by(COMUNA, label) %>% 
    dplyr::summarize(count = n()) %>% 
    ungroup() %>% 
    drop_na() %>% 
    spread(label, count) %>% 
    NAer() %>% 
    retype()
  
  # LFM
  lfm <- read_excel('D:/univalle/movilidad/tbl/join/base_identificacion_CIDSE_LFM_v3.xlsx') %>% 
    filter(fuente == 'LFM')
  lbl <- data.frame(
    value = c(1:8, 98), 
    category = c('Nacio asi', 'Enfermedad', 'Accidente', 'Hechos violentos', 'Edad avanzada', 'Enfermedad profesional', 'Otra causa', 'No sabe', 'No responde')) %>% 
    mutate(category = as.character(category))
  lfm <- lfm %>% 
    dplyr::select(num_encuesta, fuente, dir_residencia, barrio_residencia, causadiscapacidad) %>% 
    inner_join(., lbl, by = c('causadiscapacidad' = 'value')) %>% 
    group_by(category, barrio_residencia) %>% 
    dplyr::summarize(count = n()) %>% 
    ungroup() %>% 
    inner_join(., as.data.frame(brr)[,c('BARRIO', 'COMUNA')], by = c('barrio_residencia' = 'BARRIO')) %>%
    group_by(COMUNA, category) %>% 
    dplyr::summarize(count = n()) %>% 
    ungroup() 
  lfm <- lfm %>% 
    mutate(category = gsub(' ', '_', category)) %>% 
    spread(category, count) %>% 
    setNames(c('COMUNA', 'accidnt', 'edad', 'enferm', 'violenc', 'nacim', 'nosabe', 'otra')) %>% 
    NAer() %>% 
    retype()
  
  # Join
  rsl <- bind_rows(dfm, lfm) %>% 
    group_by(COMUNA) %>% 
    summarise_all(.funs = sum) %>% 
    ungroup() %>% 
    NAer() %>% 
    retype()
  
  rsl <- inner_join(st_as_sf(com), rsl, by = c('com' = 'COMUNA'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = paste0('m20__causaDiscapacidad_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
map20()

# Mapa 21. Sexo de las PcD ----------------------------------------------
map21 <- function(){
  dfm <- tbl
  
  # Univalle
  dfm <- dfm %>% 
    dplyr::select(a_5, c_1) %>% 
    inner_join(., as.data.frame(brr)[,c('BARRIO', 'COMUNA')], by = c('a_5' = 'BARRIO')) %>% 
    dplyr::select(-a_5) %>% 
    group_by(COMUNA, c_1) %>% 
    dplyr::summarize(count = n()) %>% 
    ungroup() %>% 
    spread(c_1, count) %>% 
    dplyr::select(COMUNA, Hombre, Mujer, Otro) %>% 
    NAer() %>% 
    retype()
  
  # LFM
  lfm <- read_excel('D:/univalle/movilidad/tbl/join/base_identificacion_CIDSE_LFM_v3.xlsx') %>% 
    filter(fuente == 'LFM')
  lbl <- data.frame(values = c(1, 2, 'HOMBRE', 'MUJER'),
                    category = c('Hombre', 'Mujer', 'Hombre', 'Mujer')) %>% 
    mutate(values = as.character(values))
  lfm <- lfm %>% 
    dplyr::select(num_encuesta, fuente, dir_residencia, barrio_residencia, sexo_dis) %>% 
    inner_join(., lbl, by = c('sexo_dis' = 'values')) %>% 
    inner_join(., as.data.frame(brr)[,c('BARRIO', 'COMUNA')], by = c('barrio_residencia' = 'BARRIO')) %>% 
    group_by(COMUNA, category) %>% 
    dplyr::summarise(count = n()) %>% 
    ungroup() %>% 
    spread(category, count) %>% 
    NAer() %>% 
    retype() 
    
  # Join
  rsl <- bind_rows(dfm, lfm) %>% 
    group_by(COMUNA) %>% 
    summarize_all(.funs = sum) %>% 
    ungroup() %>% 
    NAer() %>% 
    retype()
  rsl <- inner_join(st_as_sf(com), rsl, by = c('com' = 'COMUNA'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = paste0('m21_sexo_', dte), driver = 'ESRI Shapefile')
  print('Done!')
  
}

# Mapa 22. Rangos edad  ---------------------------------------------------
map22 <- function(){
  
  dfm <- tbl
  
  # Univalle
  rcl <- pull(dfm, c_2) %>% 
    as.numeric() %>% 
    cut2(., seq(1, 100, 10)) 
  rng <- data.frame(ranges = sort(as.character(unique(rcl))), 
                    category = seq(10, 100, 10)) %>% 
    arrange(category)
  dfm <- dfm %>% 
    dplyr::select(a_5, c_2) %>% 
    mutate(rango = rcl) %>% 
    inner_join(., rng, by = c('rango' = 'ranges'))
  dfm <- dfm %>% 
    inner_join(., as.data.frame(brr)[,c('BARRIO', 'COMUNA')], by = c('a_5' = 'BARRIO')) %>% 
    group_by(COMUNA, category) %>% 
    dplyr::summarise(count = n()) %>% 
    ungroup() %>% 
    mutate(category = paste0('c_', category)) %>% 
    spread(category, count) %>% 
    NAer() %>% 
    retype()
  
  # LFM
  lfm <- read_excel('D:/univalle/movilidad/tbl/join/base_identificacion_CIDSE_LFM_v3.xlsx') %>% 
    dplyr::select(num_encuesta, fuente, dir_residencia, barrio_residencia, edad_dis)
  rcl <- pull(lfm, edad_dis) %>% 
    cut2(., seq(9, 98, 10)) 
  rng <- data.frame(ranges = as.character(unique(rcl)), category = c(60,20,40,50,10,30,90,70,80)) %>% 
    arrange(category)
  lfm <- lfm %>% 
    mutate(ranges = rcl) %>% 
    inner_join(., rng, by = c('ranges' = 'ranges')) %>% 
    inner_join(., as.data.frame(brr)[,c('BARRIO', 'COMUNA')], by = c('barrio_residencia' = 'BARRIO')) %>% 
    group_by(COMUNA, category) %>% 
    dplyr::summarise(count = n()) %>% 
    ungroup() %>% 
    mutate(category = paste0('c_', category)) %>% 
    spread(category, count) %>% 
    NAer() %>% 
    retype() 
    
  # Join
  rsl <- bind_rows(dfm, lfm) %>% 
    group_by(COMUNA) %>% 
    summarize_all(.funs = sum) %>% 
    ungroup() %>% 
    NAer() %>% 
    retype()
  rsl <- inner_join(st_as_sf(com), rsl, by = c('com' = 'COMUNA'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = paste0('m22_rangosEdad_', dte), driver = 'ESRI Shapefile')
  print('Done!')
  
}

# Mapa 23. Tiempo promedio espera -----------------------------------------
map23 <- function(){
  
  dfm <- tbl
  
  # Univalle
  dfm <- dfm %>% 
    dplyr::select(a_5, e_8_1:e_8_10, e_15_1:e_15_10) %>% 
    NAer() %>% 
    retype() %>% 
    as_tibble()
  dfm[dfm == 0] <- NA
  avg <- rowMeans(dfm[,2:ncol(dfm)], na.rm = TRUE)
  dfm <- dfm %>% 
    transmute(a_5,
              t_espera = avg) %>% 
    inner_join(., as.data.frame(brr)[,c('BARRIO', 'COMUNA')], by = c('a_5' = 'BARRIO')) %>% 
    group_by(COMUNA) %>% 
    dplyr::summarise(t_espera = mean(t_espera, na.rm = TRUE)) %>% 
    ungroup()
    
  # LFM
  lfm <- read_csv('D:/univalle/movilidad/tbl/join/tiempo_lfm.csv')
  lfm <- lfm %>% 
    dplyr::select(barrio_residencia, transporte_viaje, tespera) %>% 
    inner_join(., as.data.frame(brr)[,c('BARRIO', 'COMUNA')], by = c('barrio_residencia' = 'BARRIO')) %>% 
    group_by(COMUNA) %>% 
    summarise(t_espera = mean(tespera, na.rm = TRUE)) %>% 
    ungroup()
  
  # Join
  rsl <- bind_rows(dfm, lfm) %>% 
    group_by(COMUNA) %>% 
    summarise_all(.funs = mean) %>% 
    ungroup() %>% 
    mutate(COMUNA = as.numeric(as.character(COMUNA)))
  rsl <- inner_join(st_as_sf(com), rsl, by = c('com' = 'COMUNA'))  
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = paste0('m23_tespera_', dte), driver = 'ESRI Shapefile')
  print('Done!')
}
map23()

# Mapa 24. Tiempo promedio trayecto ---------------------------------------
map24 <- function(){
  
  dfm <- tbl
  
  # Univalle
  dfm <- dfm %>% 
    dplyr::select(a_5, e_9_1:e_9_10, e_16_1:e_16_10) %>% 
    NAer() %>% 
    retype() %>% 
    as_tibble()
  dfm[dfm == 0] <- NA
  avg <- rowMeans(dfm[,2:ncol(dfm)], na.rm = TRUE)
  dfm <- dfm %>% 
    transmute(a_5,
              tviaje = avg) %>% 
    inner_join(., as.data.frame(brr)[,c('BARRIO', 'COMUNA')], by = c('a_5' = 'BARRIO')) %>% 
    group_by(COMUNA) %>% 
    dplyr::summarise(tviaje = mean(tviaje, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(COMUNA = as.numeric(as.character(COMUNA)))
  
  # LFM
  lfm <- read_csv('D:/univalle/movilidad/tbl/join/tiempo_lfm.csv')
  lfm <- lfm %>% 
    dplyr::select(barrio_residencia, transporte_viaje, tviaje) %>% 
    inner_join(., as.data.frame(brr)[,c('BARRIO', 'COMUNA')], by = c('barrio_residencia' = 'BARRIO')) %>% 
    group_by(COMUNA) %>% 
    summarise(tviaje = mean(tviaje, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(COMUNA = as.numeric(as.character(COMUNA)))
  
  # Join
  rsl <- bind_rows(dfm, lfm) %>% 
    group_by(COMUNA) %>% 
    summarise_all(.funs = mean) %>% 
    ungroup() %>% 
    mutate(COMUNA = as.numeric(as.character(COMUNA)))
  rsl <- inner_join(st_as_sf(com), rsl, by = c('com' = 'COMUNA'))  
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = paste0('m24_tviaje_', dte), driver = 'ESRI Shapefile')
  print('Done!')
}
map24()

# Mapa 25. Tiempo promedio total de desplazamiento ------------------------
map25 <- function(){
  
  dfm <- tbl
  
  # Univalle
  df1 <- dfm %>% 
    dplyr::select(a_5, e_8_1:e_8_10, e_15_1:e_15_10, ) %>% 
    NAer() %>% 
    retype() %>% 
    as_tibble()
  df1[df1 == 0] <- NA
  df2 <- dfm %>% 
    dplyr::select(a_5, e_9_1:e_9_10, e_16_1:e_16_10) %>% 
    NAer() %>% 
    retype() %>% 
    as_tibble()
  df2[df2 == 0] <- NA
  
  avg1 <- rowMeans(df1[,2:ncol(df1)], na.rm = TRUE)
  avg2 <- rowMeans(df1[,2:ncol(df2)], na.rm = TRUE)
  
  dfm <- dfm %>% 
    transmute(a_5,
              t_espera = avg1,
              t_trayecto = avg2,
              t_total = t_espera + t_trayecto) %>% 
    inner_join(., as.data.frame(brr)[,c('BARRIO', 'COMUNA')], by = c('a_5' = 'BARRIO')) %>% 
    group_by(COMUNA) %>% 
    dplyr::summarise(t_total = mean(t_total, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(COMUNA = as.numeric(as.character(COMUNA)))
  
  # LFM
  lfm <- read_csv('D:/univalle/movilidad/tbl/join/tiempo_lfm.csv')
  lfm <- lfm %>% 
    dplyr::select(barrio_residencia, transporte_viaje, tiempototal) %>% 
    inner_join(., as.data.frame(brr)[,c('BARRIO', 'COMUNA')], by = c('barrio_residencia' = 'BARRIO')) %>% 
    group_by(COMUNA) %>% 
    summarise(t_total = mean(tiempototal, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(COMUNA = as.numeric(as.character(COMUNA)))
  
  # Join
  rsl <- bind_rows(dfm, lfm) %>% 
    group_by(COMUNA) %>% 
    summarise_all(.funs = mean) %>% 
    ungroup() %>% 
    mutate(COMUNA = as.numeric(as.character(COMUNA)))
  rsl <- inner_join(st_as_sf(com), rsl, by = c('com' = 'COMUNA'))  
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = paste0('m25_ttotal_', dte), driver = 'ESRI Shapefile')
  print('Done!')
}
map25()





