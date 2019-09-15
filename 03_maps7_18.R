
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

# Categorias
ctg <- data.frame(tipo = c('Ninguno', 'Bajo', 'Medio', 'Alto', 'Muy Alto'),
                  valor = 1:5)
ctg <- ctg %>% 
  mutate(tipo = as.character(tipo))

# Mapa 7. Barreras ida moto y bicicleta -----------------------------------
map07 <- function(){
  dfm <- tbl
  
  # Univalle
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, a_5, f_19:f_33)
  lbl <- data.frame(nameCol = paste0('f_', c(19:24, 26:33)),
                    barrera = c('m_abordar', 'm_ubicar', 'm_manejar', 'm_retr', 'm_leer', 'm_pr_pav', 'm_bajar',
                                'b_abordar', 'b_ubicar', 'b_leer', 'b_movili', 'b_pr_pav', 'b_dcdrrecor', 'b_bajar'))
  nms <- as.character(lbl$barrera)
  dfm <- inner_join(dfm, as.data.frame(brr) %>% dplyr::select(BARRIO, COMUNA), by = c('a_5' = 'BARRIO'))
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'barrio', nms, 'comuna')) %>% 
    dplyr::select(-id_encuesta) %>% 
    gather(dificultad, tipo, -comuna, -barrio) %>% 
    drop_na() %>%
    inner_join(., ctg, by = 'tipo') %>% 
    group_by(comuna, dificultad) %>% 
    dplyr::summarize(promedio = mean(valor, na.rm = TRUE)) %>% 
    ungroup() %>% 
    spread(dificultad, promedio) %>% 
    NAer() %>% 
    retype() 
  
  colnames(dfm)
  
  # LFM
  lfm_mto <- st_read('D:/univalle/movilidad/data/shp/own/movilidad/lfm/07_barreras_ida_moto.shp')
  lfm_bcc <- st_read('D:/univalle/movilidad/data/shp/own/movilidad/lfm/07_barreras_ida_bicicleta.shp')
  sh1 <- lfm_mto %>% 
    dplyr::select(BARRIO, Ave_B1:Ave_B7) %>% 
    setNames(c('BARRIO', 'm_abordar', 'm_ubicar', 'm_manejar', 'm_retr', 'm_leer', 'm_pr_pav', 'm_bajar', 'geometry')) %>% 
    mutate(suma = m_abordar + m_ubicar + m_manejar + m_retr + m_leer + m_pr_pav + m_bajar) %>% 
    filter(suma > 0) %>% 
    dplyr::select(-suma) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    inner_join(., as.data.frame(brr)[,c('COMUNA', 'BARRIO')], by = c('BARRIO' = 'BARRIO')) %>%
    dplyr::select(-BARRIO) %>% 
    group_by(COMUNA) %>% 
    summarize_all(.funs = mean) %>% 
    ungroup()
  sh2 <- lfm_bcc %>% 
    dplyr::select(BARRIO, Ave_B1:Ave_B6) %>% 
    setNames(c('BARRIO', 'b_abordar', 'b_ubicar', 'b_leer', 'b_movili', 'b_pr_pav', 'b_bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    mutate(suma = b_abordar + b_ubicar + b_leer + b_movili + b_pr_pav + b_bajar) %>% 
    filter(suma > 0) %>% 
    dplyr::select(-suma) %>% 
    dplyr::select(-geometry) %>% 
    as_tibble() %>% 
    inner_join(., as.data.frame(brr)[,c('COMUNA', 'BARRIO')], by = c('BARRIO' = 'BARRIO')) %>%
    dplyr::select(-BARRIO) %>% 
    group_by(COMUNA) %>% 
    summarize_all(.funs = sum) %>% 
    ungroup()
  sh3 <- inner_join(sh1, sh2, by = c('COMUNA')) %>% 
    rename(comuna = COMUNA) %>% 
    mutate(comuna = as.numeric(as.character(comuna)))
  
  # Join
  rsl <- bind_rows(dfm, sh3)
  rsl <- inner_join(st_as_sf(com), rsl, by = c('com' = 'comuna'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = paste0('m07_brr_ida_motobici_', dte), driver = 'ESRI Shapefile')
  print('Done!')
}
map07()

# Mapa 8. Barreras regreso moto y bicicleta -------------------------------
map08 <- function(){
  sft <- dst
  dfm <- tbl
  
  # Univalle
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, f_19:f_33)
  int <- raster::intersect(as(sft, 'Spatial'), as(brr, 'Spatial')) 
  int <- st_as_sf(int) %>% 
    dplyr::select(ID_ENCUEST, COMUNA, BARRIO) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  lbl <- data.frame(nameCol = paste0('f_', c(19:24, 26:33)),
                    barrera = c('m_abrdr', 'm_ubicr', 'm_manjr', 'm_retr', 'm_leer', 'm_pr_pv', 'm_bajar',
                                'b_abrdr', 'b_ubicr', 'b_leer', 'b_movil', 'b_pr_pv', 'b_dcdrr', 'b_bajar'))
  nms <- as.character(lbl$barrera)
  dfm <- inner_join(int, dfm, by = c('ID_ENCUEST' = 'id_encuesta'))
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'comuna', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta) %>% 
    gather(dificultad, tipo, -barrio, -comuna) %>% 
    drop_na() %>% 
    dplyr::select(-barrio) %>% 
    inner_join(., ctg, by = 'tipo') %>%
    group_by(comuna, dificultad) %>% 
    dplyr::summarize(promedio = mean(valor, na.rm = TRUE)) %>% 
    ungroup() %>% 
    spread(dificultad, promedio) %>% 
    NAer() %>% 
    as_tibble() %>% 
    retype()
  
  # LFM
  lfm_mto <- st_read('D:/univalle/movilidad/data/shp/own/movilidad/lfm/08_barreras_regreso_moto.shp')
  lfm_bcc <- st_read('D:/univalle/movilidad/data/shp/own/movilidad/lfm/08_barreras_regreso_bici.shp')
  sh1 <- lfm_mto %>% 
    dplyr::select(BARRIO, Ave_B1:Ave_B7) %>% 
    setNames(c('BARRIO', 'm_abrdr', 'm_ubicr', 'm_manjr', 'm_retr', 'm_leer', 'm_pr_pv', 'm_bajar', 'geometry')) %>% 
    mutate(suma = m_abrdr + m_ubicr + m_manjr + m_retr + m_leer + m_pr_pv + m_bajar) %>% 
    filter(suma > 0) %>% 
    dplyr::select(-suma) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    inner_join(., as.data.frame(brr)[,c('COMUNA', 'BARRIO')], by = c('BARRIO' = 'BARRIO')) %>%
    dplyr::select(-BARRIO) %>% 
    group_by(COMUNA) %>% 
    summarize_all(.funs = mean, na.rm = TRUE) %>% 
    ungroup()
  sh2 <- lfm_bcc %>% 
    dplyr::select(BARRIO, Ave_B1:Ave_B6) %>% 
    setNames(c('BARRIO', 'b_abrdr', 'b_ubicr', 'b_leer', 'b_movil', 'b_pr_pv', 'b_bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    mutate(suma = b_abrdr + b_ubicr + b_leer + b_movil + b_pr_pv + b_bajar) %>% 
    filter(suma > 0) %>% 
    dplyr::select(-suma) %>%
    dplyr::select(-geometry) %>% 
    inner_join(., as.data.frame(brr)[,c('COMUNA', 'BARRIO')], by = c('BARRIO' = 'BARRIO')) %>%
    dplyr::select(-BARRIO) %>% 
    group_by(COMUNA) %>% 
    summarize_all(.funs = mean) %>% 
    ungroup()
  sh3 <- inner_join(sh1, sh2, by = c('COMUNA')) %>% 
    rename(comuna = COMUNA) %>% 
    mutate(comuna = as.numeric(as.character(comuna)))
  
  # Join
  rsl <- bind_rows(dfm, sh3) %>% 
    arrange(comuna) %>% 
    group_by(comuna) %>% 
    summarize_all(.funs = sum) %>% 
    NAer()
  rsl <- inner_join(st_as_sf(com), rsl, by = c('com' = 'comuna'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = paste0('m08_brr_regreso_motobici_', dte), driver = 'ESRI Shapefile')
  print('Done!')
  
}
map08()

# Mapa 9. Barreras ida taxi -----------------------------------------------
map09 <- function(){
  
  dfm <- tbl
  
  # Univalle
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, a_5, f_34:f_41)
  lbl <- data.frame(nameCol = paste0('f_', c(34:36, 38:41)),
                    barrera = c('abordar', 'a_priorit', 'ub_asiento', 'comu_dest', 'pagar', 'leer', 'bajar'))
  nms <- as.character(lbl$barrera)
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta) %>% 
    inner_join(., as.data.frame(brr)[,c('BARRIO', 'COMUNA')], by = c('barrio' = 'BARRIO')) %>% 
    dplyr::select(-barrio) %>% 
    gather(dificultad, tipo, -COMUNA) %>% 
    drop_na() %>% 
    inner_join(., ctg, by = 'tipo') %>% 
    group_by(COMUNA, dificultad) %>% 
    dplyr::summarize(promedio = mean(valor, na.rm = TRUE)) %>% 
    ungroup() %>% 
    spread(dificultad, promedio) %>% 
    NAer() %>% 
    retype()
  
  # LFM
  lfm <- st_read('D:/univalle/movilidad/data/shp/own/movilidad/lfm/09_barreras_ida_taxi.shp') %>% 
    dplyr::select(ID_COMUNA, Ave_B1:Ave_B7) %>% 
    setNames(c('COMUNA', 'abordar', 'a_priorit', 'ub_asiento', 'comu_dest', 'pagar', 'leer', 'bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    mutate(suma = abordar + a_priorit + ub_asiento + comu_dest + pagar + leer + bajar) %>% 
    filter(suma > 0) %>% 
    dplyr::select(-suma) %>%
    dplyr::select(-geometry) %>% 
    group_by(COMUNA) %>% 
    summarise_all(.funs = mean) %>% 
    ungroup()
  
  sort(colnames(dfm))
  sort(colnames(lfm))
  
  # Join
  rsl <- bind_rows(dfm, lfm)
  rsl <- inner_join(st_as_sf(com), dfm, by = c('com' = 'COMUNA'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = paste0('m09_brr_ida_taxi_', dte), driver = 'ESRI Shapefile')
  print('Done!')
}
map09()

# Mapa 10. Barreras regreso taxi ------------------------------------------
map10 <- function(){
  sft <- dst
  dfm <- tbl
  
  # Univalle
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, f_34:f_41)
  int <- raster::intersect(as(sft, 'Spatial'), as(brr, 'Spatial')) 
  int <- st_as_sf(int) %>% 
    dplyr::select(ID_ENCUEST, COMUNA, BARRIO) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  lbl <- data.frame(nameCol = paste0('f_', c(34:36, 38:41)),
                    barrera = c('abordar', 'a_priorit', 'ub_asiento', 'comu_dest', 'pagar', 'leer', 'bajar'))
  nms <- as.character(lbl$barrera)
  dfm <- inner_join(int, dfm, by = c('ID_ENCUEST' = 'id_encuesta'))
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'COMUNA', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta, -barrio) %>% 
    gather(dificultad, tipo, -COMUNA) %>% 
    drop_na() %>% 
    inner_join(., ctg, by = 'tipo') %>% 
    group_by(COMUNA, dificultad) %>% 
    dplyr::summarize(promedio = mean(valor, na.rm = TRUE)) %>% 
    ungroup() %>% 
    spread(dificultad, promedio) %>% 
    NAer() %>% 
    as_tibble() %>% 
    retype()
  
  # LFM
  lfm <- st_read('D:/univalle/movilidad/data/shp/own/movilidad/lfm/10_barreras_regreso_taxi.shp') %>% 
    dplyr::select(ID_COMUNA, Ave_B1:Ave_B7) %>% 
    setNames(c('COMUNA', 'abordar', 'a_priorit', 'ub_asiento', 'comu_dest', 'pagar', 'leer', 'bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(suma = abordar + a_priorit + ub_asiento + comu_dest + pagar + leer + bajar) %>% 
    filter(suma > 0) %>% 
    dplyr::select(-suma) %>%
    group_by(COMUNA) %>% 
    summarise_all(.funs = mean) %>% 
    ungroup() 
  
  # Join
  rsl <- bind_rows(dfm, lfm) %>% 
    arrange(COMUNA) %>% 
    group_by(COMUNA) %>% 
    summarize_all(.funs = sum)
  rsl <- inner_join(st_as_sf(com), rsl, by = c('com' = 'COMUNA'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = paste0('m10_brr_regreso_taxi_', dte), driver = 'ESRI Shapefile')
  print('Done!') 
}
map10()

# Mapa 11. Barreras ida Jeep ----------------------------------------------
map11 <- function(){
  
  dfm <- tbl
  
  # Univalle
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, a_5, f_78:f_87)
  lbl <- data.frame(nameCol = paste0('f_', c(78:82, 84:87)),
                    barrera = c('ubic', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'com_dst', 'pgr_trn', 'leer', 'bajar'))
  nms <- as.character(lbl$barrera)
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta) %>% 
    inner_join(., as.data.frame(brr)[,c('BARRIO', 'COMUNA')], by = c('barrio' = 'BARRIO')) %>% 
    dplyr::select(-barrio) %>% 
    gather(dificultad, tipo, -COMUNA) %>% 
    drop_na() %>% 
    inner_join(., ctg, by = 'tipo') %>% 
    group_by(COMUNA, dificultad) %>% 
    dplyr::summarize(promedio = mean(valor, na.rm = TRUE)) %>% 
    ungroup() %>% 
    spread(dificultad, promedio) %>% 
    NAer() %>% 
    retype()
  
  # LFM
  lfm <- st_read('D:/univalle/movilidad/data/shp/own/movilidad/lfm/11_barreras_ida_jeepi.shp') %>% 
    dplyr::select(ID_COMUNA, Ave_B1:Ave_B9) %>% 
    setNames(c('ID_COMUNA', 'ubic', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'com_dst', 'pgr_trn', 'leer', 'bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(suma = ubic + llgr_pard + a_prio + abrdr_vh + ubicar_asn + com_dst + pgr_trn + leer + bajar) %>% 
    filter(suma > 0) %>% 
    dplyr::select(-suma) %>%
    group_by(ID_COMUNA) %>% 
    summarise_all(.funs = mean) %>% 
    ungroup()
  
  sort(colnames(dfm))
  sort(colnames(lfm))
  
  # Join
  rsl <- bind_rows(dfm, lfm)
  rsl <- inner_join(st_as_sf(com), dfm, by = c('com' = 'COMUNA'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = paste0('m11_brr_ida_jeep_', dte), driver = 'ESRI Shapefile')
  print('Done!')
}
map11()

# Mapa 12. Barreras retorno Jeep, Guala o Vehiculo informal --------------
map12 <- function(){
  sft <- dst
  dfm <- tbl
  
  # Univalle
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, f_78:f_87)
  int <- raster::intersect(as(sft, 'Spatial'), as(brr, 'Spatial')) 
  int <- st_as_sf(int) %>% 
    dplyr::select(ID_ENCUEST, COMUNA, BARRIO) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  lbl <- data.frame(nameCol = paste0('f_', c(78:82, 84:87)),
                    barrera = c('ubic', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'com_dst', 'pgr_trn', 'leer', 'bajar'))
  nms <- as.character(lbl$barrera)
  dfm <- inner_join(int, dfm, by = c('ID_ENCUEST' = 'id_encuesta'))
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'COMUNA', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta, -barrio) %>% 
    gather(dificultad, tipo, -COMUNA) %>% 
    drop_na() %>% 
    inner_join(., ctg, by = 'tipo') %>% 
    group_by(COMUNA, dificultad) %>% 
    dplyr::summarize(promedio = mean(valor, na.rm = TRUE)) %>% 
    ungroup() %>% 
    spread(dificultad, promedio) %>% 
    NAer() %>% 
    as_tibble() %>% 
    retype()
  
  # LFM
  lfm <- st_read('D:/univalle/movilidad/data/shp/own/movilidad/lfm/12_barreras_destino_jeepi.shp') %>% 
    dplyr::select(ID_COMUNA, Ave_B1:Ave_B9) %>% 
    setNames(c('COMUNA', 'ubic', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'com_dst', 'pgr_trn', 'leer', 'bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    dplyr::select(-geometry) %>% 
    as_tibble() %>% 
    mutate(suma = ubic + llgr_pard + a_prio + abrdr_vh + ubicar_asn + com_dst + pgr_trn + leer + bajar) %>% 
    filter(suma > 0) %>% 
    dplyr::select(-suma) %>%
    group_by(COMUNA) %>% 
    summarise_all(.funs = mean) %>% 
    ungroup() 
  
  # Join
  rsl <- bind_rows(dfm, lfm) %>% 
    arrange(COMUNA) %>% 
    group_by(COMUNA) %>% 
    summarize_all(.funs = mean)
  rsl <- inner_join(st_as_sf(com), rsl, by = c('com' = 'COMUNA'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = paste0('m12_brr_regreso_jeep_', dte), driver = 'ESRI Shapefile')
  print('Done!') 
  
}

# Mapa 13. Barreras ida MIO articulado ------------------------------------
map13 <- function(){
  dfm <- tbl
  
  # Univalle
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, a_5, f_98:f_108)
  lbl <- data.frame(nameCol = paste0('f_', 98:108),
                    barrera = c('ubicar', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'uso_prio', 'dcd_rcrrd', 'com_dst', 'pagar', 'leer', 'bajar'))
  nms <- as.character(lbl$barrera)
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta) %>% 
    inner_join(., as.data.frame(brr)[,c('BARRIO', 'COMUNA')], by = c('barrio' = 'BARRIO')) %>% 
    dplyr::select(-barrio) %>% 
    gather(dificultad, tipo, -COMUNA) %>% 
    drop_na() %>% 
    inner_join(., ctg, by = 'tipo') %>% 
    group_by(COMUNA, dificultad) %>% 
    dplyr::summarize(promedio = mean(valor, na.rm = TRUE)) %>% 
    ungroup() %>% 
    spread(dificultad, promedio) %>% 
    NAer() %>% 
    retype()
  
  # LFM
  lfm <- st_read('D:/univalle/movilidad/data/shp/own/movilidad/lfm/13_barreras_ida_MIOarticulado.shp') %>% 
    dplyr::select(ID_COMUNA, Ave_B1:Ave_B11) %>% 
    setNames(c('COMUNA', 'ubicar', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'uso_prio', 'dcd_rcrrd', 'com_dst', 'pagar', 'leer', 'bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    dplyr::select(-geometry) %>% 
    as_tibble() %>% 
    mutate(suma = ubicar + llgr_pard + a_prio + abrdr_vh + ubicar_asn + uso_prio + dcd_rcrrd + com_dst + pagar + leer + bajar) %>% 
    filter(suma > 0) %>% 
    dplyr::select(-suma) %>%
    group_by(COMUNA) %>% 
    summarise_all(.funs = mean) %>% 
    ungroup()
  
  sort(colnames(dfm))
  sort(colnames(lfm))
  
  # Join
  rsl <- bind_rows(dfm, lfm) %>% 
    arrange(COMUNA)
  rsl <- inner_join(st_as_sf(com), dfm, by = c('com' = 'COMUNA'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = paste0('m13_brr_ida_MIOart_', dte), driver = 'ESRI Shapefile')
  print('Done!')
  
}
map13()

# Mapa 14. Barreras retorno MIO articulado --------------------------------
map14 <- function(){
  sft <- dst
  dfm <- tbl
  
  # Univalle
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, f_98:f_108)
  int <- raster::intersect(as(sft, 'Spatial'), as(brr, 'Spatial')) 
  int <- st_as_sf(int) %>% 
    dplyr::select(ID_ENCUEST, COMUNA, BARRIO) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  lbl <- data.frame(nameCol = paste0('f_', 98:108),
                    barrera = c('ubicar', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'uso_prio', 'dcd_rcrrd', 'com_dst', 'pagar', 'leer', 'bajar'))
  nms <- as.character(lbl$barrera)
  dfm <- inner_join(int, dfm, by = c('ID_ENCUEST' = 'id_encuesta'))
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'COMUNA', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta, -barrio) %>% 
    gather(dificultad, tipo, -COMUNA) %>% 
    drop_na() %>% 
    inner_join(., ctg, by = 'tipo') %>% 
    group_by(COMUNA, dificultad) %>% 
    dplyr::summarize(promedio = mean(valor, na.rm = TRUE)) %>% 
    ungroup() %>% 
    spread(dificultad, promedio) %>% 
    NAer() %>% 
    as_tibble() %>% 
    retype()
  
  # LFM
  lfm <- st_read('D:/univalle/movilidad/data/shp/own/movilidad/lfm/14_barreras_regreso_MIOarticulado.shp') %>% 
    dplyr::select(ID_COMUNA, Ave_B1:Ave_B11) %>% 
    setNames(c('COMUNA', 'ubicar', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'uso_prio', 'dcd_rcrrd', 'com_dst', 'pagar', 'leer', 'bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    dplyr::select(-geometry) %>% 
    as_tibble() %>% 
    mutate(suma = ubicar + llgr_pard + a_prio + abrdr_vh + ubicar_asn + uso_prio + dcd_rcrrd + com_dst + pagar + leer + bajar) %>% 
    filter(suma > 0) %>% 
    dplyr::select(-suma) %>%
    group_by(COMUNA) %>% 
    summarise_all(.funs = mean) %>% 
    ungroup() 
  
  # Join
  rsl <- bind_rows(dfm, lfm) %>% 
    arrange(COMUNA) %>% 
    group_by(COMUNA) %>% 
    summarize_all(.funs = mean)
  rsl <- inner_join(st_as_sf(com), rsl, by = c('com' = 'COMUNA'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = paste0('m14_brr_regreso_MIOart_', dte), driver = 'ESRI Shapefile')
  print('Done!') 
  
}
map14()

# Mapa 15. Barreras ida MIO complementario --------------------------------
map15 <- function(){
  dfm <- tbl
  
  # Univalle
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, a_5, f_120:f_130)
  lbl <- data.frame(nameCol = paste0('f_', 120:130),
                    barrera = c('ubicar', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'uso_prio', 'dcd_rcrrd', 'com_dst', 'pagar', 'leer', 'bajar'))
  nms <- as.character(lbl$barrera)
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta) %>% 
    inner_join(., as.data.frame(brr)[,c('BARRIO', 'COMUNA')], by = c('barrio' = 'BARRIO')) %>% 
    dplyr::select(-barrio) %>% 
    gather(dificultad, tipo, -COMUNA) %>% 
    drop_na() %>% 
    inner_join(., ctg, by = 'tipo') %>% 
    group_by(COMUNA, dificultad) %>% 
    dplyr::summarize(promedio = mean(valor, na.rm = TRUE)) %>% 
    ungroup() %>% 
    spread(dificultad, promedio) %>% 
    NAer() %>% 
    retype()
  
  # LFM
  lfm <- st_read('D:/univalle/movilidad/data/shp/own/movilidad/lfm/15_barreras_ida_MIOcomplementario.shp') %>% 
    dplyr::select(ID_COMUNA, Ave_B1:Ave_B11) %>% 
    setNames(c('COMUNA', 'ubicar', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'uso_prio', 'dcd_rcrrd', 'com_dst', 'pagar', 'leer', 'bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    dplyr::select(-geometry) %>% 
    as_tibble() %>% 
    mutate(suma = ubicar + llgr_pard + a_prio + abrdr_vh + ubicar_asn + uso_prio + dcd_rcrrd + com_dst + pagar + leer + bajar) %>% 
    filter(suma > 0) %>% 
    dplyr::select(-suma) %>%
    group_by(COMUNA) %>% 
    summarise_all(.funs = mean) %>% 
    ungroup()
  
  sort(colnames(dfm))
  sort(colnames(lfm))
  
  # Join
  rsl <- bind_rows(dfm, lfm) %>% 
    arrange(COMUNA)
  rsl <- inner_join(st_as_sf(com), dfm, by = c('com' = 'COMUNA'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = paste0('m15_brr_ida_MIOcmp_', dte), driver = 'ESRI Shapefile')
  print('Done!')
  
}
map15()

# Mapa 16. Barreras regreso MIO complementario ----------------------------
map16 <- function(){
  sft <- dst
  dfm <- tbl
  
  # Univalle
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, f_120:f_130)
  int <- raster::intersect(as(sft, 'Spatial'), as(brr, 'Spatial')) 
  int <- st_as_sf(int) %>% 
    dplyr::select(ID_ENCUEST, COMUNA, BARRIO) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  lbl <- data.frame(nameCol = paste0('f_', 120:130),
                    barrera = c('ubicar', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'uso_prio', 'dcd_rcrrd', 'com_dst', 'pagar', 'leer', 'bajar'))
  nms <- as.character(lbl$barrera)
  dfm <- inner_join(int, dfm, by = c('ID_ENCUEST' = 'id_encuesta'))
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'COMUNA', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta, -barrio) %>% 
    gather(dificultad, tipo, -COMUNA) %>% 
    drop_na() %>% 
    inner_join(., ctg, by = 'tipo') %>% 
    group_by(COMUNA, dificultad) %>% 
    dplyr::summarize(promedio = mean(valor, na.rm = TRUE)) %>% 
    ungroup() %>% 
    spread(dificultad, promedio) %>% 
    NAer() %>% 
    as_tibble() %>% 
    retype()
  
  # LFM
  lfm <- st_read('D:/univalle/movilidad/data/shp/own/movilidad/lfm/16_barreras_regreso_MIOcomplementario.shp') %>% 
    dplyr::select(ID_COMUNA, Ave_B1:Ave_B11) %>% 
    setNames(c('COMUNA', 'ubicar', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'uso_prio', 'dcd_rcrrd', 'com_dst', 'pagar', 'leer', 'bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    dplyr::select(-geometry) %>% 
    as_tibble() %>% 
    mutate(suma = ubicar + llgr_pard + a_prio + abrdr_vh + ubicar_asn + uso_prio + dcd_rcrrd + com_dst + pagar + leer + bajar) %>% 
    filter(suma > 0) %>% 
    dplyr::select(-suma) %>%
    group_by(COMUNA) %>% 
    summarise_all(.funs = mean) %>% 
    ungroup() 
  
  # Join
  rsl <- bind_rows(dfm, lfm) %>% 
    arrange(COMUNA) %>% 
    group_by(COMUNA) %>% 
    summarize_all(.funs = mean)
  rsl <- inner_join(st_as_sf(com), rsl, by = c('com' = 'COMUNA'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = paste0('m16_brr_regreso_MIOcmp_', dte), driver = 'ESRI Shapefile')
  print('Done!') 
  
}
map16()

# Mapa 17. Barreras ida peaton --------------------------------------------
map17 <- function(){
  dfm <- tbl
  
  # Univalle
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, a_5, f_1:f_10)
  lbl <- data.frame(nameCol = paste0('f_', c(1:7, 9:10)),
                    barrera = c('idnt_clles', 'sbr_bjr', 'dspl_and', 'dspl_obst', 'prb_pav', 'acera_rmps', 'crzr_clls', 'puentes', 'leer'))
  nms <- as.character(lbl$barrera)
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta) %>% 
    inner_join(., as.data.frame(brr)[,c('BARRIO', 'COMUNA')], by = c('barrio' = 'BARRIO')) %>% 
    dplyr::select(-barrio) %>% 
    gather(dificultad, tipo, -COMUNA) %>% 
    drop_na() %>% 
    inner_join(., ctg, by = 'tipo') %>% 
    group_by(COMUNA, dificultad) %>% 
    dplyr::summarize(promedio = mean(valor, na.rm = TRUE)) %>% 
    ungroup() %>% 
    spread(dificultad, promedio) %>% 
    NAer() %>% 
    retype()
  
  # LFM
  lfm <- st_read('D:/univalle/movilidad/data/shp/own/movilidad/lfm/17_barreras_ida_peaton.shp') %>% 
    dplyr::select(ID_COMUNA, Ave_B1:Ave_B9) %>% 
    setNames(c('COMUNA', 'idnt_clles', 'sbr_bjr', 'dspl_and', 'dspl_obst', 'prb_pav', 'acera_rmps', 'crzr_clls', 'puentes', 'leer', 'geometry')) %>% 
    as.data.frame() %>% 
    dplyr::select(-geometry) %>% 
    as_tibble() %>% 
    mutate(suma = idnt_clles + sbr_bjr + dspl_and + dspl_obst + prb_pav + acera_rmps + crzr_clls + puentes + leer) %>% 
    filter(suma > 0) %>% 
    dplyr::select(-suma) %>%
    group_by(COMUNA) %>% 
    summarise_all(.funs = mean) %>% 
    ungroup()
  
  sort(colnames(dfm))
  sort(colnames(lfm))
  
  # Join
  rsl <- bind_rows(dfm, lfm) %>% 
    arrange(COMUNA)
  rsl <- inner_join(st_as_sf(com), dfm, by = c('com' = 'COMUNA'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = paste0('m17_brr_ida_peaton_', dte), driver = 'ESRI Shapefile')
  print('Done!')
  
}
map17()

# Mapa 18. Barreras regreso peaton --------------------------------------------
map18 <- function(){
  sft <- dst
  dfm <- tbl
  
  # Univalle
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, f_1:f_10)
  int <- raster::intersect(as(sft, 'Spatial'), as(brr, 'Spatial')) 
  int <- st_as_sf(int) %>% 
    dplyr::select(ID_ENCUEST, COMUNA, BARRIO) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  lbl <- data.frame(nameCol = paste0('f_', c(1:7, 9:10)),
                    barrera = c('idnt_clles', 'sbr_bjr', 'dspl_and', 'dspl_obst', 'prb_pav', 'acera_rmps', 'crzr_clls', 'puentes', 'leer'))
  nms <- as.character(lbl$barrera)
  dfm <- inner_join(int, dfm, by = c('ID_ENCUEST' = 'id_encuesta'))
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'COMUNA', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta, -barrio) %>% 
    gather(dificultad, tipo, -COMUNA) %>% 
    drop_na() %>% 
    inner_join(., ctg, by = 'tipo') %>% 
    group_by(COMUNA, dificultad) %>% 
    dplyr::summarize(promedio = mean(valor, na.rm = TRUE)) %>% 
    ungroup() %>% 
    spread(dificultad, promedio) %>% 
    NAer() %>% 
    as_tibble() %>% 
    retype()
  
  # LFM
  lfm <- st_read('D:/univalle/movilidad/data/shp/own/movilidad/lfm/18_barreras_regreso_peaton.shp') %>% 
    dplyr::select(ID_COMUNA, Ave_B1:Ave_B9) %>% 
    setNames(c('COMUNA', 'idnt_clles', 'sbr_bjr', 'dspl_and', 'dspl_obst', 'prb_pav', 'acera_rmps', 'crzr_clls', 'puentes', 'leer', 'geometry')) %>% 
    as.data.frame() %>% 
    dplyr::select(-geometry) %>% 
    as_tibble() %>% 
    mutate(suma = idnt_clles + sbr_bjr + dspl_and + dspl_obst + prb_pav + acera_rmps + crzr_clls + puentes + leer) %>% 
    filter(suma > 0) %>% 
    dplyr::select(-suma) %>%
    group_by(COMUNA) %>% 
    summarise_all(.funs = mean) %>% 
    ungroup() 
  
  # Join
  rsl <- bind_rows(dfm, lfm) %>% 
    arrange(COMUNA) %>% 
    group_by(COMUNA) %>% 
    summarize_all(.funs = mean)
  rsl <- inner_join(st_as_sf(com), rsl, by = c('com' = 'COMUNA'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = paste0('m18_brr_regreso_peaton_', dte), driver = 'ESRI Shapefile')
  print('Done!') 
  
}
map18()

