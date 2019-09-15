
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, readxl, qdap, hablar, Hmisc, Rfast)
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
pth <- '../tbl/0826_total_wide.xlsx'
tbl <- read_excel(pth)
com <- shapefile('../shp/base/bcs_comunas_geo.shp')
brr <- st_read('../shp/base/bcs_barrios_geo.shp') %>% 
  mutate(BARRIO = as.character(BARRIO))
dte <- basename(pth) %>% str_sub(., start = 1, end = 4)
dst <- st_read('../shp/geocode/8-26_Destino.shp')
pth.lfm <- 'D:/univalle/movilidad/data/shp/own/movilidad/lfm'

# Labels ------------------------------------------------------------------
trn <- data.frame(label = c('camin_rod', 'vpart', 'mpart', 'bici',
                            'taxi', 'mototaxi', 'bicitaxi', 'jeep', 'vinform',
                            'buspubl', 'tinter', 'vescol',
                            'buspriv', 'MIOp', 'MIOart', 'MIOali', 'MIOc', 'Amb'),
                  transp = c('Caminando o rodando', 'Vehículo particular', 'Moto particular', 'Bicicleta',
                             'Taxi', 'Moto taxi', 'Bici taxi', 'Jeep o Guala', 'Vehículo informal',
                             'Bus o buseta pública', 'Transporte intermunicipal', 'Vehículo escolar',
                             'Bus privado de empresa', 'MIO padrón/pretroncal', 'MIO articulado/troncal',
                             'MIO alimentador', 'Mio cable', 'Ambulancia'))
trn <- mutate(trn, transp = as.character(transp),
              label = as.character(label))

# Mapa 3. Origen total de los viajes de las PcD ---------------------------
map03 <- function(dfm){
  dfm <- tbl
  
  # Univalle
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, a_5) %>% 
    inner_join(., as.data.frame(brr), by = c('a_5' = 'BARRIO')) %>% 
    group_by(COMUNA) %>% 
    dplyr::summarise(viajes = n()) %>% 
    ungroup() %>% 
    mutate(COMUNA = as.numeric(COMUNA))
  
  # LFM
  lfm <- paste0(pth.lfm, '/03_origen_viajes.shp') %>% 
    st_read %>% 
    as.data.frame() %>% 
    dplyr::select(ID_COMUNA, TOT_VIAJES) %>% 
    group_by(ID_COMUNA) %>% 
    dplyr::summarise(viajes = sum(TOT_VIAJES, na.rm = TRUE)) %>% 
    ungroup() %>% 
    rename(COMUNA = ID_COMUNA)
  
  # Join
  fnl <- rbind(dfm, lfm) %>% 
    arrange(COMUNA)
  fnl <- inner_join(st_as_sf(com), fnl, by = c('com' = 'COMUNA'))
  fnl <- as(fnl, 'Spatial')
  writeOGR(obj = fnl, dsn = '../shp/maps', layer = paste0('m03_origenViajes_', dte), driver = 'ESRI Shapefile')
  print('Done!')
  
}
map03(dfm = tbl)

# Mapa 4. Destino total de los viajes de las PcD --------------------------
map04 <- function(shp){
  shp <- dst
  
  # Univalle
  dfm <- raster::intersect(as(shp, 'Spatial'), com) %>% 
    as.data.frame %>% 
    as_tibble %>% 
    rename(comuna = d) %>% 
    group_by(comuna) %>% 
    dplyr::summarise(viajes = n()) %>% 
    ungroup()
  
  # LFM
  lfm <- paste0(pth.lfm, '/04_destino_viajes.shp') %>% 
    st_read %>% 
    as.data.frame() %>% 
    dplyr::select(ID_COMUNA, TOTAL_VIAJ) %>% 
    group_by(ID_COMUNA) %>% 
    dplyr::summarise(viajes = sum(TOTAL_VIAJ, na.rm = TRUE)) %>% 
    ungroup() %>% 
    rename(comuna = ID_COMUNA)
  
  # Join
  fnl <- rbind(dfm, lfm) %>% 
    arrange(comuna) %>% 
    group_by(comuna) %>% 
    dplyr::summarise(viajes = sum(viajes, na.rm = TRUE)) %>% 
    ungroup() %>% 
    rename(COMUNA = comuna)
  fnl <- inner_join(st_as_sf(com), fnl, by = c('com' = 'COMUNA'))
  fnl <- as(fnl, 'Spatial')
  writeOGR(obj = fnl, dsn = '../shp/maps', layer = paste0('m04_destinoViajes_', dte), driver = 'ESRI Shapefile')
  print('Done!')
  
}
map04(shp = dst)

# Mapa 5. Origen de viajes por modo de transporte de las PcD --------------
map05 <- function(dfm){
  dfm <- tbl
  
  # Univalle
  dfm <- dfm %>% 
    dplyr::select(a_5, e_7_1:e_7_10) %>% 
    inner_join(., as.data.frame(brr) %>% dplyr::select(BARRIO, COMUNA), by = c('a_5' = 'BARRIO')) %>% 
    dplyr::select(COMUNA, everything()) %>% 
    dplyr::select(-a_5) %>% 
    gather(nameCol, transporte, -COMUNA) %>% 
    drop_na() %>% 
    group_by(COMUNA, transporte) %>% 
    dplyr::summarise(count = n()) %>% 
    ungroup() %>% 
    inner_join(., trn, by = c('transporte' = 'transp')) %>% 
    dplyr::select(COMUNA, label, count) %>% 
    spread(label, count) %>% 
    mutate(COMUNA = as.character(COMUNA))
    
  # LFM
  lfm <- paste0(pth.lfm, '/mapa5_6_origen_destino.shp') %>% 
    st_read %>% 
    as.data.frame() %>% 
    dplyr::select(BARRIO, starts_with('ori', ignore.case = TRUE)) %>% 
    as_tibble() %>% 
    setNames(c('BARRIO', 'MIOp', 'buspubl', 'jeep', 'taxi', 'vpart', 'mpart', 'bici', 'camin_rod', 'otro')) %>% 
    inner_join(., as.data.frame(brr), by = c('BARRIO' = 'BARRIO')) %>% 
    dplyr::select(COMUNA, MIOp:otro) %>% 
    setNames(c('COMUNA', 'MIOp', 'buspubl', 'jeep', 'taxi', 'vpart', 'mpart', 'bici', 'camin_rod', 'otro')) %>% 
    group_by(COMUNA) %>% 
    summarize_all(.funs = sum) %>% 
    ungroup()
  
  # Join
  rsl <- bind_rows(dfm, lfm) %>% 
    NAer() %>% 
    retype()
  rsl <- inner_join(st_as_sf(com), rsl, by = c('com' = 'COMUNA'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../shp/maps', layer = paste0('m05_origenMdT_', dte), driver = 'ESRI Shapefile')
  print('Done!')
  return(rsl)
}

# Mapa 6. Destino de viajes por modo de transporte ------------------------
map06 <- function(dfm, sft){
  dfm <- tbl
  sft <- dst
  
  # Univalle
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, e_14_1:e_14_10) 
  int <- raster::intersect(as(sft, 'Spatial'), as(brr, 'Spatial'))
  int <- st_as_sf(int) %>% 
    dplyr::select(ID_ENCUEST, COMUNA, BARRIO) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  dfm <- inner_join(int, dfm, by = c('ID_ENCUEST' = 'id_encuesta'))
  dfm <- dfm %>% 
    dplyr::select(-ID_ENCUEST) %>% 
    gather(nameCol, transporte, -BARRIO, -COMUNA) %>% 
    drop_na() %>% 
    group_by(COMUNA, transporte) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    inner_join(., trn, by = c('transporte' = 'transp')) %>% 
    dplyr::select(COMUNA, label, count) %>% 
    spread(label, count) %>% 
    NAer() %>% 
    as_tibble() %>% 
    retype()
  
  # LFM
  lfm <- paste0(pth.lfm, '/mapa5_6_origen_destino.shp') %>% 
    st_read %>% 
    as.data.frame() %>% 
    dplyr::select(BARRIO, starts_with('dst', ignore.case = TRUE)) %>% 
    as_tibble() %>% 
    setNames(c('BARRIO', 'MIOp', 'buspubl', 'jeep', 'taxi', 'vpart', 'mpart', 'bici', 'camin_rod', 'otro')) %>% 
    inner_join(., as.data.frame(brr), by = c('BARRIO' = 'BARRIO')) %>% 
    dplyr::select(COMUNA, MIOp:otro) %>% 
    setNames(c('COMUNA', 'MIOp', 'buspubl', 'jeep', 'taxi', 'vpart', 'mpart', 'bici', 'camin_rod', 'otro')) %>% 
    group_by(COMUNA) %>% 
    summarize_all(.funs = sum) %>% 
    ungroup() %>% 
    mutate(COMUNA = as.numeric(COMUNA))
  
  # Join
  rsl <- bind_rows(dfm, lfm) %>% 
    NAer() %>% 
    retype()
  rsl <- inner_join(st_as_sf(com), rsl, by = c('com' = 'COMUNA'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = fnl, dsn = '../shp/maps', layer = paste0('m06_destinoMdT_', dte), driver = 'ESRI Shapefile')
  print('Done!')
  return(rsl)
}



