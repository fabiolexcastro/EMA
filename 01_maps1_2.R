
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, readxl, qdap, hablar, Hmisc, Rfast)
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
prj <- '+proj=tmerc +lat_0=3.441883333 +lon_0=-76.5205625 +k=1 +x_0=1061900.18 +y_0=872364.63 +a=6379137 +b=6357748.961329674 +units=m +no_defs'   
geo <- '+proj=longlat +datum=WGS84 +no_defs'

# Functions to use --------------------------------------------------------
calcMinimunDist <- function(x, y){
  x <- spTransform(x, CRSobj = prj)
  y <- spTransform(y, CRSobj = prj)
  ds <- pointDistance(p1 = x, p2 = y) %>% 
    rowMins(., value = TRUE)
  x <- st_as_sf(x) %>% 
    mutate(near = ds) %>% 
    dplyr::select(ID_ENCUEST, Y, near) %>% 
    as(., 'Spatial') %>% 
    spTransform(., CRSobj = geo)
  print('Done!')
  return(x)
}
makeSummary <- function(x){
  # x <- m01
  x <- x %>% 
    as.data.frame %>% 
    as_tibble %>% 
    rename(comuna = d) %>% 
    mutate(ID_ENCUEST = as.numeric(ID_ENCUEST)) %>% 
    inner_join(., tbl, by = c('ID_ENCUEST' = 'id_encuesta')) %>% 
    group_by(comuna, d_1) %>% 
    dplyr::summarise(mts = mean(near)) %>% 
    ungroup() %>% 
    inner_join(., cnt, by = c('d_1' = 'd_1')) %>% 
    group_by(comuna) %>% 
    dplyr::summarise(wgt = weighted.mean(mts, count)) %>% 
    ungroup() %>% 
    mutate(time = (wgt/0.67)/60)
  print('Done!')
  return(x)
}

# Load data ---------------------------------------------------------------
# pth <- '../tbl/0910_total_wide.xlsx'
pth <- '../tbl/0826_total_wide.xlsx'
dte <- basename(pth) %>% 
  parse_number()
com <- shapefile('../shp/base/bcs_comunas_geo.shp')
lim <- shapefile('../shp/base/lim_cali.shp')
pth.lfm <- 'D:/univalle/movilidad/data/shp/own/movilidad/lfm'

# Geocoding shapes
ori <- shapefile('../shp/geocode/8-26_Origen.shp')
ori <- raster::intersect(ori, lim)

# Checking the comunas
or2 <- raster::intersect(ori, com)
or2 %>% 
  as.data.frame %>% 
  filter(d %in% c(10, 17, 19, 22))

prd <- shapefile('../shp/mio/paradas_geo.shp')
stt <- shapefile('../shp/mio/estaciones_geo.shp')
tbl <- read_excel(pth) %>% dplyr::select(id_encuesta, a_5, d_1) %>% mutate(ID_ENCUEST = as.numeric(id_encuesta))
dcp <- tbl %>% 
  pull(d_1) %>% 
  unique() %>% 
  .[-10] %>% 
  data.frame(disc = ., disc_ctg = c('Cognitiva', 'Multiple', 'Motora', 'Sensorial', 'Psicosocial', 'Sensorial', 'Sensorial', 'Otro', 'Sensorial'))
cnt <- tbl %>% 
  group_by(d_1) %>% 
  dplyr::summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(prc = count / sum(count) * 100) %>% 
  arrange(desc(prc))

# Mapa 1. Accesiblidad a estaciones ---------------------------------------
m01 <- calcMinimunDist(x = ori, y = stt)
m01 <- raster::intersect(m01, com)
m01 <- makeSummary(m01)
m01 <- m01 %>% 
  dplyr::select(comuna, time) %>% 
  setNames(c('COMUNA', 'time')) %>% 
  filter(!COMUNA %in% c(10,17,19,22))
lfm <- paste0(pth.lfm, '/01_tiempo_estaciones_geo.shp') %>% 
  st_read() %>% 
  dplyr::select(ID_COMUNA, INDICADOR) %>% 
  setNames(c('COMUNA', 'time', 'geometry')) %>% 
  as(., 'Spatial') %>% 
  as.data.frame()
m01 <- rbind(m01, lfm)
m01 <- inner_join(st_as_sf(com), m01, by = c('com' = 'COMUNA'))
m01 <- as(m01, 'Spatial')
writeOGR(obj = m01, dsn = '../shp/maps', layer = paste0('m01_accesStt_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)

# Mapa 2. Accesiblidad a paradas ------------------------------------------
m02 <- calcMinimunDist(x = ori, y = prd)
m02 <- raster::intersect(m02, com)
m02 <- makeSummary(m02)
m02 <- m02 %>% 
  dplyr::select(comuna, time) %>% 
  setNames(c('COMUNA', 'time')) %>% 
  filter(!COMUNA %in% c(10,17,19,22))
lfm <- paste0(pth.lfm, '/02_tiempo_paradas_geo.shp') %>% 
  st_read() %>% 
  dplyr::select(ID_COMUNA, INDICAD_PA) %>% 
  setNames(c('COMUNA', 'time', 'geometry')) %>% 
  as(., 'Spatial') %>% 
  as.data.frame()
m02 <- rbind(m02, lfm)
m02 <- inner_join(st_as_sf(com), m02, by = c('com' = 'COMUNA'))
m02 <- as(m02, 'Spatial')
writeOGR(obj = m02, dsn = '../shp/maps', layer = paste0('m02_accesPrd_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)


