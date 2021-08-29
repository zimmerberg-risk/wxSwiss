Sys.setenv(TZ = 'UTC')
Sys.setlocale(locale = "en_GB.UTF-8")

library(data.table)
library(tidyverse)
library(jsonlite)
library(wx)

dir.data <- "~/Temp/smn/smn_stn"
dir.obs <- "~/Temp/smn"
dir.out <- "inst/smn"

## ----------------------------- Contents ------------------------------------

#' dt.stat: basic station info
#' dt.oscar: station history (by sensor)

## ----------------------------- Def ------------------------------------
server <- "https://data.geo.admin.ch"
server.oscar <- "https://oscar.wmo.int/surface/rest/api/stations/station?"
api.oscar <- "https://oscar.wmo.int/surface/rest/api/stations/observation/grouping"

vars <- list(
  list(mch = "lufttemperatur-10min", values = "value", code_var = "tt"),
  list(mch = "luftfeuchtigkeit-10min",  values = "value", code_var = "rh"),
  list(mch = "niederschlag-10min", values = "value", code_var = "pp"),
  list(mch = "niederschlag-1d", values = "value", code_var = "pp_1d"),
  list(mch = "gesamtschnee-1d", values = "value", code_var = "snow"),
  list(mch = "neuschnee-1d",  values = "value", code_var = "snow_new"),
  list(mch = "luftdruck-qfe-10min", values = "value", code_var = "p_qfe"),
  list(mch = "luftdruck-qnh-10min",  values = "value", code_var = "p_qnh"),
  list(mch = "luftdruck-qff-10min",  values = "value", code_var = "p_qff"),
  list(mch = "windgeschwindigkeit-kmh-10min", values = c("value", "wind_direction"), code_var = c("ff", "dir")),
  list(mch = "wind-boeenspitze-kmh-10min",  values = c("value"), code_var = "fx"),
  list(mch = "sonnenscheindauer-10min", values = "value", code_var = "sun"),
  list(mch = "globalstrahlung-10min", values = "value", code_var = "rad")
)

def.network <- list(
  smn = "messnetz-automatisch",
  climate = "messnetz-klima",
  atmos = "messnetz-atmosphaere",
  pp_manual = "messnetz-manuell",
  synop = "messnetz-beobachtungen",
  aero = "messnetz-flugwetter"
)

## ----------------------------- Networks ------------------------------------
list.stat <- mapply(names(def.network), def.network, SIMPLIFY = F, FUN = function(nw, url){
# url = "messnetz-automatisch"; nw = "smn"
  url.1 <- sprintf("%s/ch.meteoschweiz.%s_de.csv", dir.data, url)
  dt <- fread(url.1, encoding = "Latin-1")
  dt[,  network := nw]
  dt
})
dt.stat <- rbindlist(list.stat, fill = T)

smn.stn <- dt.stat %>%
  dplyr::select(
    stn = `Abk.`,
    id_wigos = `WIGOS-ID`,
    name = Station,
    from = `Daten seit`,
    z = `Stationshöhe m. ü. M.`,
    z_baro = `Barometerhöhe m. ü. Boden`,
    lon = KoordinatenE,
    lat = KoordinatenN,
    x = Längengrad,
    y = Breitengrad,
    canton = Kanton,
    network,
    airport = Flugplatz
  ) %>%
  group_by(stn, id_wigos, name,  z,   lon,     lat,         x,        y, canton) %>%
  summarise(z_baro = median(z_baro, na.rm = T), from = min(from, na.rm = T), network = paste(network, collapse = ","), airport = max(airport, na.rm = T)) %>%
  mutate(
    from = as.Date(strptime(from, "%d.%m.%Y")),
    id_wmo = str_extract(id_wigos, "[0-9]{5}$"),
    id_wigos = case_when(id_wigos == "" ~ NA_character_, TRUE ~id_wigos)
  ) %>%
  ungroup() %>%
  arrange(stn) %>%
  as.data.table()

which(duplicated(smn.stn$stn))



## ----------------------------- WIGOS ------------------------------------

dt.oscar.vars.lut  <- rbindlist(list(
  p = list(titleName = "Atmosphere > Pressure", observationTitle = "Atmospheric pressure - [Geometry: Point]"),
  pp = list(titleName = "Atmosphere > Precipitation",  observationTitle = "Amount of precipitation - [Geometry: Point]"),
  tt = list(titleName = "Atmosphere > Temperature", observationTitle = "Air temperature (at specified distance from reference surface) - [Geometry: Point]"),
  rh = list(titleName = "Atmosphere > Temperature", observationTitle = "Humidity  (at specified distance from reference surface) - [Geometry: Point]"),
  rad = list(titleName = "Atmosphere > Radiation",  observationTitle = "Solar > Global solar radiation (downwelling) - [Geometry: Point]"),
  dir = list(titleName = "Atmosphere > Wind", observationTitle = "Horizontal wind direction at specified distance from reference surface - [Geometry: Point]"),
  ff = list(titleName = "Atmosphere > Wind", observationTitle = "Horizontal wind speed at specified distance from reference surface - [Geometry: Point]"),
  fx = list(titleName = "Atmosphere > Wind", observationTitle = "Horizontal wind speed at specified distance from reference surface - [Geometry: Point]"),
  snow = list(titleName = "Terrestrial > Cryosphere_", observationTitle = "Snow > Snow depth - [Geometry: Point]"),
  snow_new = list(titleName = "Terrestrial > Cryosphere_", observationTitle = "Snow > Depth of snowfall - [Geometry: Point]")
), idcol = "code_var")

dt.wigos <- smn.stat %>% filter(!is.na(id_wigos)) #%>% dplyr::slice(1:5)

list.oscar <- mapply(dt.wigos$stn, dt.wigos$id_wigos, SIMPLIFY = F, FUN = function(id.sma, id.wigos){
  # id.sma = "ABO"; id.wigos = "0-20000-0-06735"
  cat(id.sma)
  def <- try(fromJSON(sprintf("%swmoIndex=%s", server.oscar, id.wigos)))
  if(inherits(def, "try-error")) return(NULL)

  oscar.vars <- fromJSON(sprintf("%s/%s", api.oscar, def$stationId), simplifyDataFrame = F)
  dt.oscar.vars <- simplify(oscar.vars) %>% rbindlist() %>% unnest_wider(observationTitle) %>% as.data.table

  dt.vars <- merge(dt.oscar.vars, dt.oscar.vars.lut, by = c("titleName", "observationTitle"))

  list.res <- mapply(dt.vars$observationId, dt.vars$code_var, SIMPLIFY = F, FUN = function(id.obs, code.var){
    # id.obs <- 171591; code.var = "tt"
    cat(" ", code.var)
    j1 <- fromJSON(sprintf("https://oscar.wmo.int/surface/rest/api/stations/deployments/%s", id.obs), flatten = T, simplifyDataFrame = F, simplifyVector = F)
    j <- map(j1, ~simplify(.x)) #jj <- map(j, ~as.data.table(enframe(unlist(.x))))

    data.table(
      stn = id.sma,
      id_wigos = id.wigos,
      para = code.var,
      from = map(j, ~ .x$dataGenerations[[1]]$observationSince) %>% replace_na(NA_character_) %>% unlist(),
      to = map(j, ~ .x$dataGenerations[[1]]$observationTill) %>% replace_na(NA_character_)  %>% unlist(),
      h = map(j, ~ as.numeric(.x$distanceFromRefSurface)) %>% replace_na(NA_real_) %>% unlist(),
      x = map(j, ~ .x$instrument$locations[[1]]$longitude) %>% replace_na(NA_real_) %>% unlist(),
      y = map(j, ~ .x$instrument$locations[[1]]$latitude) %>% replace_na(NA_real_) %>% unlist(),
      z = map(j, ~ .x$instrument$locations[[1]]$elevation) %>% replace_na(NA_real_) %>% unlist()
    )
  })
  dt <- rbindlist(list.res)
  cat("\n")
  dt
})
smn.stn.sensor.hist <- rbindlist(list.oscar)
setorder(smn.stn.sensor.hist, stn, para, from)

# Fix missing z
smn.stn.sensor.hist[, `:=`(
  z = fifelse(z == -99, NA_real_, z),
  h = fifelse(z == -99, NA_real_, h),
  from = as.Date(from),
  to = as.Date(to)
)]
# Omit soil temp
smn.stn.sensor.hist <- smn.stn.sensor.hist[!(para == "tt" & h == 0.05)]
smn.stn.sensor.hist[stn == "SMA" & para == "snow"]



## ----------------------------- Sensor ------------------------------------

load_smn_variable <- function(var, server){
  path <- sprintf("ch.meteoschweiz.messwerte-%s", var)
  file <- sprintf("ch.meteoschweiz.messwerte-%s_de.json", var)
  fromJSON(file.path(dir.obs, file), simplifyDataFrame = T, flatten = F)
}

list.meta <- lapply(seq_along(vars), function(i){
  # i = 1
  def <- vars[[i]]
  print(def$code_var)
  j <- load_smn_variable(def$mch, server)
  data.table(
    para = def$code_var,
    stn = j$features$id,
    #j$features$properties$station_name,
    z_str = j$features$properties$altitude,
    h_str = j$features$properties$measurement_height,
    lon = do.call(rbind, j$features$geometry$coordinates)[,1],
    lat = do.call(rbind, j$features$geometry$coordinates)[,2],
    descr = gsub("<.*?>", "", j$features$properties$description)
  )
})
dt.meta <- rbindlist(list.meta, fill = T)

smn.stn.sensor <- dt.meta %>%
  tidyr::separate(h_str, into = c("h", "h_obj_str"), sep = "\\(", remove = F) %>%
  mutate(
    z_para = as.numeric(str_extract(z_str, "[0-9\\.]+")),
    h = as.numeric(str_extract(h_str, "[0-9\\.]+")),
    obj = str_extract(h_obj_str, "[A-Z][a-z]+"),
    h_obj = as.numeric(str_extract(h_obj_str, "[0-9\\.]+"))
  ) %>%
  dplyr::select(stn, para, lon, lat, z_para, h, h_obj, obj) %>%
  as.data.table()

smn.stn.sensor[stn == "SMA"]

## --------------------------- Merge ------------------------------
smn.stn.name <- fread(file.path(dir.out, "smn_stn_name.csv"))
smn.stn <- merge(smn.stn, smn.stn.name, by = "stn", all = TRUE)
smn.stn <-  merge(smn.stn, smn.stn.sensor[, .(para_list = paste(para, collapse = ","), para_count = .N), .(stn)], by = "stn", all = TRUE)
smn.stn[, short_name := fifelse(is.na(short_name), name, short_name)]

smn.stn[is.na(para_list)]

## --------------------------- Register ------------------------------

write.table(smn.stn, file.path("inst", "smn", "smn_stn.csv"),
            sep = ",", na = "", row.names = F, append = F, fileEncoding = "UTF-8")
write.table(smn.stn.sensor.hist, file.path("inst", "smn", "smn_stn_sensor_hist.csv"),
            sep = ",", na = "", row.names = F, append = F, fileEncoding = "UTF-8")
write.table(smn.stn.sensor, file.path("inst", "smn", "smn_stn_sensor.csv"),
            sep = ",", na = "", row.names = F, append = F, fileEncoding = "UTF-8")

usethis::use_data(
  smn.stn,
  smn.stn.sensor,
  smn.stn.sensor.hist,
  overwrite = TRUE
)
