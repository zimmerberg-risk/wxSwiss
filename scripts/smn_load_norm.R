##' ...............................................................................
##'  SwissMetNet: Load normals
##' ...............................................................................
##'  M. Sänger  07/2019 10/2020
##' ...............................................................................
##' 

## -------------------------------- Header -------------------------------------------
pkg.version = "2_0"
dir.R <- "~/R" # "~/R" "/R"

dir.pkg <- file.path(dir.R, "pkg", paste0("pkg_v_", pkg.version))
source(file.path(dir.pkg, "def.R"), local = T, echo = F)

## -------------------------------- Files/Packages -------------------------------------------
fct.list <- c("fct_sql")
pkg.list <- c("readr", "stringr", "rlang", "tidyr", "RPostgreSQL")

loadPkg(pkg.list, verbose = T)
loadFct(fct.list)

## -------------------------------- Def -------------------------------------------
dir.data <- "~/Documents/www_myweather/db/smn/ch.meteoschweiz.normwerttabellen"
loc <- locale(encoding = "ISO-8859-1")
# cn.str <- "do"
cn.str <- "do.postgresql"
period <- c("1961 – 1990", "1981 – 2010")
months <- exprs(Jan,   Feb,   Mar,   Apr,   Mai,   Jun,   Jul,   Aug,   Sep,   Okt,   Nov,   Dez)

cn <- sql_connect(cn.str, user = "postgres", dbname = "wx", schema = "smn")
dt.station <- tbl(cn, "station") %>%  collect()
dbDisconnect(cn)


dt.lut <- tribble(
  ~code_var_sma, ~code_var, ~agg,
  "prestam0", "p_qfe", "avg",
  "rre150m0", "pp", "sum",
  "rsd010m0", "pp_days", "sum",
  "sre000m0", "sun", "sum",
  "sremaxmv", "rel_sun", "avg",
  "tnd00nm0", "frost_day", "avg",
  "tnd00xm0", "ice_day", "avg",
  "tnd25xm0", "summer_day", "avg",
  "tnd30xm0", "heat_day", "avg",
  "tre2dymn", "tt", "min",
  "tre2dymx", "tt", "max",
  "tre200m0", "tt", "avg",
  "ure200m0", "rh", "avg",
  "gre000m0", "rad", "avg"
)
# nto000m0 cc
# pva200m0 e Dampfdruck hPa

dt.lut <- dt.lut %>% crossing(period)

read_smn_norm <- function(code_var_sma, code_var, agg, period, ...){
  # period = "1981 – 2010"; code_var_sma = "tre2dymx"; code_var  = "tt"; agg = "max"
  dir <- file.path(dir.data, period)
  file.name <- sprintf("nvrep_%s_de.txt", code_var_sma)
  path <- file.path(dir, file.name)
  if(file.exists(path)){
  read_delim(path, delim = "\t", skip = 8,  locale = loc) %>% 
      mutate(period = !!period, code_var = !!code_var, agg = !!agg)
  }
}

dt.in <- pmap(dt.lut[,],  ~ read_smn_norm(..1, ..2, ..3, ..4)) %>%
  bind_rows() %>%
  dplyr::select(period, code_var, agg, name = Station, Referenzperiode, Jan:Jahr)

dt.out <- dt.in %>%
   gather("month", "value", Jan:Jahr) %>%
    mutate(month = match( month, as.character(months)), name = enc2utf8(name))

# Match Names
dt.station.lut <- dt.out %>%
  distinct(name) %>%
  full_join(dt.station, by = "name") %>%
  dplyr::select(id_sma, name) %>%
  filter(is.na(id_sma)) %>%
  arrange(name)

# write_excel_csv(dt.station.lut, file.path(dir.data, "station_lut.csv"))
dt.station.lut.mapped <- read_csv(file.path(dir.data, "station_lut_mapped.csv"))

# Combine
dt.out.mapped <- dt.out %>%
  left_join(dt.station %>% filter(!duplicated(dt.station$name)), by = "name") %>%
  left_join(dt.station.lut.mapped, by = "name") %>%
  mutate(id_sma = case_when(!is.na(id_sma.x) ~ id_sma.x, TRUE ~ id_sma.y)) %>%
  dplyr::select(period,  id_sma, code_var, agg, month, name, value)

write_excel_csv(dt.out.mapped, file.path(dir.data, "normwerttabellen_parsed.csv"))

## -------------------------------- Daily -------------------------------------------
xxx <- dt.out.mapped %>% filter(code_var == "tt", agg == "max", id_sma == "SMA", period == "1961 – 1990", month > 0)

days = seq(1:365)
time.from <- as.Date("2018-01-01")
time.to <- as.Date("2020-12-31")


monthly2daily <- function(x = sin(1:12), agg = "sum"){
  if(length(x) != 12){
    warning("n != 12")
    return(tibble())
  }
  dt.daily <- tibble(time =  seq(time.from, time.to, "1 day")) %>%
    mutate(
      doy = lubridate::yday(time), 
      month = lubridate::month(time), 
      year = lubridate::year(time) - 2017, 
      days.seq = row_number(),
      mdays = lubridate::days_in_month(time)
    )
  dt.monthly <- tibble(time =  seq(time.from, time.to, "1 month")) %>% 
    mutate(
      month.mid.day = lubridate::floor_date(time, "1 month") + lubridate::days(floor(lubridate::days_in_month(time)/2) - 1),
      doy = lubridate::yday(month.mid.day), 
      year = lubridate::year(time) - 2017, 
      days.seq = (year - 1) * 365 + doy,
      value = rep(x, 3),
      value = case_when(agg == "sum" ~ value/lubridate::days_in_month(time), TRUE ~ value)
    )
  dt.daily$value <- spline(x = dt.monthly$days.seq, y = dt.monthly$value, xout = dt.daily$days.seq, method = "fmm")$y # , method = "hyman"
  
  dt.daily %>% 
    filter(year == 2) %>% 
    mutate(value =  round(value, 2)) %>%
    select(doy, month, value) %>%
    bind_rows({.} %>% filter(doy == 365) %>% mutate(doy = 366)) # Leap years
}

dt.daily <- dt.out.mapped %>% 
  #filter(id_sma == "SAE") %>%
  group_by(period, id_sma, code_var, agg) %>%
  filter(month > 0) %>%
  arrange(month) %>%
  group_modify(~monthly2daily(.x$value, .x$agg[1]), .keep = TRUE)

# Test
# t1 <- dt.daily %>% filter(code_var == "pp", agg == "sum", id_sma == "SAE", period == "1961 – 1990", month > 0)
# t2 <- dt.out.mapped %>% filter(code_var == "pp", agg == "sum", id_sma == "SAE", period == "1961 – 1990", month > 0)
# sum(t1$value)
# sum(t2$value)
# plot(t1$doy, t1$value, type = "l")
# points(seq(15, 365-15, length.out = 12), t2$value/30)


## -------------------------------- Upload -------------------------------------------
# Upload yearly
cn <-  sql_connect(cn.str, user = "postgres", dbname = "wx", schema = "smn")
dbWriteTable(cn, "norm_yearly", dt.out.mapped %>% filter(is.na(month)), overwrite = T, row.names = F)
dbDisconnect(cn)

# Upload monthly
cn <-  sql_connect(cn.str, user = "postgres", dbname = "wx", schema = "smn")
dbWriteTable(cn, "norm_monthly", dt.out.mapped %>% filter(!is.na(month)), overwrite = T, row.names = F)
dbDisconnect(cn)

# Upload daily
cn <-  sql_connect(cn.str, user = "postgres", dbname = "wx", schema = "smn")
dbWriteTable(cn, "norm_daily", dt.daily, overwrite = T, row.names = F)
dbDisconnect(cn)
