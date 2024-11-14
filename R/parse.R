
#' Parse VQHA80.csv from MeteoSwiss
#'
#' @author M. Saenger
#' @param p File path or URL
#' @param remote Load from remote (URL)
#' @return data table
#' @description
#'  Source: https://data.geo.admin.ch/ch.meteoschweiz.messwerte-aktuell/VQHA80.csv
#'  Description: https://data.geo.admin.ch/ch.meteoschweiz.messwerte-aktuell/info/VQHA80_de.txt
#' @examples
#' parse_mch_VQHA80(remote = TRUE)
#' @export
#'
parse_mch_VQHA80 <- function(p = NULL, remote = FALSE){
  cols <- c("stn", "time", "tt", "pp", "sun", "rad", "rh", "td", "dir", "ff", "fx", "p_qfe", "p_qff", "p_qnh", "z_850", "z_700")
  cols.tower <- c("dir_1", "ff_1", "fx_1", "tt_1", "rh_1", "td_1")
  cols.classes <- c(rep("character", 2), rep("numeric", length(cols) + length(cols.tower) - 2))

  if(remote){
    p <- "https://data.geo.admin.ch/ch.meteoschweiz.messwerte-aktuell/VQHA80.csv"
    if(!httr::HEAD(p)$status == 200){
      warning("URL not found")
      return(data.table())
    }
  } else {
    if(!file.exists(p)){
      warning("File not found")
      return(data.table())
    }
  }

  # file.info(p)
  # Read
  dat <- data.table::fread(p, na.strings = "-", col.names = c(cols, cols.tower),
                           integer64 = "character", stringsAsFactors = FALSE, colClasses = cols.classes)
  dat <- dat[, mget(c(cols, cols.tower))]

  # Merge main/tower parameters
  dat[, `:=`(
    dir = fifelse(!is.na(dir_1), dir_1, dir),
    ff = fifelse(!is.na(ff_1), ff_1, ff),
    fx = fifelse(!is.na(fx_1), fx_1, fx),
    tt = fifelse(!is.na(tt_1), tt_1, tt),
    rh = fifelse(!is.na(rh_1), rh_1, rh),
    td = fifelse(!is.na(td_1), td_1, td)
  )]
  dat <- dat[, mget(cols)]
  dat[, `:=`(time = as.POSIXct(time, tryFormats = "%Y%m%d%H%M%OS", tz = "UTC"))]
  dat.melt <- melt(dat, id.vars = c("stn", "time"), variable.name = "para", variable.factor = FALSE)
  dat.melt[, value := fifelse(value %in% c(9999, 99999), NA_real_, value)]
  dat.melt
}

#' Parse VQHA98.csv from MeteoSwiss (Precipitation)
#'
#' @author M. Saenger
#' @param p File path or URL
#' @param remote Load from remote (URL)
#' @return data table
#' @description
#'  Source: https://data.geo.admin.ch/ch.meteoschweiz.messwerte-aktuell/VQHA98.csv
#'  Description: https://data.geo.admin.ch/ch.meteoschweiz.messwerte-aktuell/info/VQHA98_de.txt
#' @examples
#' parse_mch_VQHA98(remote = TRUE)
#' @export
#'
parse_mch_VQHA98 <- function(p = NULL, remote = FALSE){
  cols.classes <- c(rep("character", 2), "numeric")

  if(remote){
    p <- "https://data.geo.admin.ch/ch.meteoschweiz.messwerte-aktuell/VQHA98.csv"
    if(!httr::HEAD(p)$status == 200){
      warning("URL not found")
      return(data.table())
    }
  } else {
    if(!file.exists(p)){
      warning("File not found")
      return(data.table())
    }
  }

  # Read
  dat <- fread(p, na.strings = "-", col.names = c("stn", "time", "value"),
               integer64 = "character", stringsAsFactors = FALSE, colClasses = cols.classes)
  dat[, `:=`(time = as.POSIXct(time, tryFormats = "%Y%m%d%H%M%OS", tz = "UTC"), para = "pp")]
  dat[, value := fifelse(value %in% c(9999, 99999), NA_real_, value)]
  dat
}

#' Parse JSON from MeteoSwiss
#'
#' @author M. Saenger
#' @param p Directory path or URL
#' @param para Parameter
#' @param remote Load from remote (URL)
#' @return data table
#' @description
#'  Source:
#'  Description:
#' @examples
#' parse_mch_json(para = "ff", remote = TRUE)
#' @export
#'
parse_mch_json <- function(p = NULL, para = "tt", remote = FALSE){

  list.para <- list(
    tt = list(mch = "lufttemperatur-10min", values = "value", code_var = "tt"),
    rh = list(mch = "luftfeuchtigkeit-10min",  values = "value", code_var = "rh"),
    pp = list(mch = "niederschlag-10min", values = "value", code_var = "pp"),
    pp_1d = list(mch = "niederschlag-1d", values = "value", code_var = "pp_1d"),
    snow = list(mch = "gesamtschnee-1d", values = "value", code_var = "snow"),
    snow_new = list(mch = "neuschnee-1d",  values = "value", code_var = "snow_new"),
    p_qfe = list(mch = "luftdruck-qfe-10min", values = "value", code_var = "p_qfe"),
    p_qnh = list(mch = "luftdruck-qnh-10min",  values = "value", code_var = "p_qnh"),
    p_qff = list(mch = "luftdruck-qff-10min",  values = "value", code_var = "p_qff"),
    ff = list(mch = "windgeschwindigkeit-kmh-10min", values = c("value"), code_var = c("ff", "dir")),
    dir = list(mch = "windgeschwindigkeit-kmh-10min", values = c("wind_direction"), code_var = c("ff", "dir")),
    fx = list(mch = "wind-boeenspitze-kmh-10min",  values = c("value"), code_var = "fx"),
    foehn = list(mch = "foehn-10min",  values = "value", code_var = "foehn"),
    sun = list(mch = "sonnenscheindauer-10min", values = "value", code_var = "sun"),
    rad = list(mch = "globalstrahlung-10min", values = "value", code_var = "rad")
  )
  para <- match.arg(para, names(list.para))

  name.var <- list.para[[para]]$mch
  value.var <- list.para[[para]]$values
  aux.var <- c("reference_ts",  "altitude", "unit", value.var) #"measurement_height",
  aux.var.names <- c("time", "z", "unit", "value") #h

  if(remote){
    p <- sprintf("https://data.geo.admin.ch/ch.meteoschweiz.messwerte-%s/ch.meteoschweiz.messwerte-%s_de.json", name.var, name.var)
    if(!httr::HEAD(p)$status == 200){
      warning("URL not found")
      return(data.table())
    }
  } else {
    p <- file.path(p, sprintf("ch.meteoschweiz.messwerte-%s_de.json", name.var))
    if(!file.exists(p)){
      warning("File not found")
      return(data.table())
    }
  }

  json <- jsonlite::fromJSON(p, simplifyDataFrame = TRUE, flatten = TRUE)

  dat <- as.data.table(json$features[, c("id", paste0("properties.", aux.var))])
  dat <- cbind(dat, do.call(rbind, json$features$geometry.coordinates))
  setnames(dat, c("stn", aux.var.names, "lon", "lat"))

  dat[, `:=`(para = para, time = suppressWarnings(as.POSIXct(strptime(time, "%Y-%m-%dT%H:%M:%SZ"))))]

  # Heal missing time from missing records
  dat[, time := fifelse(is.na(time), max(time, na.rm = T), time)]
  dat[, value := fifelse(value %in% c(9999, 99999), NA_real_, value)]

  dat[]
}

#' Parse parse_nbcn_daily from MeteoSwiss
#'
#' @author M. Saenger
#' @param p File path or URL
#' @param remote Load from remote (URL)
#' @param stn Station identifier (e. g. "SMA" or "BAS")
#' @param type "current" for current year or "previous" for historical
#' @return data table
#' @examples
#' dat <- parse_nbcn_daily(p = dir.data, remote = TRUE, stn = "SMA", type = "current")
#' @export
#'
parse_nbcn_daily <- function(p = NULL, remote = FALSE, stn = "SMA", type = c("current", "previous")){

  type <- match.arg(type)
  f <- sprintf("nbcn-daily_%s_%s.csv", stn, type)

  if(remote){
    p <- sprintf("https://data.geo.admin.ch/ch.meteoschweiz.klima/nbcn-tageswerte/%s", f)
    if(!httr::HEAD(p)$status == 200){
      warning("URL not found")
      return(data.table())
    }
  } else {
    p <- file.path(p, f)
    if(!file.exists(p)){
      warning("File not found")
      return(data.table())
    }
  }

  cols <- c("prestad0", "rre150d0", "sre000d0", "tre200dn", "tre200dx", "tre200d0", "ure200d0", "gre000d0", "hto000d0", "nto000d0")
  para <- c("p_qfe", "pp",    "sun",   "tt",    "tt",    "tt",    "rh",    "rad",   "snow",  "cc")
  agg <- c("avg", "sum", "sum", "min", "max", "avg", "avg", "avg", "term", "sum")

  dat <- fread(p, na.strings = "-", integer64 = "numeric", stringsAsFactors = FALSE, colClasses = c(rep("character", 2), rep("numeric", 10)))
  dat <- melt(dat, id.vars = 1:2, variable.factor = FALSE)
  dat[, `:=`(agg = agg[match(variable, cols)], para = para[match(variable, cols)], date = as.POSIXct(date, tz = "UTC", format="%Y%m%d"))]
  set(dat, j = "variable", value = NULL)
  setnames(dat, c("stn", "time", "value", "agg", "para"))
  setcolorder(dat, c("stn",  "para", "time", "agg", "value"))
  dat[]
}

#' Parse parse_nbcn_monthly from MeteoSwiss
#'
#' @author M. Saenger
#' @param p File path or URL
#' @param remote Load from remote (URL)
#' @param stn Station identifier (e. g. "SMA" or "BAS")
#' @param type "current" for current year or "previous" for historical
#' @return data table
#' @examples
#' dat <- parse_nbcn_daily(p = dir.data, remote = TRUE, stn = "SMA", type = "current")
#' @export
#'
parse_nbcn_monthly <- function(p = NULL, remote = FALSE, stn = "SMA", type = c("current", "previous")){

  type <- match.arg(type)
  f <- sprintf("nbcn-monthly_%s_%s.csv", stn, type)

  if(remote){
    p <- sprintf("https://data.geo.admin.ch/ch.meteoschweiz.klima/nbcn-monatswerte/%s", f)
    if(!httr::HEAD(p)$status == 200){
      warning("URL not found")
      return(data.table())
    }
  } else {
    p <- file.path(p, f)
    if(!file.exists(p)){
      warning("File not found")
      return(data.table())
    }
  }

  cols <- c("gre000m0", "hto000m0", "nto000m0", "prestam0", "rre150m0", "sre000m0", "tre200m0", "tre200mn", "tre200mx", "ure200m0")
  para <- c("rad", "snow", "cloud",  "p_qfe", "pp", "sun", "tt", "tt", "tt", "rh")
  agg <- c("avg", "avg", "avg", "avg", "sum", "sum", "avg", "min", "max", "avg")

  dat <- fread(p, na.strings = "-", integer64 = "numeric", stringsAsFactors = FALSE, colClasses = c(rep("character", 2), rep("numeric", 10)))
  dat <- melt(dat, id.vars = 1:2, variable.factor = FALSE)
  dat[, `:=`(agg = agg[match(variable, cols)], para = para[match(variable, cols)], date = as.POSIXct(date, tz = "UTC", format="%Y%m%d"))]
  set(dat, j = "variable", value = NULL)
  setnames(dat, c("stn", "time", "value", "agg", "para"))
  setcolorder(dat, c("stn",  "para", "time", "agg", "value"))
  dat[]
}


#' Parse normal data from MeteoSwiss
#'
#' @author M. Saenger
#' @param p File path or URL
#' @param remote Load from remote (URL)
#' @param period Period (e. g. "1981-2010")
#' @return data table
#' @export
#'
parse_mch_norm <- function(p = NULL, remote=FALSE, period="1981-2010"){
  # period <- "1981-2010"
  period <- match.arg(period, c("1961-1990", "1981-2010", "1991-2020"))

  dt.lut <- tibble::tribble(
    ~para_str, ~para, ~agg,
    "fkl010m0", "ff", "avg",
    "pva200m0", "e", "avg",
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
  dt.lut <- as.data.table(dt.lut)
  months <- c("Jan", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")

  # Download
  if(remote){
    p <- "https://data.geo.admin.ch/ch.meteoschweiz.klima/normwerte/normwerte.zip"
    temp <- tempfile(fileext = ".zip")
    utils::download.file(p, temp)
    cn <- temp
    if(!httr::HEAD(p)$status == 200){
      warning("URL not found")
      return(data.table())
    }
  } else {
    cn <- file.path(p, f)
    if(!file.exists(p)){
      warning("File not found")
      return(data.table())
    }
  }

  # Unzip
  temp.dir <- tempdir()
  f <- unzip(cn, list = T)[[1]]
  unzip(cn, exdir=temp.dir)
  f <- f[grepl(sprintf(".+%s_de.txt", period), f)]
  #list.files(temp.dir)

  # Process
  dat.list <- lapply(f, function(i){
    # i <- f[1]
    para.str <-  stringr::str_extract(i, paste(dt.lut$para_str, collapse="|"))
    def <- dt.lut[para_str == para.str,]
    if(nrow(def) == 0) return(data.table())
    dt <- data.table::fread(file.path(temp.dir, i), encoding = "Latin-1")
    dt <- dt[, .(Station, Jan, Feb, Mar, Apr, Mai, Jun, Jul, Aug, Sep, Okt, Nov, Dez, Jahr)]
    dt <- cbind(dt, def)
    dt
  })
  dat <- rbindlist(dat.list)
  dat <- melt(dat, id.vars = c("Station", "para", "agg", "para_str"), variable.name = "month")

  dat[, `:=`(
    month = match(month, months),
    period = period,
    tspan = fifelse(is.na(month), "year", "month")
  )]
  dat <- dat[, .(period, name=Station, para, agg, tspan, month, value)]

  # Match station names
  dat <- smn.stn[, .(stn, name)][dat, on=c("name")]

  # Missing
  # dat[is.na(stn)]
  dat[!is.na(stn)][]

}

# xxx <- parse_mch_norm(remote = TRUE, period = "1991-2020")
# with(xxx[para=="tt" & agg=="max" & tspan=="month"], plot(month, value, type="p", lwd=2))

#' Parse COSMO-2E Point Forecasts from MeteoSwiss
#'
#' @author M. Saenger
#' @param p File path or URL
#' @param remote Load from remote (URL)
#' @param stn.list Character vector of station identifier (e. g. "SMA" or c("BAS", "ABO"))
#' @return data table
#' @examples
#'
#' require(data.table)
#' require(ggplot2)
#'
#' # Download
#' dt <- parse_cosmo_2e(remote = TRUE, stn.list = c("SMA"))
#'
#' # Quantiles
#' q <- c(.1, .9)
#' dt.q <- dt[, as.list(quantile(value, q, na.rm = TRUE)), .(stn, para, time)]
#'
#' # Station information (including grid altitude)
#' attr(dt, "meta")
#'
#' # Plot temperature forcast (10-90% as ribbon, members as lines)
#' ggplot(dt[para == "tt"], aes(time, value)) +
#'   geom_ribbon(aes(y = NULL, ymin = `10%`, ymax = `90%`),
#'   data = dt.q[para == "tt"], fill = "lightblue") +
#'   geom_path(aes( group = member), size = .1)
#' @export
#'
parse_cosmo_2e <- function(p=NULL, remote=FALSE, stn.list=NULL){

  f <- "COSMO-E-all-stations.csv"

  if(remote){
    p <- sprintf("https://data.geo.admin.ch/ch.meteoschweiz.prognosen/punktprognosen/%s", f)
    temp <- tempfile()
    utils::download.file(p, temp)
    cn <- temp
    if(!httr::HEAD(p)$status == 200){
      warning("URL not found")
      return(data.table())
    }
  } else {
    cn <- file.path(p, f)
    if(!file.exists(p)){
      warning("File not found")
      return(data.table())
    }
  }

  dt.para <- data.table(
    para_src = c("T_2M", "FF_10M", "DD_10M", "TOT_PREC", "RELHUM_2M", "DURSUN"),
    para = c("tt", "ff_kmh", "dir", "pp", "rh", "sun")
  )

  ##  Download
  txt <- readLines(con = cn, encoding = "UTF-8", skipNul = FALSE) #suppressWarnings() Latin-1

  ## Meta data
  time.init <- as.POSIXct(gsub(".*([0-9]{4}-[0-9]{2}-[0-9]{2};[0-9]{1,2}:[0-9]{2}).*", "\\1", x = txt[9]), format = "%Y-%m-%d;%H:%M")
  dt.meta <- do.call(data.table, lapply(txt[18:22], function(i) strsplit(i, ";")[[1]][-1]))
  setnames(dt.meta, c("stn", "x", "y", "grid_id", "grid_z"))

  dt.var <- do.call(data.table, lapply(txt[24:25], function(i) strsplit(i, ";")[[1]][-1]))
  para_src <- strsplit(txt[24], ";")[[1]][-(1:3)]
  #unit <- strsplit(txt[26], ";")[[1]][-(1:3)]
  member <- as.integer(strsplit(txt[26], ";")[[1]][-(1:3)])

  if(!is.null(stn.list)){
    ind <- which(grepl(sprintf("^(%s).+$", paste(stn.list, collapse="|")), txt, perl = TRUE))
    txt <- txt[c(1:27, ind)]
    dt.meta <- dt.meta[stn %in% stn.list]
  }
  # Read data
  list.dat <- lapply(28:length(txt), function(i){
    l <- txt[i]
    value <- strsplit(l, ";")[[1]]
    if(length(value) < 129){
      warning(cat("Skipped: ", value[1], value[2], i, "\n"))
      return(data.table())
    }
    data.table(stn = value[1], para_src = para_src, member, time = value[2], value = as.numeric(value[-(1:3)]))
  })
  dat <- data.table::rbindlist(list.dat)

  # Process data
  dat[, `:=`(
    time = as.POSIXct(time, tz = "UTC", format = "%Y%m%d %H:%M"),
    time_init = time.init,
    value = fifelse(value == -999, NA_real_, value)
  )]
  dat <- merge(dat, dt.para, by = "para_src")
  set(dat, j = "para_src", value = NULL)
  setcolorder(dat, c("stn",  "para", "member", "time_init",  "time", "value"))
  structure(dat[], meta = dt.meta)
}

# dt <- parse_cosmo_2e(remote = TRUE, stn.list = c("SMA"))
# # Quantiles
# q <- c(.1, .9)
# dt.q <- dt[, as.list(quantile(value, q, na.rm = TRUE)), .(stn, para, time)]
#
# # Station information (including grid altitude)
# attr(dt, "meta")
#
# # Plot temperature forcast (10-90% as ribbon, members as lines)
# ggplot(dt[para == "tt"], aes(time, value)) +
#   geom_ribbon(aes(y = NULL, ymin = `10%`, ymax = `90%`),
#   data = dt.q[para == "tt"], fill = "lightblue") +
#   geom_path(aes( group = member), size = .1)


#' Parse SLF snow height data
#'
#' @author M. Saenger
#' @param p File path or URL
#' @param remote Load from remote (URL)
#' @param id.var Identifier variable (e. g. "tt" for air temperature)
#' @return data table
#' @description
#'  Source: https://public-meas-data-v2.slf.ch/public/station-data/timepoint/SNOW_HEIGHT/current/geojson
#'  Description: -
#' @examples
#' parse_slf_snow(remote=TRUE, id.var="tt")
#' @export
#'
parse_slf_snow <- function(p = NULL, remote = FALSE, id.var="snow"){
  # id.var = "tt"

  id.var.src.list <- c("SNOW_HEIGHT", "HEIGHT_NEW_SNOW_1D", "WIND_MEAN", "TEMPERATURE_AIR")
  id.var.tg.list <- c("snow", "snow_new", "ff", "tt")
  id.var.src <- id.var.src.list[match(id.var, id.var.tg.list)]
  var.cols <- c("value", "velocity", "direction")
  server <- "https://public-meas-data-v2.slf.ch/public/station-data/timepoint"

  p <- sprintf("%s/%s/current/geojson", server, id.var.src)
  f <- sprintf("slf-%s.geojson", id.var)

  if(remote){
    temp <- tempfile(fileext = "geojson")
    utils::download.file(p, temp, quiet=T)
    cn <- temp
    if(file.size(cn) < 10000){
      warning("URL not found")
      return(data.table())
    }
  } else {
    cn <- file.path(p, f)
    if(!file.exists(p)){
      warning("File not found")
      return(data.table())
    }
  }
  # Read geojson
  dat <- sf::st_read(cn, quiet = T) |>
    dplyr::mutate(lon = sf::st_coordinates(geometry)[,1], lat = sf::st_coordinates(geometry)[,2]) |>
    sf::st_drop_geometry() |>
    as.data.table()

  unlink(cn)

  dat <- dat |>
    melt(measure.vars = intersect(var.cols, names(dat)), variable.name = "para", value.name = "value", variable.factor=F)

  dat[, `:=`(
    para = fifelse(para == "velocity", "ff", fifelse(para == "direction", "dir", id.var)),
    stn = fifelse(network=="SMN", stringr::str_extract(code, "[A-Z]{3}"), sprintf("CH-SLF-%s", code)),
    provider = fifelse(network=="SMN", "MCH", "SLF")
  )]
  dat <- dat[!is.na(value), .(stn=stn, provider, lon, lat, hh=elevation, name=label, manual, time=timestamp, para, value=round(value, 1))]
  dat[]
}
# xxx <- parse_slf_snow(remote=T, id.var="tt")
# library(tinyplot)
# plt(lat ~ lon | value, data=xxx, fill = "by", pch=21)
# with(xxx, text(lon, lat, labels = name, col=1, cex=.6, pos=1))
