
## -------------------------------- SwissMetNet-------------------------------------------

#' Projections
#'
#' @docType data
#' @keywords dataset
#' @export
#'
lib.crs <- list(
  wgs84 = list(
    epsg = 4326,
    crs = "+proj=longlat +datum=WGS84"
  ),
  lv03 = list(
    epsg = 4149,
    crs = "+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs"
    ),
  lv95 = list(
    epsg = 4151,
    crs = "+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +units=m +no_defs"
  ),
  osm = list(
    epsg = 0,
    crs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"
  )
)

## -------------------------------- SwissMetNet-------------------------------------------

#' SwissMetNet Stations
#'
#' @docType data
#' @keywords dataset
#'
smn.stn <- data.table::fread("inst/smn/smn_stn.csv")

#' SwissMetNet Station Names
#'
#' @docType data
#' @keywords dataset
#'
smn.stn.name <- data.table::fread("inst/smn/smn_stn_name.csv")

#' SwissMetNet Station Sensors
#'
#' @docType data
#' @keywords dataset
#'
smn.stn.sensor <- data.table::fread("inst/smn/smn_stn_sensor.csv")

#' SwissMetNet Station Sensor History
#'
#' @docType data
#' @keywords dataset
#'
smn.stn.sensor.hist <- data.table::fread("inst/smn/smn_stn_sensor_hist.csv")

## -------------------------------- Shapefiles -------------------------------------------

#' Shapefile: Swiss country border (simplified)
#'
#' @docType data
#' @keywords dataset
#'
ch.ctry <- sf::read_sf("inst/shp/ch_country_5pct.shp")

#' Shapefile: Swiss Canton border (simplified)
#'
#' @docType data
#' @keywords dataset
#'
ch.canton <- sf::read_sf("inst/shp/ch_canton_5pct.shp")

#' Shapefile: Swiss district border (simplified)
#'
#' @docType data
#' @keywords dataset
#'
ch.bezirk <- sf::read_sf("inst/shp/ch_bezirk_5pct.shp")

#' Shapefile: Swiss lakes (simplified)
#'
#' @docType data
#' @keywords dataset
#'
ch.lake <- sf::read_sf("inst/shp/ch_lake_5pct.shp")

#' Shapefile: Swiss rivers (simplified)
#'
#' @docType data
#' @keywords dataset
#'
ch.river<- sf::read_sf("inst/shp/ch_river_5pct.shp")


