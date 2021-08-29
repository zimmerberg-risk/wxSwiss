# data-raw/mydataset.R
# Data import and processing pipeline

library(sf)
library(dplyr)
library(rmapshaper)
library(ggplot2)

dir.geo = "~/Data/Geodata"

## --------------------------- Data ------------------------------
# https://data.geo.admin.ch/
file.ch.canton <- "/CHE/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_1_TLM_KANTONSGEBIET.shp"
file.ch.country <- "/CHE/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_1_TLM_LANDESGEBIET.shp"
file.ch.bezirk <- "/CHE/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_1_TLM_BEZIRKSGEBIET.shp"
file.ch.poly <- "/CHE/swissNAMES3D_LV95/shp_LV95_LN02/swissNAMES3D_PLY.shp"

file.ch.lake <- "/CHE/20161001_SMV500_SHAPE_CHL95/Shapefiles/22_DKM500_GEWAESSER_PLY.shp"
file.ch.river <- "/CHE/20161001_SMV500_SHAPE_CHL95/Shapefiles/25_DKM500_GEWAESSER_LIN.shp"

shp.ch.country <- read_sf(file.path(dir.geo, file.ch.country))
shp.ch.canton <- read_sf(file.path(dir.geo, file.ch.canton))
shp.ch.bezirk<- read_sf(file.path(dir.geo, file.ch.bezirk))
shp.ch.poly <- read_sf(file.path(dir.geo, file.ch.poly))
#shp.ch.lake <- shp.ch.poly %>% dplyr::filter(OBJEKTART %in% c("See")) %>% st_zm()

shp.ch.lake <- read_sf(file.path(dir.geo, file.ch.lake))
shp.ch.river <- read_sf(file.path(dir.geo, file.ch.river))

shp_simplify <- function(x, name, crs = lib.crs$wgs84$crs, keep = .1, thresh = 0, verbose = TRUE, ...){
  y <- x %>%
    st_transform(crs = lib.crs$wgs84$crs) %>%
    rmapshaper::ms_simplify(., keep = keep, keep_shapes = FALSE) %>%
    mutate(area = as.numeric(st_area({.}))) %>%
    filter(area >= quantile(area, thresh))

  print(sprintf("all valid: %s", all(y %>% st_is_valid())))
  if(verbose){
    #plot(y,  max.plot = 1)
    pl <- ggplot2::ggplot(y) + geom_sf(fill = "black", col = "black", size = 0.1) + theme_bw()
    print(pl)
  }
  file.name <- file.path("data-raw", sprintf("%s_%spct.shp", name, keep*100))
  print(file.name)
  st_write(y, file.name, append = F)
  y
}
## --------------------------- Process ------------------------------

ch_country_5_pct <- shp_simplify(x = shp.ch.country, name = "ch_country", keep = .05)
ch_country_1_pct <- shp_simplify(x = shp.ch.country, name = "ch_country", keep = .01)
ch_canton_5_pct <- shp_simplify(x = shp.ch.canton, name = "ch_canton", keep = .05)
ch_canton_1_pct <- shp_simplify(x = shp.ch.canton, name = "ch_canton", keep = .01)
ch_bezirk_5_pct <- shp_simplify(x = shp.ch.bezirk, name = "ch_bezirk", keep = .05)
ch_bezirk_1_pct <- shp_simplify(x = shp.ch.bezirk, name = "ch_bezirk", keep = .01)

ch_lake_5_pct <- shp_simplify(x = shp.ch.lake, name = "ch_lake", keep = .05, thresh = 0)
ch_lake_10_pct <- shp_simplify(x = shp.ch.lake, name = "ch_lake", keep = .1, thresh = 0)
ch_river_5_pct <- shp_simplify(x = shp.ch.river, name = "ch_river", keep = .05, thresh = 0)
ch_river_10_pct <- shp_simplify(x = shp.ch.river, name = "ch_river", keep = .1, thresh = 0)

## --------------------------- Register ------------------------------
ch.ctry <- sf::read_sf("inst/shp/ch_country_5pct.shp")
ch.canton <- sf::read_sf("inst/shp/ch_canton_5pct.shp")
ch.bezirk <- sf::read_sf("inst/shp/ch_bezirk_5pct.shp")
ch.lake <- sf::read_sf("inst/shp/ch_lake_5pct.shp")
ch.river<- sf::read_sf("inst/shp/ch_river_5pct.shp")

usethis::use_data(
  ch.ctry,
  ch.canton,
  ch.bezirk,
  ch.lake,
  ch.river,
 internal=FALSE, overwrite=TRUE)
