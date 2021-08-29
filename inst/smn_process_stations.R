##' ...............................................................................
##'  SMN: Process Stations
##' ...............................................................................
##'  M. Sänger  07/2019, 01/2021
##' ...............................................................................
##'
##'
##' QGIS ops:
##' - r.geomorphon
##' - r.horizon



library(data.table)
library(raster)
library(dplyr)
library(sf)
library(wx)
library(wxPlot)
library(ggplot2)

dir.geo <- "~/Data/Geodata"
dir.out <- file.path(dir.geo, "SMN")
res <- 25L
intp.method <- "bilinear"
ext.test <- c(xmin = 720000, xmax = 760000, ymin = 220000, ymax = 250000)

# Empty brick
br.25m <- raster::brick(extent(ext.test), crs = lib.crs$lv03$crs)
res(br) <- res

## -------------------------------- Meta -------------------------------------------
geomorphon <- read.csv("inst/landform/geomorphon.csv", stringsAsFactors = F)
ternary.pattern <- as.matrix(read.csv("inst/landform/ternary_pattern.csv", stringsAsFactors = F))
ternary.pattern.lut <- matrix(match(ternary.pattern, geomorphon$id_lf), nrow = nrow(ternary.pattern))
col.areal <- read.csv("inst/landform/areal_classification.csv", stringsAsFactors = F)

## -------------------------------- SMN -------------------------------------------

sf.smn.stat <- smn.stat.sensor %>%
  select(id_sma, code_var, lon, lat) %>%
  mutate(to = Sys.Date())  %>%
  st_as_sf(coords = c("lon", "lat"), crs = lib.crs$lv95$crs) %>%
  st_transform(crs = lib.crs$lv03$crs)

sf.smn.stat.hist  <-  smn.stat.sensor.hist %>%
  dplyr::select(id_sma, code_var, to, x, y) %>%
  filter(!is.na(x), !is.na(to), y < 48) %>%
  st_as_sf(coords = c("x", "y"), crs = lib.crs$wgs84$crs) %>%
  st_transform(crs = lib.crs$lv03$crs)

sf.smn <- bind_rows(sf.smn.stat, sf.smn.stat.hist) %>% arrange(id_sma, to, code_var)

ggplot(sf.smn %>% filter(code_var == "tt")) +
  geom_sf(data = ch_canton_5_pct %>%  st_transform(crs = lib.crs$lv03$crs), fill = NA) +
  geom_sf(data = ch_lake_5_pct %>%  st_transform(crs = lib.crs$lv03$crs), fill = "lightblue", colour = "dodgerblue2") +
  geom_sf(aes(colour = to)) +
  theme_bw()

## -------------------------------- Data Geo -------------------------------------------
#DHM25 Res: 25m LV03 LN02
#SRTM
r.dhm25 <- raster(file.path(dir.geo, "CHE/dhm25.asc"), crs = lib.crs$lv03$crs)
r.dhm100.slope <- raster(file.path(dir.geo, "DEM/DHM25/dhm100_slope.tif"))
plot(r.dhm100.slope)

r.test <- crop(r.dhm25, extent(ext.test))
raster::writeRaster(r.test, file.path(dir.geo, "CHE/dhm25_demo.asc"))

#  slope, aspect, TPI, TRI, roughness, flowdir
slope = terrain(r.test, opt="slope", unit="radians", neighbors=8)

plot(slope)
coord <- st_coordinates(sf.smn.stat.lv03)
points(x = coord[,1], y = coord[,2])


# Focal function
lf = function(x, na.rm = T) geomorph(x, ncell = ncell(r.test), flatness.thresh = .5, res = 25, verbose = T)
focal.matrix <- matrix(1, nrow = 9, ncol = 9)
geomorph <- raster::focal(r.test, fun = lf , w = focal.matrix, pad = T, padValue = NA)

plot(geomorph)

## -------------------------------- Data BFS -------------------------------------------

# Res: 100m

dt.areal.in <- fread(file.path(dir.geo, "/CHE/AREA_NOAS04_17_161114.csv"))
dt.areal <- melt(dt.areal.in, id.vars = c("X", "Y", "GMDE", "RELI"), measure.vars = "AS09_17")
r.areal.ch <- rasterFromXYZ(dt.areal[, .(X, Y, value)], crs = lib.crs$lv03$crs)
names(r.areal.ch) <- "lu"
# plot(r.areal.ch, breaks = col.area$id_areal, col = col.area$colour)

## -------------------------------- Focal -------------------------------------------

buffer <- 250
size <- buffer/res
width <- 2*size + 1
n.center <- (width^2 + 1)/2
n <- (buffer/res * 2)^2
m <-  matrix(1, nrow = width, ncol = width)

lib.focal <- list(
  mean = function(x, na.rm = T) mean(x, na.rm = T),
  rel_mean = function(x, na.rm = T) x[n.center] - mean(x, na.rm = T),
  rel_median = function(x, na.rm = T) x[n.center] - mean(x, na.rm = T),
  lu_concrete = function(x, na.rm = T) sum(case_when(x %in% 1:4 ~ 1, T ~ 0), na.rm = T)/width^2,
  lu_water = function(x, na.rm = T) sum(case_when(x %in% 13:14 ~ 1, T ~ 0), na.rm = T)/width^2,
  lf = function(x, na.rm = T) geomorph(x, ncell = ncell(br), flatness.thresh = .5, res = res.ch)
)

r.mean <- raster::focal(r.test, m, lib.focal$rel_mean, na.rm = T, pad = T)
plot(r.mean)

## -------------------------------- Extract Areal -------------------------------------------
seq.buffer <- c(250, 500, 2000)

list.smn.areal <- lapply(seq.buffer, function(i){
  # i = 2000
  sf.buffer <- sf.smn %>% st_buffer(i) %>% mutate(ID = row_number())
  p <- raster::extract(r.areal.ch, sf.buffer, buffer = NULL, df = T, cellnumbers = T, method = 'simple')
  xy <- cbind(ID = p$ID, lu = p$lu, xyFromCell(r.areal.ch, p[,2]))

  sf.buffer.1 <- sf.buffer %>%
    st_drop_geometry() %>%
    left_join(xy, by = "ID", copy = T)

  # rrr <- rasterFromXYZ(sf.buffer.1 %>% filter(id_sma == "GVE", code_var == "tt", to == "2021-01-19") %>% select(x,y,lu))
  # plot(rrr, breaks = col.area$id_areal, col = col.area$colour)
  as.data.frame(sf.buffer.1)
})
dt.smn.areal <- rbindlist(list.smn.areal, idcol = "buffer")
dt.smn.areal[, pixel := .N, .(id_sma, code_var, to)]
dt.smn.areal[, .N, .(id_sma, code_var, to, lu)]


# Extract circle
sf.buffer <- sf.smn %>% dplyr::filter(id_sma == "BAS") %>% st_buffer(5000)

yyy <- raster::extract(r.areal.ch, xxx, buffer = NULL, df = T, cellnumbers = T, method = 'simple')
ex.df.coords <- cbind(yyy, xyFromCell(r.areal.ch, yyy[,2]))
rrr <- rasterFromXYZ(ex.df.coords[, c(4, 5, 3)])
plot(rrr, breaks = col.area$id_areal, col = col.area$colour)

## -------------------------------- Extract DEM -------------------------------------------
raster::extract(r.mean, sf.smn.stat.lv03[1:10, c("lon", "lat")], buffer = NULL, df = T, cellnumbers = T, method = 'simple')



## -------------------------------- Def -------------------------------------------

# res <- 1/1000L; res.ch <- 200L; res.out <- res; buffer <- 600L # hi -res
res <- 1/100L; res.ch <- 500L; res.out <- 1/250; buffer <- 3000L # lo -res
res <- 1/1000L; res.ch <- 300L; res.out <- 1/250; buffer <- 3000L # lo -res
res <- 1/200L; res.ch <- 300L; res.out <- 1/200; buffer <- 3000L
intp.method <- "bilinear"

x.range <- c(5.9, 10.6)
y.range <- c(45.7, 47.85)

x.ch.range <- c(475000L, 850000L)
y.ch.range <- c(66000L, 305000L)

ext <- c(x.range, y.range)
ext <- extent(ext)
ext.ch <- extent(c(x.ch.range, y.ch.range)) #projectExtent(ext, crs(crs.str.che03))

r.empty <- raster(ext, res = res, crs = crs.wgs84)
r.out <- raster(ext, res = res.out, crs = crs.wgs84)
r.ch.empty <- raster(ext.ch, res = res.ch, crs = crs.str.che03) #projectExtent(r.empty, crs(crs.str.che03))

## -------------------------------- Station -------------------------------------------
cn <- sql_connect(cn.str.postgres, user = "postgres", dbname = "wx", schema = "smn")

tbl.station.sensor <- tbl(cn, "sensor") %>%
  #filter(code_var == "tt") %>%
  dplyr::select(id_sma, z, h_obj, obj)

dt.stat <- tbl(cn, "station") %>%
  dplyr::select(id_sma, name, x, y, z_mean = z) %>%
  left_join(tbl.station.sensor, by = "id_sma") %>%
  collect() %>%
  distinct(id_sma, .keep_all = T)  %>%
  mutate(
    lon = round(WGS.to.CH.y(y, x)),
    lat = round(WGS.to.CH.x(y, x)),
    z = case_when(is.na(z) ~ z_mean, TRUE ~ z)
  )

dbDisconnect(cn)

dt.landform <- sql_get(cn.str, dbname = "meta",  "SELECT * FROM landform")
m.lut.num <- sql_get(cn.str, dbname = "meta", "SELECT * FROM landform_lut")
dt.areal.meta <- sql_get(cn.str, "SELECT * FROM meta.areal_classification", "meta")
dt.windrose <- sql_get(cn.str, "SELECT * FROM meta.windrose;", "meta")

## -------------------------------- Data BFS -------------------------------------------
dt.areal.in <- fread(file.path(dir.geo, "/CHE/AREA_NOAS04_17_161114.csv"))
dt.areal <- melt(dt.areal.in, id.vars = c("X", "Y", "GMDE", "RELI"), measure.vars = "AS09_17")
r.areal.ch <- rasterFromXYZ(dt.areal[, .(X, Y, value)], crs = crs.str.che03)
names(r.areal.ch) <- "lu"

r.areal.ch <- raster::aggregate(r.areal.ch, fact = res.ch/100L, fun = modal)
r.areal.ch <- extend(r.areal.ch, r.ch.empty)
r.areal.ch <- projectRaster(r.areal.ch, r.ch.empty, method = "ngb")
# plot(r.areal.ch)

## -------------------------------- Geo -------------------------------------------

# DEM
r.dem <- readRDS(file.path(dir.geo, sprintf("RData/STRM_ch_wgs84_terrain_raster_%s.RDS", res)))
r.dem <- r.dem$z #names(r.dem) <- "z"

# shp <- readOGR(file.path(dir.geo, "RData", "CHE_lakes_simplified_0_01.shp"))
# dt.lake <- fortify(shp, id = "UUID")

# Terrain
br.terrain <- readRDS(file.path(dir.geo, "RData", paste0("STRM_", "ch", "_wgs84_terrain_raster_", res,".RDS")))

# Combine
br.ch <- projectExtent(br.terrain, crs(crs.str.che03))
br.ch <- projectRaster(br.terrain, r.ch.empty, method = intp.method)
#br.ch <- extend(br.ch, r.ch.empty)

br <- stack(br.ch, r.areal.ch)

# https://cran.r-project.org/web/packages/dynatopmodel/dynatopmodel.pdf
twi <- dynatopmodel::upslope.area(br.ch$z, atb = TRUE, deg = 1/100L, fill.sinks = T)
br$twi <- twi$atb

## -------------------------------- Process -------------------------------------------
#dt.stat <- bind_cols(dt.stat, as.data.frame(raster::extract(br, dt.stat %>% dplyr::select(lon, lat))))

size <- buffer/res.ch
width <- 2*size + 1
n.center <- (width^2 + 1)/2
n <- (buffer/res.ch * 2)^2

lib.matrix <- list(
  unif =  matrix(1, nrow = width, ncol = width)
)
lib.focal <- list(
  mean = function(x, na.rm = T) mean(x, na.rm = T),
  rel_mean = function(x, na.rm = T) x[n.center] - mean(x, na.rm = T),
  rel_median = function(x, na.rm = T) x[n.center] - mean(x, na.rm = T),
  lu_concrete = function(x, na.rm = T) sum(case_when(x %in% 1:4 ~ 1, T ~ 0), na.rm = T)/width^2,
  lu_water = function(x, na.rm = T) sum(case_when(x %in% 13:14 ~ 1, T ~ 0), na.rm = T)/width^2,
  lf = function(x, na.rm = T) geomorph(x, ncell = ncell(br), flatness.thresh = .5, res = res.ch)
)
lib.mod <- list(
  z_rel_mean = list(var = "z", fct = "rel_mean", m = "unif"),
  lu_pct_concrete = list(var = "lu", fct = "lu_concrete", m = "unif"),
  lu_pct_water = list(var = "lu", fct = "lu_water", m = "unif"),
  twi_outer = list(var = "twi", fct = "mean", m = "unif"),
  lf_outer =  list(var = "z", fct = "lf", m = "unif")
)
list.mod <- map(lib.mod, ~ focal(br[[.x$var]], w = lib.matrix[[.x$m]], fun = lib.focal[[.x$fct]]))
br.mod <- brick(list.mod)
br.out <- exec(stack, br, br.mod)

# Convert to WGS84
br.WGS84 <- projectRaster(br.out, r.out, method = intp.method)
plot(br.WGS84$twi_outer)

dt.br <- as.data.frame(br.WGS84, xy = T) %>% as.data.table()
dt.br[, `:=`(z_mod = ifelse(z_rel_mean < 0, z - z_rel_mean, z), res = res, buffer = buffer)]

dt.res <- bind_cols(
  dt.stat %>% rename(z_stat = z),
  raster::extract(br.WGS84, dt.stat %>% dplyr::select(x, y), buffer = NULL, df = T, cellnumbers = T, method = 'simple')
  ) %>%
  mutate(z_mod = ifelse(z_rel_mean < 0, z - z_rel_mean, z), res = res, buffer = buffer) %>%
  as.data.table

copy.table(dt.res)

saveRDS(dt.res, file.path(dir.data, sprintf("smn_meta_%s.RDS", res)))
saveRDS(dt.br, file.path(dir.data, sprintf("smn_dem_%s.RDS", res)))


