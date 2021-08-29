Sys.setenv(TZ = 'UTC')
Sys.setlocale(locale = "en_GB.UTF-8")

library(data.table)
library(sf)
library(ggplot2)
library(wxSwiss)

id.para = "pp"

dt.wx <- parse_mch_VQHA80(remote = TRUE)
dt.pp <- parse_mch_VQHA98(remote = TRUE)
dt <- rbind(dt.wx, dt.pp)

dt <- parse_mch_json(para = id.para, remote = TRUE)

dt.1 <- dt[para == id.para]
dt.2 <- smn.stn[dt.1, on = "stn"]

sf.data <- sf::st_as_sf(dt.2[!is.na(x)], coords = c("x", "y"), crs = 4326)

def.para <- smn.para[para == id.para]
t <- dt$time[1]
lab <- sprintf("%s [%s]", def.para$name_para_abbr, def.para$unit)
title <- sprintf("%s [%s] %s", def.para$name_para, def.para$unit, t)

ggplot() +
  geom_sf(data = ch.ctry, fill = "white") +
  geom_sf(data = ch.canton, fill = NA, colour = "grey", size = .25) +
  #geom_sf(data = ch.river, colour = "lightblue") +
  geom_sf(data = ch.lake, fill = "lightblue") +
  geom_sf(aes(fill = value), sf.data, shape = 21, size = 3) +
  scale_fill_viridis_c() +
  geom_sf_text(aes(label = stn), sf.data, nudge_x = 5e3, nudge_y = 5e3) +
  coord_sf(crs = lib.crs$lv03$crs, expand = FALSE) +
  labs(
    x = NULL,
    y = NULL,
    fill = lab,
    title = title,
    caption = "wxSwiss Package | Data: MeteoSwiss, Swisstopo") +
  theme_bw()
