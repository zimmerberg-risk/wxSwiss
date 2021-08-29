Sys.setenv(TZ = 'UTC')
Sys.setlocale(locale = "en_GB.UTF-8")

library(data.table)
library(sf)
library(ggplot2)
library(wxSwiss)

dt <- parse_mch_VQHA80(remote = TRUE)
dt.1 <- dt[smn.stn, on = "stn"]
dt.2 <- dt.1[para == "ff"]
sf.data <- sf::st_as_sf(dt.2, coords = c("x", "y"), crs = 4326)

ggplot() +
  geom_sf(data = ch.ctry, fill = "white") +
  geom_sf(data = ch.canton, fill = NA, colour = "grey", size = .25) +
  geom_sf(data = ch.lake, fill = "lightblue") +
  geom_sf(aes(fill = value), sf.data, shape = 21, size = 3) +
  scale_fill_viridis_c() +
  geom_sf_text(aes(label = stn), sf.data, nudge_x = 5e3, nudge_y = 5e3) +
  coord_sf(crs = lib.crs$lv03$crs, expand = FALSE) +
  labs(x = NULL, y = NULL, caption = "wxSWiss Package | Data: MeteoSwiss, Swisstopo") +
  theme_bw()
