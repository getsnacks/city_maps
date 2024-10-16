## OpenStreetMap: Çanakkale

library(sf)
library(ggplot2)
library(osmdata)
library(showtext)
library(raster)
library(dplyr)

font_add_google(name = 'Roboto Condensed', family = 'roboto')
font_add_google(name = 'Abel', family = 'abel')
showtext_auto()

bb = c(26.2333, 40.0768, 26.5612, 40.2339) #for Assos

roads_all <- opq(bbox = bb) |>
  add_osm_feature(key = 'highway') |>
  osmdata_sf()

buildings <- opq(bb) |> 
  add_osm_feature(key = "building") %>%
  osmdata_sf()

landuse <- opq(bb) |>
  add_osm_feature(key = 'landuse', value = c('grass','forest')) |>
  osmdata_sf()

leisure <- opq(bb) |>
  add_osm_feature(key = 'leisure', value = c('garden','park')) |>
  osmdata_sf()

natural <- opq(bbox = bb) |>
  add_osm_feature(key = 'natural') |>
  osmdata_sf()

# circle
long = 26.3971
lat = 40.15535

center_proj <-
  tibble(long, lat) %>%
  st_as_sf(coords = c("long", "lat"), crs = crs(roads_all$osm_lines))

dist <-  6000
circle <- tibble(long, lat) %>%
  st_as_sf(coords = c("long", "lat"), crs = crs(roads_all$osm_lines)) %>%
  st_buffer(dist = dist) %>%
  st_transform(crs = crs(roads_all$osm_lines))


roads_all_lines <- st_intersection(circle, roads_all$osm_lines)
buildings_lines <- st_intersection(circle, buildings$osm_polygons)
landuse_lines <- st_intersection(circle, landuse$osm_polygons) 
leisure_lines <- st_intersection(circle, leisure$osm_polygons)
natural_lines <- st_intersection(circle, natural$osm_polygons)

# color palette from: https://x.com/AlexCristache/status/1842448450731786742/photo/3
ggplot() +
  geom_sf(data = landuse_lines, color = '#024a43', fill = '#024a43') +
  geom_sf(data = leisure_lines, color = '#142d25', fill = '#142d25') +
  geom_sf(data = natural_lines, color = '#142d25', fill = '#142d25')+
  geom_sf(data = buildings_lines, color = '#f2e3d8', fill = '#aa423a',
          linewidth = .3) +
  geom_sf(data = roads_all_lines, color = '#db783e',linewidth = .4) +
  theme_void() +
  theme(plot.caption = element_text(hjust = .5, color = '#024a43', 
                                    family = 'roboto', size = 90),
        plot.subtitle = element_text(hjust = .5, size = 40, family = 'abel',
                                     color = '#142d25'),
        plot.background = element_rect(color = NA, fill = '#f2e3d8')) +
  labs(caption = 'Canakkale/ Turkiye')

ggsave('ckale_map.png', height = 10, width = 8, dpi = 400)

# dark theme
ggplot() +
  geom_sf(data = landuse_lines, color = '#aa423a', fill = '#aa423a') +
  geom_sf(data = leisure_lines, color = '#db783e', fill = '#db783e') +
  geom_sf(data = natural_lines, color = '#db783e', fill = '#db783e')+
  geom_sf(data = buildings_lines, color = '#f2e3d8', fill = '#f2e3d8',
          linewidth = .3) +
  geom_sf(data = roads_all_lines, color = '#024a43',linewidth = .4) +
  theme_void() +
  theme(plot.caption = element_text(hjust = .5, color = '#f2e3d8', 
                                    family = 'roboto', size = 90),
        plot.subtitle = element_text(hjust = .5, size = 40, family = 'abel',
                                     color = '#f2e3d8'),
        plot.background = element_rect(color = NA, fill = '#142d25')) +
  labs(caption = 'Canakkale/ Turkiye')

ggsave('ckale_map_dark.png', height = 10, width = 8, dpi = 400)
