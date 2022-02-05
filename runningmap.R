# Code written by Daniel Cullen
# https://github.com/d-cullen/dcullen.github.io/blob/b566b498aa9dafe0027eb177513d6dd49d1d7ad5/R%20code/runningmap.R
#
# Using code from the following sources:
# https://padpadpadpad.github.io/post/animate-your-strava-activities-using-rstrava-and-gganimate/
# https://medium.com/@annthurium/getting-started-with-the-strava-api-a-tutorial-f3909496cd2d
# https://www.r-bloggers.com/where-do-you-run-to-map-your-strava-activities-on-static-and-leaflet-maps/
# http://www.databrew.cc/posts/strava.html
# https://github.com/fawda123/rStrava
library(rStrava) # devtools::install_github('fawda123/rStrava')
library(dplyr)
library(tidyr)
library(purrr)
library(sp)
library(raster)
library(leaflet)
library(htmlwidgets)

setwd("~/tmp")

## Necessary info from Strava api https://www.strava.com/settings/api
app_name       <- Sys.getenv("STRAVA_NAME") # chosen by user
app_client_id  <- Sys.getenv("STRAVA_CLIENT_ID")     # an integer, assigned by Strava
app_secret     <- Sys.getenv("STRAVA_APP_SECRET") # an alphanumeric secret, assigned by Strava
app_scope      <- 'activity:read_all'
cache = TRUE
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope, cache))
stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])

key <- Sys.getenv("GOOGLE_MAPS_API_KEY")

my_data  <- get_activity_list(stoken, after = as.Date('2019-01-01'))
act_data <- compile_activities(my_data)

## keep only activity id and map line
keeps <- c('map.summary_polyline', 'upload_id', 'type')
my_acts <- dplyr::select(act_data, match(keeps, names(act_data)))

#dplyr::glimpse(my_acts)

## convert map polyline to collection of lat lon coordinates
lat_lon <- my_acts %>%
  filter(type == 'Run') %>%
  filter(!is.na(map.summary_polyline)) %>%
  nest(., -upload_id, -type) %>%
  mutate(coords = map(data, get_latlon, key),
         distance = map(coords, ~get_dists(.x$lon, .x$lat))) %>%
  unnest(., data) %>%
  unnest(., coords)

#dplyr::glimpse(lat_lon)

## Create blank map bounded by given lon and lat
#51.441545, -0.922193
lats.range <- c(51.6, 51.3)
lons.range <- c(-0.9, -0.93)
## tile options CartoDB.Positron , CartoDB.DarkMatter , Stamen.Toner
map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles('CartoDB.Positron',
                   options = providerTileOptions(noWrap = T, minZoom=12, maxZoom=12)) %>%
  fitBounds(lng1 = min(lons.range), lat1 = max(lats.range), lng2 <- max(lons.range), lat2 = min(lats.range))

## Loop through each activity to add activity to map
loop <- unique(lat_lon$upload_id)
for (i in loop) {
  lat_lon_single <- filter(lat_lon, upload_id == i)

  ## reorder columns so lat lon are first
  lat_lon_single <- dplyr::select(lat_lon_single, lat, lon, everything())

  interp <- raster::spLines(as.matrix(lat_lon_single[,1:2])) %>%
    sp::spsample(., n = 250, type = 'regular') %>%
    data.frame() %>%
    mutate(., distance = get_dists(lon, lat),
           n = row_number())

  map <- addPolylines(map, lng = interp$lon, lat = interp$lat,
                      color = 'blue', opacity = 1/4, weight = 2)
}
map

saveWidget(map, file="runningmap.html")



