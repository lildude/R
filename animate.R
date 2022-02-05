# load packages ####
# From https://padpadpadpad.github.io/post/animate-your-strava-activities-using-rstrava-and-gganimate/
library(rStrava) # devtools::install_github('fawda123/rStrava')
library(gganimate) # devtools::install_github('dgrtwo/gganimate')
library(dplyr)
library(tidyr)
library(purrr)
library(sp)
library(ggmap)
library(raster)

# initial setup ####
# Strava key
app_name <- Sys.getenv("STRAVA_NAME")
app_client_id <- Sys.getenv("STRAVA_CLIENT_ID")
app_secret <- Sys.getenv("STRAVA_APP_SECRET")
app_scope      <- 'activity:read_all'
cache = TRUE
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope, cache))
stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])

key <- Sys.getenv("GOOGLE_MAPS_API_KEY")

# download strava data
my_acts <- get_activity_list(stoken, after = as.Date('2022-01-21'))

length(my_acts)

# compile activities into a tidy dataframe
my_acts <- compile_activities(my_acts)

# have a look at the dataframe
dplyr::glimpse(my_acts)

# columns to keep
desired_columns <- c('distance', 'elapsed_time', 'moving_time', 'start_date', 'start_date_local', 'type', 'map.summary_polyline', 'upload_id', 'start_latitude', 'start_longitude')

# keep only desired columns
my_acts <- dplyr::select(my_acts, match(desired_columns, names(my_acts)))

# transformations ####
my_acts <- mutate(my_acts,
                  activity_no = seq(1,n(), 1),
                  elapsed_time = elapsed_time/60/60,
                  moving_time = moving_time/60/60,
                  date = gsub("T.*$", '', start_date) %>%
                    as.POSIXct(., format = '%Y-%m-%d'),
                  EUdate = format(date, '%d/%m/%Y'),
                  month = format(date, "%m"),
                  day = format(date, "%d"),
                  year = format(date, "%Y")) %>%
  mutate_at(., c('month', 'day'), as.numeric)


# get lat lon and distance of every ride ####
lat_lon <- my_acts %>%
  filter(!is.na(map.summary_polyline)) %>%
  nest(., -activity_no) %>%
  mutate(coords = map(data, get_latlon, key),
         distance = map(coords, ~get_dists(.x$lon, .x$lat))) %>%
  unnest(., data) %>%
  unnest(., coords, distance)

# Create gif of a single ride
lat_lon_single <- filter(lat_lon, activity_no == 2)
nrow(lat_lon_single)


# reorder columns so lat lon are first
lat_lon_single <- dplyr::select(lat_lon_single, lat, lon, everything())

# make new data with Duffy's method
interp <- raster::spLines(as.matrix(lat_lon_single[,1:2])) %>%
  sp::spsample(., n = 250, type = 'regular') %>%
  data.frame() %>%
  mutate(., distance = get_dists(lon, lat),
         ele = rgbif::elevation(latitude = .$lat, longitude = .$lon, key = GoogleAPI)$elevation,
         ele_diff = c(0, diff(ele)),
         dist_diff = c(0, diff(distance)),
         grad = c(0, (ele_diff[2:n()]/10)/dist_diff[2:n()]),
         n = row_number())

# make bbox
bbox <- ggmap::make_bbox(lon, lat, data = interp, f = 1.3)

# download map
map <- get_map(location = bbox, source = 'google', maptype = 'terrain')

single_ride <- ggmap(map, darken = 0.15) +
  geom_path(aes(x = lon, y = lat,  col = grad, group = 1, frame = n, cumulative = TRUE), data = interp, size = 2, alpha = 1) +
  scale_color_distiller('Gradient (%)', palette = 'Spectral') +
  labs(title = 'Ride to St Just and back') +
  coord_cartesian() +
  ggforce::theme_no_axes(theme_bw(base_size = 16))

# animate plot
animation::ani.options(interval = 1/20)
gganimate::gganimate(single_ride, title_frame = FALSE, 'where_you_want_to_save_it.gif', ani.width = 800, ani.height = 700)

