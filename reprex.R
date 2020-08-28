library(tibble)

# polygon of tectonic plates
plate <- tibble(lat = c(-42.059, -41.910, -41.756, -41.657, -41.500, -41.188,
                        -40.807, -40.424, -40.087, -39.685, -39.230, -38.889,
                        -38.538, -38.205, -37.476, -36.793, -36.179, -35.542,
                        -35.022, -34.706, -34.485, -34.241, -33.846, -33.580,
                        -33.191, -32.696, -32.203, -31.957, -31.793, -31.452,
                        -31.037, -30.678, -30.322, -29.881, -29.469, -29.065,
                        -28.697, -28.264, -27.784, -27.318, -26.857, -26.437,
                        -26.025, -25.730, -25.426, -25.142, -24.836, -24.472,
                        -24.070, -23.750, -23.750, -23.853, -23.952, -24.029,
                        -24.105, -24.105, -24.242, -24.720, -25.197, -25.976,
                        -26.767, -27.377, -27.985, -28.693, -29.498, -29.792,
                        -30.355, -30.997, -31.333, -31.751, -32.347, -33.019,
                        -33.602, -34.246, -34.786, -35.161, -35.917, -36.461,
                        -37.102, -37.485, -37.885, -38.287, -38.674, -38.762,
                        -39.230, -40.046, -40.311, -40.598, -40.791, -40.979,
                        -41.157, -41.566, -41.757, -42.059, -42.059),
                lon = c(175.503,  176.081 , 176.673,  177.123,  177.607,
                        178.015,  178.284,  178.566,  178.792,  178.950,
                        179.125,  179.215,  179.366,  179.569,  179.838,
                        -179.811, -179.371, -179.044, -178.641, -178.539,
                        -178.413, -178.294, -178.208, -177.981, -177.810,
                        -177.666, -177.649, -177.538, -177.301, -177.108,
                        -176.919, -176.690, -176.547, -176.339, -176.153,
                        -176.078, -175.995, -175.885, -175.785, -175.545,
                        -175.400, -175.423, -175.415, -175.382, -175.297,
                        -175.261, -175.229, -175.209, -175.102, -174.985,
                        -174.985, -175.691, -176.356, -176.887, -177.419,
                        -177.419, -177.448, -177.552, -177.657, -177.793,
                        -178.017, -178.248, -178.482, -178.713, -178.966,
                        -179.074, -179.226, -179.393, -179.515, -179.740,
                        179.980,  179.657,  179.345,  178.996,  178.681,
                        178.403,  177.890,  177.507,  177.049,  176.770,
                        176.509,  176.241,  175.995,  175.932,  175.609,
                        176.074,  175.868,  175.537,  175.324,  175.012,
                        174.632,  174.763,  174.945,  175.503,  175.503)
)

library(ggplot2)
library(dplyr)

# world data to map world map
world <- map_data("world")

# world map with tectonic plate in green
world %>%
  ggplot() +
  geom_map(map = world,
           aes(x = long, y = lat,
               map_id = region)) +
  geom_polygon(data = plate,
               aes(x = lon,
                   y = lat),
               fill = NA,
               colour = "dark green")


library(sf)
plate.sf <- st_polygon(x = list(as.matrix(plate %>% select(lon, lat)))) %>%
  st_wrap_dateline()

world %>%
  ggplot() +
  geom_map(map = world,
           aes(x = long, y = lat, map_id = region)) +
  geom_sf(data = plate.sf,
          colour = "dark green")

plate2 <- lapply(seq_along(plate.sf),
                 function(i) as.data.frame(plate.sf[[i]][[1]]) %>%
                   rename(lon = V1, lat = V2) %>%
                   mutate(group = i)) %>%
  data.table::rbindlist()

world %>%
  ggplot() +
  geom_map(map = world,
           aes(x = long,
               y = lat,
               map_id = region)) +
  geom_polygon(data = plate2,
               aes(x = lon, y = lat, group = group),
               fill = NA,
               colour = "dark green")

# ------------ Multiple plates
# mock up data frame with 2 distinct plates (mirror image of each other)
plates <- rbind(plate %>% mutate(plate = 1),
                plate %>% mutate(lat = -lat, plate = 2)) %>%
  select(plate, lat, lon)

# process data for geom_polygon approach
plates2 <- plates %>%

  # split into separate data frame for each plate
  split(.$plate) %>%

  # convert to polygon & split along date line (as before)
  lapply(function(d) d %>% select(lon, lat) %>%
           as.matrix() %>%
           list() %>%
           st_polygon() %>%
           st_wrap_dateline()) %>%

  # convert each plate back to data frame (as before)
  lapply(function(d) lapply(seq_along(d),
                            function(i) as.data.frame(d[[i]][[1]]) %>%
                              rename(lon = V1, lat = V2) %>%
                              mutate(group = i)) %>%
           data.table::rbindlist()) %>%
  # combine into one overall data frame
  bind_rows(.id = "plate") %>%
  mutate(group = paste(plate, group, sep = "."))

# result
world %>%
  ggplot() +
  geom_map(map = world,
           aes(x = long, y = lat,
               map_id = region)) +
  geom_polygon(data = plates2,
               aes(x = lon, y = lat, group = group),
               fill = NA, colour = "dark green")

