library(rstac)

collection <- rstac::stac_read("https://s3.eu-central-1.wasabisys.com/stac/openlandmap/wv_mcd19a2v061.seasconv/collection.json")
items <- rstac::read_items(collection, limit = 1000, page = 1)
# ...fix latlong -> longlat
fix_longlat <- function(item) {
  item$bbox <- item$bbox[c(2, 1, 4, 3)]
  item$geometry$coordinates[[1]] <-
    lapply(item$geometry$coordinates[[1]], function(coord) {
      coord[c(2, 1)]
    })
  item
}
items2 <- items
items2$features <- lapply(items$features, fix_longlat)
items2$features[[1]]$geometry
items$features[[1]]$geometry

data <- structure(
  lapply(items2$features[[1]]$geometry$coordinates, \(x){
    coords <- matrix(unlist(x), ncol = 2, byrow = TRUE)
    structure(c(coords), dim = dim(coords))
  }),
  class = c("XY", "POLYGON", "sfg")
)
dput(data)

# filter by item_id
items_filter(items2, id == "wv_mcd19a2v061.seasconv_20220701_20220731")$features[[1]]

# filter by datetime
start_date <- "2018-01-01"
end_date <- "2018-12-31"
items_filter(items2, as.Date(properties$datetime) >= {{start_date}}) |>
  items_filter(as.Date(properties$datetime) <= {{end_date}})

# filter by space
library(sf)
sf_use_s2(FALSE)

geom <- sf::st_sfc(sf::st_point(c(-10, -10)), crs = 4326)
geom <- sf::st_transform(geom, crs = 4326)

items_sf <- rstac::items_as_sf(items2)

select <- apply(sf::st_intersects(
  x = items_sf,
  y = geom,
  prepared = T
), 1, length) > 0

items3 <- items2
items3$features <- items2$features[select]
items3




# reproducible example of intersection issue with s2 engine

point <- st_sfc(st_point(c(-10, -10)), crs = 4326)
geometry <- st_sfc(
  st_polygon(list(
    rbind(c(-180, -62),
          c(179, -62),
          c(179, 87),
          c(-180, 87),
          c(-180, -62)))),
  crs = 4326
)
sf_use_s2(TRUE)
st_intersects(geometry, point, sparse = F)
sf_use_s2(FALSE)
st_intersects(geometry, point, sparse = F)
