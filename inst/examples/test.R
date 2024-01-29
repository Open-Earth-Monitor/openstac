library(rstac)
library(sf)

collection <- rstac::stac_read("https://s3.eu-central-1.wasabisys.com/stac/openlandmap/wv_mcd19a2v061.seasconv/collection.json")
items <- rstac::read_items(collection, limit = 1000, page = 1)

# TODO: properties$gsd -> int

append_db <- function(collection, items, file) {
  # ...fix latlong -> longlat
  fix_longlat <- function(item) {
    if (abs(item$bbox[[2]]) > 90) {
      item$bbox <- item$bbox[c(2, 1, 4, 3)]
      item$geometry$coordinates[[1]] <-
        lapply(item$geometry$coordinates[[1]], function(coord) {
          coord[c(2, 1)]
        })
    }
    item
  }
  items$features <- lapply(items$features, fix_longlat)
  # erase links
  collection$links <- NULL
  items$features <- lapply(items$features, \(x) {x$links <- NULL; x})
  items$links <- NULL
  # prepare db
  db <- list(collections = list(), items = list())
  if (file.exists(file))
    db <- readRDS(file)
  db$collections[[collection$id]] <- collection
  db$items[[collection$id]] <- items
  saveRDS(db, file)
}

append_db(collection, items, "~/stac-local.rds")


data <- structure(
  lapply(items$features[[1]]$geometry$coordinates, \(x){
    coords <- matrix(unlist(x), ncol = 2, byrow = TRUE)
    structure(c(coords), dim = dim(coords))
  }),
  class = c("XY", "POLYGON", "sfg")
)
plot(data)

# filter by item_id
items_filter(items, id == "wv_mcd19a2v061.seasconv_20220701_20220731")$features[[1]]

# filter by datetime
start_date <- "2018-01-01"
end_date <- "2018-12-31"
items_filter(items, as.Date(properties$datetime) >= {{start_date}}) |>
  items_filter(as.Date(properties$datetime) <= {{end_date}})

# filter by space
sf_use_s2(FALSE)

geom <- sf::st_sfc(sf::st_point(c(-10, -10)), crs = 4326)
geom <- sf::st_transform(geom, crs = 4326)

items_sf <- rstac::items_as_sf(items)

select <- apply(sf::st_intersects(
  x = items_sf,
  y = geom,
  prepared = T
), 1, length) > 0

items3 <- items
items$features <- items$features[select]
items




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
