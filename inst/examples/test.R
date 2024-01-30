library(rstac)
library(sf)


append_db <- function(collection, file) {
  fix_item <- function(item, collection_id) {
    # ... fix latlong -> longlat
    if (abs(item$bbox[[2]]) > 90) {
      item$bbox <- item$bbox[c(2, 1, 4, 3)]
      item$geometry$coordinates[[1]] <-
        lapply(item$geometry$coordinates[[1]], function(coord) {
          coord[c(2, 1)]
        })
    }
    # ... fix gsd
    if ("gsd" %in% names(item))
      item$gsd <- as.numeric(item$gsd)
    # ... fix collection
    item$collection <- collection_id
    item$links <- NULL
    item
  }

  # fetch data
  items <- rstac::read_items(collection, limit = 10000, page = 1)
  # fix items
  collection$links <- NULL
  items$features <- lapply(items$features, fix_item, collection$id)
  items$links <- NULL
  # prepare db
  db <- list(collections = list(), items = list())
  if (file.exists(file))
    db <- readRDS(file)
  db$collections[[collection$id]] <- collection
  db$items[[collection$id]] <- items
  saveRDS(db, file)
}

catalog <- rstac::stac_read("https://s3.eu-central-1.wasabisys.com/stac/openlandmap/catalog.json")
col_links <- rstac::links(catalog, rel == "child")
for (link in col_links) {
  print(link)
  collection <- rstac::link_open(link)
  append_db(
    collection = collection,
    file = "~/openlandmap.rds"
  )
}
