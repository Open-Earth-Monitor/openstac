library(mongolite)

update_db <- function(db, collection) {
  if (!requireNamespace("rstac"))
    stop("Package rstac was not found. ",
         'Please, run `install.packages("rstac")` to install it.',
         call. = FALSE)
  if (!requireNamespace("sf"))
    stop("Package sf was not found. ",
         'Please, run `install.packages("sf")` to install it.',
         call. = FALSE)
  # item fixing function
  fix_item <- function(item, collection_id) {
    # ... fix latlong -> longlat
    item$bbox <- lapply(item$bbox, as.numeric)
    if (abs(item$bbox[[2]]) > 90) {
      item$bbox <- item$bbox[c(2, 1, 4, 3)]
      item$geometry$coordinates[[1]] <-
        lapply(item$geometry$coordinates[[1]], function(coord) {
          coord[c(2, 1)]
        })
    }
    # ... fix gsd
    if ("gsd" %in% names(item$properties))
      item$properties$gsd <- as.numeric(item$properties$gsd)
    # ... fix collection
    item$collection <- collection_id
    item$links <- NULL
    item
  }
  # fetch items
  items <- rstac::read_items(collection, limit = 10000, page = 1)
  # fix collection
  collection$links <- NULL
  collection$extent$spatial$bbox[[1]] <- lapply(collection$extent$spatial$bbox[[1]], as.numeric)
  if (abs(collection$extent$spatial$bbox[[1]][[2]]) > 90) {
    collection$extent$spatial$bbox[[1]] <- collection$extent$spatial$bbox[[1]][c(2, 1, 4, 3)]
  }
  # fix items
  items$features <- lapply(items$features, fix_item, collection$id)
  items$links <- NULL
  # update db
  # ... call mongodb ingest functions <<<<<<<<<<<<<<
  db$collections$push(collection)
  for (item in items$features) {
    db$items$push(item)
  }
}

create_db <- function(catalog_url, db_name, db_url, overwrite = FALSE) {
  rel <- NULL
  catalog <- rstac::stac_read(catalog_url)
  # prepare db
  db <- list(
    collections = mongolite::mongo(collection = "collections", db = db_name, url = db_url),
    items = mongolite::mongo(collection = "items", db = db_name, url = db_url)
  )
  # filter collections
  links <- rstac::links(catalog, rel == "child")
  for (link in links) {
    print(link)
    collection <- rstac::link_open(link)
    # skip if collections is already in db and overwrite is FALSE
    if (collection$id %in% names(db$collections) && !overwrite) next
    update_db(db, collection)
  }
}

# OpenLandMap
create_db(
  catalog_url = "https://s3.eu-central-1.wasabisys.com/stac/openlandmap/catalog.json",
  db_name = "docker/olm/openlandmap.rds",
  db_url = "mongodb::/localhost:3453",
  overwrite = FALSE
)
