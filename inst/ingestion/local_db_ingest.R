library(rstac)
library(sf)

update_db <- function(db, collection) {
  if (!requireNamespace("rstac"))
    stop("Package rstac was not found. ",
         'Please, run `install.packages("rstac")` to install it.',
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
  # fix items
  collection$links <- NULL
  items$features <- lapply(items$features, fix_item, collection$id)
  items$links <- NULL
  # update db
  db$collections[[collection$id]] <- collection
  db$items[[collection$id]] <- items
  db
}

create_db <- function(catalog_url, db_file, overwrite = FALSE) {
  catalog <- rstac::stac_read(catalog_url)
  # prepare db
  db <- list(collections = list(), items = list())
  if (file.exists(db_file))
    db <- readRDS(db_file)
  # filter collections
  links <- rstac::links(catalog, rel == "child")
  for (link in links) {
    print(link)
    collection <- rstac::link_open(link)
    # skip if collections is already in db and overwrite is FALSE
    if (collection$id %in% names(db$collections) && !overwrite) next
    db <- update_db(db, collection)
    saveRDS(db, db_file)
  }
}

# OpenLandMap
create_db(
  catalog_url = "https://s3.eu-central-1.wasabisys.com/stac/openlandmap/catalog.json",
  db_file = "inst/local_db/openlandmap.rds",
  overwrite = FALSE
)
