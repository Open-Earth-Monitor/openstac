library(mongolite)

update_db <- function(db, collection, overwrite) {
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
    item$bbox[c(1, 2)] <- pmax(item$bbox[c(1, 2)], c(-180, -90))
    item$bbox[c(3, 4)] <- pmin(item$bbox[c(3, 4)], c(180, 90))
    item$geometry$coordinates[[1]] <-
      lapply(item$geometry$coordinates[[1]], function(coord) {
        coord <- pmax(coord, c(-180, -90))
        coord <- pmin(coord, c(180, 90))
      })
    # ... fix datetime
    item$properties$datetime <- openstac:::mongo_datetime(openstac:::as_datetime(item$properties$datetime))
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
  if (!openstac:::db_collections_id_exist(db, collection$id) || overwrite)
    db$collections$update(
      query = jsonlite::toJSON(collection, auto_unbox = TRUE, null = "list"),
      upsert = TRUE
    )
  for (item in items$features) {
    if (!openstac:::db_items_id_exist(db, collection$id, item$id) || overwrite)
      #print(item)
      db$items$update(
        query = jsonlite::toJSON(item, auto_unbox = TRUE, null = "list"),
        upsert = TRUE
      )
  }
  #db$collections$run('{"dropDatabase": 1}')
}

create_db <- function(catalog_url, db_name, db_url, overwrite = FALSE) {
  rel <- NULL
  catalog <- rstac::read_stac(catalog_url)
  # prepare db
  db <- openstac:::new_db("mongodb", db = db_name, url = db_url)
  db$collections$run('{"createIndexes": "collections", "indexes":[{"key":{"id":1}, "name":"id_index", "unique": true}]}')
  db$items$run('{"createIndexes": "items", "indexes":[{"key":{"collection":1, "id":1}, "name":"collection_id_index", "unique": true}]}')
  db$items$index('{"geometry":"2dsphere"}')
  # filter collections
  links <- rstac::links(catalog, rel == "child")
  for (link in links[1]) {
    #print(link)
    collection <- rstac::link_open(link)
    # skip if collections is already in db and overwrite is FALSE
    update_db(db, collection, overwrite)
  }
}

# OpenLandMap
create_db(
  catalog_url = "https://s3.eu-central-1.wasabisys.com/stac/openlandmap/catalog.json",
  db_name = "openlandmap3",
  db_url = "mongodb://127.0.0.1:27017",
  overwrite = FALSE
)
