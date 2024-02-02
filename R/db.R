drivers <- function() c("local")

new_db <- function(driver, ...) {
  stopifnot(is.character(driver))
  stopifnot(driver %in% drivers())
  class(driver) <- driver
  UseMethod("new_db", driver)
}

db_collections_id_exist <- function(db, ids) {
  UseMethod("db_collections_id_exist", db)
}

db_collections <- function(db) {
  UseMethod("db_collections", db)
}

db_collection <- function(db, collection_id) {
  UseMethod("db_collection", db)
}

db_items_id_exist <- function(db, collection_id, ids) {
  UseMethod("db_items_id_exist", db)
}

db_items <- function(db, collection_id, limit, bbox, datetime, page) {
  UseMethod("db_items", db)
}

db_item <- function(db, collection_id, item_id) {
  UseMethod("db_item", db)
}

db_search <- function(db,
                      limit,
                      bbox,
                      datetime,
                      intersects,
                      ids,
                      collections,
                      page) {
  UseMethod("db_search", db)
}
