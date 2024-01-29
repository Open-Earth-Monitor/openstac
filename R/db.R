drivers <- function() c("local")

set_db <- function(api, driver, ...) {
  db <- new_db(driver, ...)
  api_set_attr(api, "db", db)
}

get_db <- function(api) {
  stopifnot(exists("db", envir = api_env(api)))
  api_attr(api, "db")
}

new_db <- function(driver, ...) {
  stopifnot(is.character(driver))
  stopifnot(driver %in% drivers())
  class(driver) <- driver
  UseMethod("new_db", driver)
}

db_collections_id <- function(db) {
  UseMethod("db_collections_id", db)
}

#' @export
db_collections <- function(db) {
  UseMethod("db_collections", db)
}

db_collection <- function(db, collection_id) {
  UseMethod("db_collection", db)
}

db_items_id <- function(db, collection_id) {
  UseMethod("db_items_id", db)
}

db_items <- function(db,
                     collection_id,
                     limit,
                     bbox,
                     exact_date,
                     start_date,
                     end_date,
                     page) {
  UseMethod("db_items", db)
}

db_item <- function(db, collection_id, item_id) {
  UseMethod("db_item", db)
}

db_search <- function(db,
                      limit,
                      bbox,
                      exact_date,
                      start_date,
                      end_date,
                      intersects,
                      ids,
                      collections,
                      page) {
  UseMethod("db_search", db)
}
