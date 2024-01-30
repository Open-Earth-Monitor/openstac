drivers <- function() c("local")

#' @export
set_db <- function(api, driver, ...) {
  db <- new_db(driver, ...)
  api_set_attr(api, "db", db)
}

get_db <- function(api) {
  api_stopifnot(
    value = exists("db", envir = api_env(api)),
    code = 500
  )
  api_attr(api, "db")
}

new_db <- function(driver, ...) {
  api_stopifnot(
    value = is.character(driver),
    code = 500
  )
  api_stopifnot(
    value = driver %in% drivers(),
    code = 500
  )
  class(driver) <- driver
  UseMethod("new_db", driver)
}

db_collections_id_exist <- function(db, ids) {
  UseMethod("db_collections_id_exist", db)
}

#' @export
db_collections <- function(db) {
  UseMethod("db_collections", db)
}

db_collection <- function(db, collection_id) {
  UseMethod("db_collection", db)
}

db_items_id_exist <- function(db, collection_id, ids) {
  UseMethod("db_items_id_exist", db)
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
