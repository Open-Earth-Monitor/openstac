drivers <- function() c("local")

set_db <- function(api, driver, ...) {
  db <- new_db(driver, ...)
  set_attr(api, "db", db)
}

get_db <- function(api) {
  stopifnot(exists("db", envir = get_env(api)))
  get_attr(api, "db")
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
                     items_id,
                     exact_date,
                     start_date,
                     end_date,
                     bbox,
                     intersects) {
  UseMethod("db_items", db)
}

db_cache <- function(db) {
  attr(db, "cache")
}

is_cached <- function(cache, name) {
  name <- paste0(name, collapse = "_")
  exists(name, envir = cache)
}

get_cache <- function(cache, name) {
  name <- paste0(name, collapse = "_")
  get(name, envir = cache)
}

set_cache <- function(cache, name, value) {
  name <- paste0(name, collapse = "_")
  assign(name, value, envir = cache)
}

get_items <- function(db,
                      collection_id,
                      limit,
                      bbox,
                      exact_date,
                      start_date,
                      end_date,
                      page) {
  UseMethod("get_items", db)
}

get_item <- function(db, collection_id, item_id) {
  UseMethod("get_item", db)
}

search_items <- function(db,
                         limit,
                         bbox,
                         exact_date,
                         start_date,
                         end_date,
                         intersects,
                         ids,
                         collections,
                         page) {
  UseMethod("search_items", db)
}
