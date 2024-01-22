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

get_collections <- function(db) {
  UseMethod("get_collections", db)
}

get_collection <- function(db, collection_id) {
  UseMethod("get_collection", db)
}

get_items <- function(db,
                      collection_id,
                      ids,
                      limit,
                      geometry,
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
                         limit = NULL,
                         bbox = NULL,
                         datetime = NULL,
                         intersects = NULL,
                         ids = NULL,
                         collections = NULL,
                         page = NULL) {
  UseMethod("search_items", db)
}
