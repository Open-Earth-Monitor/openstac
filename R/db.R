drivers <- function() c("local")

new_db <- function(driver, ...) {
  stopifnot(is.character(driver))
  stopifnot(driver %in% drivers())
  class(driver) <- driver
  UseMethod("new_connection", driver)
}

set_db <- function(api, driver, ...) {
  stopifnot(is.character(driver))
  stopifnot(driver %in% drivers())
  set_attr(api, "db", new_db(driver, ...))
}

get_db <- function(api) {
  get_attr(api, "db")
}

load_collections <- function(api) {
  db <- get_db(api)
  stopifnot(!is.null(db))
  UseMethod("load_collections", db)
}

get_collections <- function(api) {
  db <- get_db(api)
  stopifnot(!is.null(db))
  UseMethod("get_collections", db)
}

get_collection <- function(api, collection_id) {
  db <- get_db(api)
  stopifnot(!is.null(db))
  UseMethod("get_collection", db)
}

# ---- local ----

new_connection.local <- function(driver, file, ...) {
  stopifnot(file.exists(file))
  structure(
    c(list(driver = driver, file = file), list(...)),
    class = as.character(driver)
  )
}

load_collections.local <- function(api) {
  connection <- get_db(api)
  file <- connection$file
  stopifnot(file.exists(file))
  set_attr(api, "collections", readRDS(file))
}

get_collections.local <- function(api) {

}

get_collection.local <- function(api, collection_id) {

}
