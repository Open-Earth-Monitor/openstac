#' Database Management Functions
#'
#' This set of functions provides functionality for managing the
#' database associated with the API.
#'
#' \itemize{
#'
#' \item `set_db`: Creates a new database object based on the
#'   specified driver and sets it as the API database. It
#'   dispatches to the appropriate method for constructing the
#'   database object.
#'
#' \item `get_db`: Retrieves the database associated with the API.
#'
#' }
#'
#' @param api An object representing the API.
#'
#' @param driver The driver for the database (e.g., "local", "mongodb").
#'
#' @param ... Additional arguments to be passed to the database driver
#'   constructor.
#'
#' @return The API object with the database set.
#'
#' @name db_functions
NULL
drivers <- c("local", "mongodb")

#' @rdname db_functions
#' @export
set_db <- function(api, driver, ...) {
  db <- new_db(driver, ...)
  api_attr(api, "db") <- db
  api
}
#' @rdname db_functions
#' @export
get_db <- function(api) {
  api_attr(api, "db")
}
#' @keywords internal
new_db <- function(driver, ...) {
  stopifnot(is.character(driver))
  stopifnot(driver %in% drivers)
  class(driver) <- driver
  UseMethod("new_db", driver)
}
#' @keywords internal
db_collections_id <- function(db, ids = NULL) {
  UseMethod("db_collections_id", db)
}
#' @keywords internal
db_collections_id_exist <- function(db, ids) {
  UseMethod("db_collections_id_exist", db)
}
#' @keywords internal
db_collections <- function(db) {
  # TODO: implement pagination limit
  UseMethod("db_collections", db)
}
#' @keywords internal
db_collection <- function(db, collection_id) {
  UseMethod("db_collection", db)
}
#' @keywords internal
db_items_id_exist <- function(db, collection_id, ids) {
  UseMethod("db_items_id_exist", db)
}
#' @keywords internal
db_items <- function(db, collection_id, limit, bbox, datetime, page) {
  UseMethod("db_items", db)
}
#' @keywords internal
db_item <- function(db, collection_id, item_id) {
  UseMethod("db_item", db)
}
#' @keywords internal
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
