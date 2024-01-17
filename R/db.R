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

new_link <- function(rel, href, ...) {
  dots <- list(...)
  c(list(rel = rel, href = href), dots)
}

update_collection_links <- function(collection, api) {
  collection$links <- list(
    new_link(
      rel = "root",
      href = get_endpoint(api, "/")
    ),
    new_link(
      rel = "self",
      href = get_endpoint(
        api,
        paste("/collections", collection$id, sep = "/")
      )
    ),
    new_link(
      rel = "item",
      href = get_endpoint(
        api,
        paste("/collections", collection$id, "items", sep = "/")
      )
    )
  )
  collection
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

new_collection.local <- function(api, collection) {
  data <- get_attr(api, "collections")
  if (is.null(data)) data <- list()
  data[[collection$id]] <- collection
  set_attr(api, "collections", data)
}

save_collections.local <- function(api) {
  connection <- get_db(api)
  file <- connection$file
  stopifnot(!is.null(file))
  data <- get_attr(api, "collections")
  stopifnot(!is.null(data))
  saveRDS(data, file)
}

load_collections.local <- function(api) {
  connection <- get_db(api)
  file <- connection$file
  stopifnot(!is.null(file))
  stopifnot(file.exists(file))
  data <- readRDS(file)
  set_attr(api, "collections", data)
}

get_collections.local <- function(api) {
  data <- get_attr(api, "collections")
  stopifnot(!is.null(data))
  data
}

get_collection.local <- function(api, collection_id) {
  data <- get_attr(api, "collections")
  stopifnot(!is.null(data))
  stopifnot(collection_id %in% names(data))
  data[[collection_id]]
}
