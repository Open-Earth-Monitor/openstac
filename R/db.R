drivers <- function() c("local")

set_db <- function(api, driver, ...) {
  stopifnot(is.character(driver))
  stopifnot(driver %in% drivers())
  class(driver) <- driver
  UseMethod("set_db", driver)
}

get_db <- function(api) {
  stopifnot(exists("db", envir = get_env(api)))
  get_attr(api, "db")
}

get_collections <- function(api) {
  db <- get_db(api)
  UseMethod("get_collections", db)
}

get_collection <- function(api, collection_id) {
  db <- get_db(api)
  UseMethod("get_collection", db)
}

get_items <- function(api, collection_id, limit, bbox, datetime) {
  db <- get_db(api)
  UseMethod("get_items", db)
}

get_item <- function(api, collection_id, item_id) {
  db <- get_db(api)
  UseMethod("get_item", db)
}

new_link <- function(rel, href, ...) {
  dots <- list(...)
  c(list(rel = rel, href = href), dots)
}

update_collection_links <- function(data, api) {
  data$links <- list(
    new_link(
      rel = "root",
      href = get_endpoint(api, "/")
    ),
    new_link(
      rel = "self",
      href = get_endpoint(
        api,
        paste("/collections", data$id, sep = "/")
      )
    ),
    new_link(
      rel = "item",
      href = get_endpoint(
        api,
        paste("/collections", data$id, "items", sep = "/")
      )
    )
  )
  data
}

update_collections_links <- function(data, api) {
  data$links <- list(
    new_link(
      rel = "root",
      href = get_endpoint(api, "/")
    ),
    new_link(
      rel = "self",
      href = get_endpoint(api, "/collections")
    )
  )
  data
}

update_item_links <- function(data, api, collection_id) {
  data$links <- list(
    new_link(
      rel = "root",
      href = get_endpoint(api, "/")
    ),
    new_link(
      rel = "self",
      href = get_endpoint(api, "/collections")
    ),
    new_link(
      rel = "collection",
      href = get_endpoint(api, "/collections", collection_id)
    )
  )
  data
}

update_items_links <- function(data, api, collection_id) {
  data$links <- list(
    new_link(
      rel = "root",
      href = get_endpoint(api, "/")
    ),
    new_link(
      rel = "self",
      href = get_endpoint(api, "/collections")
    ),
    new_link(
      rel = "collection",
      href = get_endpoint(api, "/collections", collection_id)
    )
  )
  data
}
