set_db.local <- function(api, driver, file, ...) {
  stopifnot(file.exists(file))
  data <- readRDS(file)
  stopifnot(is.list(data))
  env <- list2env(data, parent = emptyenv(), hash = TRUE)
  db <- structure(
    c(list(collections = env), list(...)),
    class = as.character(driver)
  )
  set_attr(api, "db", db)
}

get_collections.local <- function(api) {
  db <- get_db(api)
  data <- as.list(db$collections, all.names = TRUE)
  data <- lapply(data, update_collection_links, api = api)
  data <- list(
    collections = unname(data),
    links = list(
      new_link(
        rel = "root",
        href = get_endpoint(api, "/")
      ),
      new_link(
        rel = "self",
        href = get_endpoint(api, "/collections")
      )
    )
  )
  data
}

get_collection.local <- function(api, collection_id) {
  db <- get_db(api)
  stopifnot(exists(collection_id, envir = db$collections))
  collection <- get(collection_id, envir = db$collections)
  update_collection_links(collection, api)
}
