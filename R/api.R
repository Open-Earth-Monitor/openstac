api <- NULL

#' @export
create_api <- function(id, title, description) {
  structure(
    list(
      id = id,
      title = title,
      description = description,
      stac_version = stac_version
    ),
    class = c(id, "stac_api"),
    env = new.env(hash = TRUE, parent = parent.frame())
  )
}

api_env <- function(api) {
  attr(api, "env")
}

api_attr <- function(api, name) {
  if (exists(name, envir = api_env(api)))
    get(name, envir = api_env(api), inherits = FALSE)
}

api_set_attr <- function(api, name, value) {
  assign(name, value, envir = api_env(api), inherits = FALSE)
  api
}

api_plumb <- function(api) {
  api_attr(api, "plumb")
}

api_host <- function(api) {
  get("host", envir = api_env(api), inherits = FALSE)
}

#' @export
run_api <- function(api, host = "http://127.0.0.1:8000") {
  stopifnot(grepl("^.+://", host))
  stopifnot(grepl(":[0-9]+$", host))
  api_set_attr(api, "host", host)
  api_file <- system.file("R/stac-api.R", package = "stacserver")
  plumb <- plumber::pr(api_file, envir = environment())
  #plumb <- plumber::pr_set_error(plumb, api_error_handler)
  api_set_attr(api, "plumb", plumb)
  plumber::pr_run(
    pr = plumb,
    host = get_host_name(host),
    port = get_host_port(host)
  )
}

#' @export
api_landing_page <- function(api) {
  doc <- list(
    type = "Catalog",
    id = api$id,
    title = api$title,
    description = api$description,
    stac_version = api$stac_version,
    conformsTo = conforms_to
  )
  # update links
  params <- link_params(
    host = api_host(api)
  )
  update_links("landing_page", doc, params)
}

#' @export
api_conformance <- function(api) {
  list(conformsTo = conforms_to)
}

#' @export
api_collections <- function(api) {
  db <- get_db(api)
  doc <- list(collections = db_collections(db))
  # update links
  params <- link_params(
    host = api_host(api)
  )
  update_links("collections", doc, params)
}

#' @export
api_collection <- function(api, collection_id) {
  db <- get_db(api)
  check_collection_in_db(db, collection_id)
  doc <- db_collection(db, collection_id)
  # update links
  params <- link_params(
    host = api_host(api),
    collection_id = collection_id
  )
  update_links("collection", doc, params)
}

#' @export
api_items <- function(api,
                      collection_id,
                      limit,
                      bbox,
                      datetime,
                      page) {
  db <- get_db(api)
  check_collection_in_db(db, collection_id)
  doc <- db_items(
    db = db,
    collection_id = collection_id,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    page = page
  )
  # update links
  params <- link_params(
    host = api_host(api),
    collection_id = collection_id,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    page = page
  )
  update_links("items", doc, params)
}

#' @export
api_item <- function(api, collection_id, item_id) {
  db <- get_db(api)
  check_collection_in_db(db, collection_id)
  check_item_in_db(db, collection_id, item_id)
  doc <- db_item(db, collection_id, item_id)
  # update links
  params <- link_params(
    host = api_host(api),
    collection_id = collection_id,
    item_id = item_id
  )
  update_links("item", doc, params)
}

#' @export
api_search <- function(api,
                       limit,
                       bbox,
                       datetime,
                       intersects,
                       ids,
                       collections,
                       page,
                       method) {
  db <- get_db(api)
  doc <- db_search(
    db = db,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    intersects = intersects,
    ids = ids,
    collections = collections,
    page = page
  )
  # update links
  params <- link_params(
    host = api_host(api),
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    intersects = intersects,
    ids = ids,
    collections = collections,
    page = page,
    method = method
  )
  update_links("search", doc, params)
}

api_error_handler <- function(req, res, e) {
  if (is.null(e$status)) e$status <- 500
  if (is.null(e$message)) e$message <- "Internal server error"
  res$status <- e$status
  list(code = e$status, message = paste("Error:", e$message))
}

#' @export
api_stop <- function(status, ...) {
  stop(errorCondition(paste0(...), code = status))
}

#' @export
api_stopifnot <- function(value, status, ...) {
  message <- paste0(...)
  if (!nzchar(message))
    message <- paste(deparse(substitute(value)), "is not TRUE")
  if (!value) api_stop(status, message)
}
