# A list of all conformance classes specified in a standard that the
# server conforms to.
ogcapi_conforms_to <- c(
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/core",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/oas30",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/geojson"
)

#' @export
create_ogcapi <- function(title, description, conforms_to = NULL, ...) {
  create_api(
    api_class = "ogcapi",
    title = title,
    description = description,
    conforms_to = c(ogcapi_conforms_to, conforms_to), ...
  )
}

#' @export
api_landing_page.ogcapi <- function(api, req, res, ...) {
  list(title = api$title, description = api$description) |>
    links_landing_page(req, res)
}

#' @export
api_conformance.ogcapi <- function(api, req, res, ...) {
  list(conformsTo = api$conforms_to)
}

#' @export
api_collections.ogcapi <- function(api, req, res, ...) {
  db <- api_db(api)
  list(collections = db_collections(db)) |>
    links_collections(req, res)
}

#' @export
api_collection.ogcapi <- function(api, req, res, collection_id, ...) {
  db <- api_db(api)
  check_collection_in_db(db, collection_id)
  db_collection(db, collection_id) |>
    links_collection(req, res)
}

#' @export
api_items.ogcapi <- function(api,
                             req,
                             res,
                             collection_id,
                             limit,
                             bbox,
                             datetime,
                             page, ...) {
  db <- api_db(api)
  check_collection_in_db(db, collection_id)
  doc <- db_items(
    db = db,
    collection_id = collection_id,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    page = page
  )
  links_items(
    doc = doc,
    req = req,
    res = res,
    collection_id = collection_id,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    page = page
  )
}

#' @export
api_item.ogcapi <- function(api, req, res, collection_id, item_id, ...) {
  db <- api_db(api)
  check_collection_in_db(db, collection_id)
  check_item_in_db(db, collection_id, item_id)
  db_item(db, collection_id, item_id) |>
    links_item(req, res, collection_id)
}
