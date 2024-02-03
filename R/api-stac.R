# A list of all conformance classes specified in a standard that the
# server conforms to.
stac_conforms_to <- c(
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/core",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/oas30",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/geojson",
  "https://api.stacspec.org/v1.0.0/core",
  "https://api.stacspec.org/v1.0.0/collections",
  "https://api.stacspec.org/v1.0.0/item-search",
  "https://api.stacspec.org/v1.0.0/ogcapi-features/"
)

stac_version <- "1.0.0"

#' @export
create_stac <- function(id, title, description, conforms_to = NULL, ...) {
  create_api(
    api_class = c("stac", "ogcapi"),
    title = title,
    description = description,
    conforms_to = c(stac_conforms_to, conforms_to),
    stac_version = stac_version,
    id = id, ...
  )
}

#' @export
api_landing_page.stac <- function(api, req, res, ...) {
  doc <- list(
    stac_version = api$stac_version,
    type = "Catalog",
    id = api$id,
    title = api$title, description = api$description,
    conformsTo = api$conforms_to
  )
  doc <- links_landing_page(doc, api, req, res)
  doc
}

#' @export
api_conformance.stac <- function(api, req, res, ...) {
  NextMethod("api_conformance", api)
}

#' @export
api_collections.stac <- function(api, req, res, ...) {
  NextMethod("api_collections", api)
}

#' @export
api_collection.stac <- function(api, req, res, collection_id, ...) {
  NextMethod("api_collection", api)
}

#' @export
api_items.stac <- function(api,
                           req,
                           res,
                           collection_id,
                           limit,
                           bbox,
                           datetime,
                           page, ...) {
  NextMethod("api_items", api)
}

#' @export
api_item.stac <- function(api, req, res, collection_id, item_id, ...) {
  NextMethod("api_item", api)
}

#' @export
api_search.stac <- function(api,
                            req,
                            res,
                            limit,
                            bbox,
                            datetime,
                            intersects,
                            ids,
                            collections,
                            page, ...) {
  db <- api_db(api)
  check_collection_in_db(db, collections)
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
  links_search(
    doc = doc,
    api = api,
    req = req,
    res = res,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    ids = ids,
    collections = collections,
    page = page
  )
}
