# A list of all conformance classes specified in a standard that the
# server conforms to.
stac_conforms_to <- c(
  "https://api.stacspec.org/v1.0.0/core",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/core",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/oas30",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/geojson"
)

stac_version <- "v1.0.0"

#' @export
create_stac <- function(title, description, conforms_to = NULL, ...) {
  create_api(
    api_class = c("stac", "ogcapi"),
    title = title,
    description = description,
    conforms_to = c(stac_conforms_to, conforms_to),
    stac_version = stac_version,
    type = "Catalog", ...
  )
}

#' @export
api_landing_page.stac <- function(api, req, res, ...) {
  NextMethod("api_landing_page", api)
}

#' @export
api_conformance.stac <- function(api, req, res, ...) {
  NextMethod("api_landing_page", api)
}

#' @export
api_collections.stac <- function(api, req, res, ...) {
  NextMethod("api_landing_page", api)
}

#' @export
api_collection.stac <- function(api, req, res, collection_id, ...) {
  NextMethod("api_landing_page", api)
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
  NextMethod("api_landing_page", api)
}

#' @export
api_item.stac <- function(api, req, res, collection_id, item_id, ...) {
  NextMethod("api_landing_page", api)
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
  db_collections_id_exist(db, collections)
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
