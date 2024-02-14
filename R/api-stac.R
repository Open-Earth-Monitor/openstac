stac_version <- "1.0.0"

#' @rdname api_handling
#' @export
create_stac <- function(id, title, description, conforms_to = NULL, ...) {
  create_api(
    api_class = c("stac", "ogcapi"),
    title = title,
    description = description,
    conforms_to = conforms_to,
    stac_version = stac_version,
    id = id, ...
  )
}
#' @rdname api_handling
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
#' @rdname api_handling
#' @export
api_conformance.stac <- function(api, req, res, ...) {
  NextMethod("api_conformance", api)
}
#' @rdname api_handling
#' @export
api_collections.stac <- function(api, req, res, ...) {
  NextMethod("api_collections", api)
}
#' @rdname api_handling
#' @export
api_collection.stac <- function(api, req, res, collection_id, ...) {
  NextMethod("api_collection", api)
}
#' @rdname api_handling
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
#' @rdname api_handling
#' @export
api_item.stac <- function(api, req, res, collection_id, item_id, ...) {
  NextMethod("api_item", api)
}
#' @rdname api_handling
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
  db <- get_db(api)
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
