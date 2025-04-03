#' @rdname api_handling
#' @export
api_landing_page.stac <- function(api, req, res, ...) {
  doc <- NextMethod("api_landing_page", api)
  host <- get_host(api, req)
  doc <- c(list(
    type = "Catalog"
  ), doc)
  if (api$has("stac_version")) {
    doc <- c(list(
      stac_version = api$get("stac_version")
    ), doc)
  }
  if (api$has("id")) {
    doc <- c(list(
      id = api$get("id")
    ), doc)
  }
  if (api$has("conforms_to")) {
    doc <- c(list(
      conformsTo = api$conforms_to()
    ), doc)
  }
  doc <- update_link(
    doc = doc,
    rel = "search",
    href = make_url(host, "/search"),
    type = "application/geo+json",
    title = "STAC search",
    method = "GET"
  )
  doc <- update_link(
    doc = doc,
    rel = "search",
    href = make_url(host, "/search"),
    type = "application/geo+json",
    title = "STAC search",
    method = "POST"
  )
  doc
}
#' @rdname api_handling
#' @export
api_conformance.stac <- function(api, req, res, ...) {
  doc <- NextMethod("api_conformance", api)
  doc
}
#' @rdname api_handling
#' @export
api_collections.stac <- function(api, req, res, ...) {
  doc <- NextMethod("api_collections", api)
  doc
}
#' @rdname api_handling
#' @export
api_collection.stac <- function(api, req, res, collection_id, ...) {
  doc <- NextMethod("api_collection", api)
  doc
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
  doc <- NextMethod("api_items", api)
  doc
}
#' @rdname api_handling
#' @export
api_item.stac <- function(api, req, res, collection_id, item_id, ...) {
  doc <- NextMethod("api_item", api)
  doc
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
  host <- get_host(api, req)
  method <- get_method(req)
  # check parameters
  if (!is.null(limit)) {
    limit <- parse_int(limit[[1]])
    check_limit(limit, min = 1, max = 10000)
  }
  if (missing(bbox)) bbox <- NULL
  if (missing(intersects)) intersects <- NULL
  api_stopifnot(
    is.null(bbox) || is.null(intersects),
    status = 400,
    "only one of either intersects or bbox may be provided"
  )
  if (!is.null(bbox)) {
    bbox <- parse_dbl(bbox)
    check_bbox(bbox)
  }
  if (missing(datetime)) datetime <- NULL
  if (!is.null(datetime)) {
    datetime <- parse_datetime(datetime[[1]])
  }
  if (!is.null(intersects)) {
    api_stopifnot(
      method == "POST",
      status = 405,
      "the request method is not supported"
    )
    intersects <- parse_geojson(intersects)
    check_intersects(intersects)
  }
  if (missing(ids)) ids <- NULL
  if (!is.null(ids)) ids <- parse_str(ids)
  api_stopifnot(
    !missing(collections),
    status = 400,
    "collections parameter must be provided"
  )
  if (!is.null(collections)) {
    collections <- parse_str(collections)
    check_collections(collections)
  }
  if (!is.null(page)) {
    page <- parse_int(page[[1]])
    check_page(page)
  }
  # do search
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
  doc <- map_features(doc, \(item) {
    item <- link_root(item, api, req)
    item <- link_self(item, api, req, "application/geo+json")
    item <- update_link(
      doc = item,
      rel = "collection",
      href = make_url(host, "/collections", item$collection),
      type = "application/json"
    )
    item
  })
  doc <- link_root(doc, api, req)
  doc <- link_self(doc, api, req, "application/geo+json")
  # add navigation links
  if (method == "GET") {
    doc <- links_navigation(
      doc = doc,
      api = api,
      req = req,
      "/search",
      bbox = deparse_array(bbox),
      datetime = deparse_datetime(datetime),
      ids = ids,
      collections = collections,
      limit = limit,
      page = page,
      type = "application/geo+json"
    )
  } else if (method == "POST") {
    doc <- links_navigagion_post(
      doc = doc,
      api = api,
      req = req,
      "/search",
      limit = limit,
      page = page,
      type = "application/geo+json",
      merge = TRUE
    )
  }
  doc
}
