#' @rdname api_handling
#' @export
api_landing_page.oafeat <- function(api, req, res, ...) {
  host <- get_host(api, req)
  doc <- list(title = api$title(), description = api$description())
  doc <- link_root(doc, api, req)
  doc <- link_self(doc, api, req, "application/json")
  doc <- link_spec(doc, api, req)
  doc <- link_docs(doc, api, req)
  doc <- update_link(
    doc = doc,
    rel = "conformance",
    href = make_url(host, "/conformance"),
    type = "application/json"
  )
  doc <- update_link(
    doc = doc,
    rel = "data",
    href = make_url(host, "/collections"),
    type = "application/json"
  )
  db <- get_db(api)
  doc$links <- c(doc$links, lapply(db_collections(db), \(col) {
    update_link(
      doc = col,
      rel = "child",
      href = make_url(host, "/collections", escape_url(col$id)),
      type = "application/json",
      title = col$title
    )
  }))
  doc
}
#' @rdname api_handling
#' @export
api_conformance.oafeat <- function(api, req, res, ...) {
  doc <- list(conformsTo = api$conforms_to())
  doc
}
#' @rdname api_handling
#' @export
api_collections.oafeat <- function(api, req, res, ...) {
  # TODO: implement pagination limit
  host <- get_host(api, req)
  db <- get_db(api)
  doc <- create_collections(db_collections(db))
  doc <- map_collections(doc, \(col) {
    col <- link_root(col, api, req)
    col <- update_link(
      doc = col,
      rel = "self",
      href = make_url(host, "/collections", escape_url(col$id)),
      type = "application/json"
    )
    col <- update_link(
      doc = col,
      rel = "parent",
      href = make_url(host, "/"),
      type = "application/json"
    )
    col <- update_link(
      doc = col,
      rel = "item",
      href = make_url(host, "/collections", escape_url(col$id), "items"),
      type = "application/geo+json"
    )
    col
  })
  doc <- link_root(doc, api, req)
  doc <- link_self(doc, api, req, "application/json")
  doc
}
#' @rdname api_handling
#' @export
api_collection.oafeat <- function(api, req, res, collection_id, ...) {
  host <- get_host(api, req)
  db <- get_db(api)
  check_collection_in_db(db, collection_id)
  doc <- db_collection(db, collection_id)
  doc <- link_root(doc, api, req)
  doc <- link_self(doc, api, req, "application/json")
  doc <- update_link(
    doc = doc,
    rel = "parent",
    href = make_url(host, "/"), # TODO: how to get a parent collection/catalog other than /?
    type = "application/geo+json"
  )
  doc <- update_link(
    doc = doc,
    rel = "item",
    href = make_url(host, "/collections", escape_url(collection_id), "items"),
    type = "application/geo+json"
  )
  doc
}
#' @rdname api_handling
#' @export
api_items.oafeat <- function(api,
                             req,
                             res,
                             collection_id,
                             limit,
                             bbox,
                             datetime,
                             page, ...) {
  # defaults
  if (is.null(limit)) {
    limit <- 10
  }
  if (is.null(page)) {
    page <- 1
  }
  # check parameters
  if (!is.integer(limit)) {
    limit <- parse_int(limit[[1]])
    check_limit(limit, min = 1, max = 10000)
  }
  if (!is.null(bbox)) {
    bbox <- parse_dbl(bbox)
    check_bbox(bbox)
  }
  if (!is.null(datetime)) {
    datetime <- parse_datetime(datetime[[1]])
  }
  if (!is.integer(page)) {
    page <- parse_int(page[[1]])
    check_page(page)
  }
  # do items
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
  doc <- link_root(doc, api, req)
  doc <- link_self(doc, api, req, "application/geo+json")
  host <- get_host(api, req)
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
  doc <- update_link(
    doc = doc,
    rel = "collection",
    href = make_url(host, "/collections", collection_id),
    type = "application/json"
  )
  doc <- links_navigation(
    doc = doc,
    api = api,
    req = req,
    "/collections",
    collection_id,
    "items",
    bbox = deparse_array(bbox),
    datetime = deparse_datetime(datetime),
    limit = limit,
    page = page,
    type = "application/geo+json"
  )
  doc
}
#' @rdname api_handling
#' @export
api_item.oafeat <- function(api, req, res, collection_id, item_id, ...) {
  host <- get_host(api, req)
  db <- get_db(api)
  check_collection_in_db(db, collection_id)
  check_item_in_db(db, collection_id, item_id)
  doc <- db_item(db, collection_id, item_id)
  doc <- link_root(doc, api, req)
  doc <- link_self(doc, api, req, "application/geo+json")
  doc <- update_link(
    doc = doc,
    rel = "collection",
    href = make_url(host, "/collections", collection_id),
    type = "application/json"
  )
  doc
}
