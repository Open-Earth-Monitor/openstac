new_link <- function(rel, href, ...) {
  dots <- list(...)
  not_null <- !vapply(dots, is.null, logical(1), USE.NAMES = FALSE)
  c(list(rel = rel, href = href), dots[not_null])
}

get_link <- function(host, ...) {
  dots <- c(...)
  segments <- unname(dots)
  params <- NULL
  if (!is.null(names(dots))) {
    segments <- unname(dots[names(dots) == ""])
    params <- dots[names(dots) != ""]
  }
  path <- paste0(segments, collapse = "/")
  href <- paste0(host, path)
  query <- paste(names(params), unname(params), sep = "=", collapse = "&")
  if (query != "") href <- paste0(href, "?", query)
  href
}

#' @export
add_link <- function(doc, rel, href, ...) {
  doc$links <- c(doc$links, list(new_link(rel, href, ...)))
  doc
}

links_landing_page <- function(doc, api, req, res, ...) {
  host <- get_host(req)
  doc$links  <- list(
    new_link(
      rel = "root",
      href = get_link(host, "/"),
      type = "application/json"
    ),
    new_link(
      rel = "self",
      href = get_link(host, "/"),
      type = "application/json"
    ),
    new_link(
      rel = "conformance",
      href = get_link(host, "/conformance"),
      type = "application/json"
    ),
    new_link(
      rel = "data",
      href = get_link(host, "/collections"),
      type = "application/json"
    ),
    new_link(
      rel = "search",
      href = get_link(host, "/search"),
      type = "application/geo+json",
      title = "STAC search",
      method = "GET"
    ),
    new_link(
      rel = "search",
      href = get_link(host, "/search"),
      type = "application/geo+json",
      title = "STAC search",
      method = "POST"
    ),
    new_link(
      rel = "service-doc",
      href = get_link(host, "/__docs__/"),
      type = "text/html",
      title = "the API documentation"
    ),
    new_link(
      rel = "service-spec",
      href = get_link(host, "/openapi.json"),
      type = "application/vnd.oai.openapi+json;version=3.0",
      title = "API conformance classes implemented by this server"
    )
  )
  db <- get_db(api)
  doc$links <- c(
    doc$links,
    lapply(db_collections(db), function(doc) {
      new_link(
        rel = "child",
        href = get_link(host, "/collections", doc$id),
        type = "application/json",
        title = doc$title
      )
    })
  )
  doc
}

links_collection <- function(doc, api, req, res, ...) {
  host <- get_host(req)
  doc$links <- list(
    new_link(
      rel = "root",
      href = get_link(host, "/"),
      type = "application/json"
    ),
    new_link(
      rel = "self",
      href = get_link(host, "/collections", doc$id),
      type = "application/json"
    ),
    new_link(
      rel = "item",
      href = get_link(host, "/collections", doc$id, "items"),
      type = "application/geo+json"
    )
  )
  doc
}

links_collections <- function(doc, api, req, res, ...) {
  doc$collections <- lapply(doc$collections, function(collection) {
    links_collection(collection, api, req, res)
  })
  host <- get_host(req)
  doc$links <- list(
    new_link(
      rel = "root",
      href = get_link(host, "/"),
      type = "application/json"
    ),
    new_link(
      rel = "self",
      href = get_link(host, "/collections"),
      type = "application/json"
    )
  )
  doc
}

links_item <- function(doc, api, req, res, collection_id, ...) {
  host <- get_host(req)
  doc$links <- list(
    new_link(
      rel = "root",
      href = get_link(host, "/"),
      type = "application/json"
    ),
    new_link(
      rel = "self",
      href = get_link(host, "/collections", collection_id,
                          "items", doc$id),
      type = "application/geo+json"
    ),
    new_link(
      rel = "collection",
      href = get_link(host, "/collections", collection_id),
      type = "application/json"
    )
  )
  doc
}

links_items <- function(doc,
                        api,
                        req,
                        res,
                        collection_id,
                        limit,
                        bbox,
                        datetime,
                        page, ...) {
  pages <- get_pages(doc, limit)
  # update item links
  doc$features <- lapply(doc$features, function(item) {
    links_item(item, api, req, res, collection_id)
  })
  host <- get_host(req)
  doc$links <- list(
    new_link(
      rel = "root",
      href = get_link(host, "/"),
      type = "application/json"
    ),
    new_link(
      rel = "self",
      href = get_link(
        host = host,
        "/collections",
        collection_id,
        "items",
        limit = limit,
        bbox = bbox,
        datetime = datetime_as_str(datetime),
        page = page
      ),
      type = "application/geo+json"
    ),
    new_link(
      rel = "collection",
      href = get_link(host, "/collections", collection_id),
      type = "application/json"
    )
  )
  # add navigation links
  if (page > 1 && page <= pages)
    doc <- add_link(
      doc = doc,
      rel = "prev",
      href = get_link(
        host = host,
        "/collections",
        collection_id,
        "items",
        limit = limit,
        bbox = bbox,
        datetime = datetime_as_str(datetime),
        page = page - 1
      ),
      type = "application/geo+json"
    )
  if (page < pages)
    doc <- add_link(
      doc = doc,
      rel = "next",
      href = get_link(
        host = host,
        "/collections",
        collection_id,
        "items",
        limit = limit,
        bbox = bbox,
        datetime = datetime_as_str(datetime),
        page = page + 1
      ),
      type = "application/geo+json"
    )
  doc
}

links_search <- function(doc,
                         api,
                         req,
                         res,
                         limit,
                         bbox,
                         datetime,
                         ids,
                         collections,
                         page, ...) {
  pages <- get_pages(doc, limit)
  # update item links
  doc$features <- lapply(doc$features, function(item) {
    links_item(item, api, req, res, item$collection)
  })
  host <- get_host(req)
  doc$links <- list(
    new_link(
      rel = "root",
      href = get_link(host, "/"),
      type = "application/json"
    )
  )
  method <- get_method(req)
  # add navigation links
  if (method == "GET") {
    if (page > 1 && page <= pages)
      doc <- add_link(
        doc = doc,
        rel = "prev",
        href = get_link(
          host = host,
          "/search",
          limit = limit,
          bbox = bbox,
          datetime = datetime_as_str(datetime),
          ids = ids,
          collections = collections,
          page = page - 1
        ),
        type = "application/geo+json"
      )
    if (page < pages)
      doc <- add_link(
        doc = doc,
        rel = "next",
        href = get_link(
          host = host,
          "/search",
          limit = limit,
          bbox = bbox,
          datetime = datetime_as_str(datetime),
          ids = ids,
          collections = collections,
          page = page + 1
        ),
        type = "application/geo+json"
      )
  } else if (method == "POST") {
    doc$links <- list(
      new_link(
        rel = "root",
        href = get_link(host, "/"),
        type = "application/json"
      )
    )
    if (page > 1 && page <= pages)
      doc <- add_link(
        doc = doc,
        rel = "prev",
        href = get_link(host, "/search"),
        body = list(
          page = page - 1
        ),
        merge = TRUE,
        type = "application/geo+json"
      )
    if (page < pages)
      doc <- add_link(
        doc = doc,
        rel = "next",
        href = get_link(host, "/search"),
        body = list(
          page = page + 1
        ),
        merge = TRUE,
        type = "application/geo+json"
      )
  }
  doc
}
