new_link <- function(rel, href, ...) {
  dots <- list(...)
  c(list(rel = rel, href = href), dots)
}

get_link <- function(req, res, ...) {
  dots <- c(...)
  segments <- unname(dots)
  params <- NULL
  if (!is.null(names(dots))) {
    segments <- unname(dots[names(dots) == ""])
    params <- dots[names(dots) != ""]
  }
  path <- paste0(segments, collapse = "/")
  href <- paste0(get_host(req), path)
  query <- paste(names(params), unname(params), sep = "=", collapse = "&")
  if (query != "") href <- paste0(href, "?", query)
  href
}

#' @export
add_link <- function(doc, rel, href, ...) {
  doc$links <- c(doc$links, list(new_link(rel, href, ...)))
  doc
}

links_landing_page <- function(doc, req, res, ...) {
  doc$links  <- list(
    new_link(
      rel = "self",
      href = get_link(req, res, "/"),
      type = "application/json"
    ),
    new_link(
      rel = "conformance",
      href = get_link(req, res, "/conformance"),
      type = "application/json"
    ),
    new_link(
      rel = "data",
      href = get_link(req, res, "/collections"),
      type = "application/json"
    ),
    new_link(
      rel = "search",
      href = get_link(req, res, "/search"),
      type = "application/json"
    ),
    new_link(
      rel = "service-doc",
      href = get_link(req, res, "/__docs__/"),
      type = "text/html",
      title = "the API documentation"
    ),
    new_link(
      rel = "service-spec",
      href = get_link(req, res, "/openapi.json"),
      type = "application/vnd.oai.openapi+json;version=3.0",
      title = "API conformance classes implemented by this server"
    )
  )
  doc
}

links_collection <- function(doc, req, res, ...) {
  doc$links <- list(
    new_link(
      rel = "root",
      href = get_link(req, res, "/"),
      type = "application/json"
    ),
    new_link(
      rel = "self",
      href = get_link(req, res, "/collections", doc$id),
      type = "application/json"
    ),
    new_link(
      rel = "item",
      href = get_link(req, res, "/collections", doc$id, "items"),
      type = "application/json"
    )
  )
  doc
}

links_collections <- function(doc, req, res, ...) {
  doc$collections <- lapply(doc$collections, function(collection) {
    links_collection(collection, req, res)
  })
  doc$links <- list(
    new_link(
      rel = "root",
      href = get_link(req, res, "/"),
      type = "application/json"
    ),
    new_link(
      rel = "self",
      href = get_link(req, res, "/collections"),
      type = "application/json"
    )
  )
  doc
}

links_item <- function(doc, req, res, collection_id, ...) {
  doc$links <- list(
    new_link(
      rel = "root",
      href = get_link(req, res, "/"),
      type = "application/json"
    ),
    new_link(
      rel = "self",
      href = get_link(req, res, "/collections", collection_id,
                          "items", doc$id),
      type = "application/json"
    ),
    new_link(
      rel = "collection",
      href = get_link(req, res, "/collections", collection_id),
      type = "application/json"
    )
  )
  doc
}

links_items <- function(doc,
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
    links_item(item, req, res, collection_id)
  })
  doc$links <- list(
    new_link(
      rel = "root",
      href = get_link(req, res, "/"),
      type = "application/json"
    ),
    new_link(
      rel = "self",
      href = get_link(
        req = req,
        res = res,
        "/collections",
        collection_id,
        "items",
        limit = limit,
        bbox = bbox,
        datetime = datetime_as_str(datetime),
        page = page
      ),
      type = "application/json"
    ),
    new_link(
      rel = "collection",
      href = get_link(req, res, "/collections", collection_id),
      type = "application/json"
    )
  )
  # add navigation links
  if (page > 1 && page <= pages)
    doc <- add_link(
      doc = doc,
      rel = "prev",
      href = get_link(
        req = req,
        res = res,
        "/collections",
        collection_id,
        "items",
        limit = limit,
        bbox = bbox,
        datetime = datetime_as_str(datetime),
        page = page - 1
      ),
      type = "application/json"
    )
  if (page < pages)
    doc <- add_link(
      doc = doc,
      rel = "next",
      href = get_link(
        req = req,
        res = res,
        "/collections",
        collection_id,
        "items",
        limit = limit,
        bbox = bbox,
        datetime = datetime_as_str(datetime),
        page = page + 1
      ),
      type = "application/json"
    )
  doc
}

links_search <- function(doc,
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
    links_item(item, req, res, item$collection)
  })
  doc$links <- list(
    new_link(
      rel = "root",
      href = get_link(req, res, "/"),
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
          req = req,
          res = res,
          "/search",
          limit = limit,
          bbox = bbox,
          datetime = datetime_as_str(datetime),
          ids = ids,
          collections = collections,
          page = page - 1
        ),
        type = "application/json"
      )
    if (page < pages)
      doc <- add_link(
        doc = doc,
        rel = "next",
        href = get_link(
          req = req,
          res = res,
          "/search",
          limit = limit,
          bbox = bbox,
          datetime = datetime_as_str(datetime),
          ids = ids,
          collections = collections,
          page = page + 1
        ),
        type = "application/json"
      )
  } else if (method == "POST") {
    doc$links <- list(
      new_link(
        rel = "root",
        href = get_link(req, res, "/"),
        type = "application/json"
      )
    )
    if (page > 1 && page <= pages)
      doc <- add_link(
        doc = doc,
        rel = "prev",
        href = get_link(req, res, "/search"),
        body = list(
          page = page - 1
        ),
        merge = TRUE,
        type = "application/json"
      )
    if (page < pages)
      doc <- add_link(
        doc = doc,
        rel = "next",
        href = get_link(req, res, "/search"),
        body = list(
          page = page + 1
        ),
        merge = TRUE,
        type = "application/json"
      )
  }
  doc
}
