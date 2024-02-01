new_link <- function(rel, href, ...) {
  dots <- list(...)
  c(list(rel = rel, href = href), dots)
}

add_link <- function(links, rel, href, ...) {
  c(links, list(new_link(rel, href, ...)))
}

link_params <- function(host, ...) {
  list(host = host, ...)
}

update_links <- function(doc_type, doc, params) {
  class(doc_type) <- doc_type
  UseMethod("update_links", doc_type)
}

#' @export
update_links.landing_page <- function(doc_type, doc, params) {
  host <- params$host
  doc$links  <- list(
    new_link(
      rel = "self",
      href = get_endpoint(host, "/")
    ),
    new_link(
      rel = "conformance",
      href = get_endpoint(host, "/conformance")
    ),
    new_link(
      rel = "data",
      href = get_endpoint(host, "/collections")
    )
  )
  doc
}

#' @export
update_links.collection <- function(doc_type, doc, params) {
  host <- params$host
  doc$links <- list(
    new_link(
      rel = "root",
      href = get_endpoint(host, "/")
    ),
    new_link(
      rel = "self",
      href = get_endpoint(host, "/collections", doc$id)
    ),
    new_link(
      rel = "item",
      href = get_endpoint(host, "/collections", doc$id, "items")
    )
  )
  doc
}

#' @export
update_links.collections <- function(doc_type, doc, params) {
  host <- params$host
  # add collection links
  doc$collections <- lapply(doc$collections, function(collection) {
    update_links("collection", collection, params)
  })
  doc$links <- list(
    new_link(
      rel = "root",
      href = get_endpoint(host, "/")
    ),
    new_link(
      rel = "self",
      href = get_endpoint(host, "/collections")
    )
  )
  doc
}

#' @export
update_links.item <- function(doc_type, doc, params) {
  host <- params$host
  collection_id <- params$collection_id
  doc$links <- list(
    new_link(
      rel = "root",
      href = get_endpoint(host, "/")
    ),
    new_link(
      rel = "self",
      href = get_endpoint(host, "/collections", collection_id, "items", doc$id)
    ),
    new_link(
      rel = "collection",
      href = get_endpoint(host, "/collections", collection_id)
    )
  )
  doc
}

#' @export
update_links.items <- function(doc_type, doc, params) {
  host <- params$host
  collection_id <- params$collection_id
  limit <- params$limit
  bbox <- params$bbox
  exact_date <- params$exact_date
  start_date <- params$start_date
  end_date <- params$end_date
  page <- params$page
  pages <- get_pages(doc, limit)
  # update item links
  doc$features <- lapply(doc$features, function(item) {
    update_links("item", item, params)
  })
  doc$links <- list(
    new_link(
      rel = "root",
      href = get_endpoint(host, "/")
    ),
    new_link(
      rel = "self",
      href = get_endpoint(
        host = host,
        "/collections",
        collection_id,
        "items",
        limit = limit,
        bbox = bbox,
        datetime = datetime(start_date, end_date, exact_date),
        page = page
      )
    ),
    new_link(
      rel = "collection",
      href = get_endpoint(host, "/collections", collection_id)
    )
  )
  # add navigation links
  if (page > 1 && page <= pages)
    doc$links <- add_link(
      doc$links,
      rel = "prev",
      href = get_endpoint(
        host = host,
        "/collections",
        collection_id,
        "items",
        limit = limit,
        bbox = bbox,
        datetime = datetime(start_date, end_date, exact_date),
        page = page - 1
      )
    )
  if (page < pages)
    doc$links <- add_link(
      doc$links,
      rel = "next",
      href = get_endpoint(
        host = host,
        "/collections",
        collection_id,
        "items",
        limit = limit,
        bbox = bbox,
        datetime = datetime(start_date, end_date, exact_date),
        page = page + 1
      )
    )
  doc
}

#' @export
update_links.search <- function(doc_type, doc, params) {
  host <- params$host
  limit <- params$limit
  bbox <- params$bbox
  exact_date <- params$exact_date
  start_date <- params$start_date
  end_date <- params$end_date
  # no need for parameter: `intersects <- params$intersects`
  ids <- params$ids
  collections <- params$collections
  page <- params$page
  method <- params$method
  pages <- get_pages(doc, limit)
  # update item links
  doc$features <- lapply(doc$features, function(item) {
    params <- list(
      host = host,
      collection_id = item$collection
    )
    update_links("item", item, params)
  })
  doc$links <- list(
    new_link(
      rel = "root",
      href = get_endpoint(host, "/")
    )
  )
  # add navigation links
  if (method == "GET") {
    if (page > 1 && page <= pages)
      doc$links <- add_link(
        doc$links,
        rel = "prev",
        href = get_endpoint(
          api = api,
          "/search",
          limit = limit,
          bbox = bbox,
          datetime = datetime(start_date, end_date, exact_date),
          ids = ids,
          collections = collections,
          page = page - 1
        )
      )
    if (page < pages)
      doc$links <- add_link(
        doc$links,
        rel = "next",
        href = get_endpoint(
          api = api,
          "/search",
          limit = limit,
          bbox = bbox,
          datetime = datetime(start_date, end_date, exact_date),
          ids = ids,
          collections = collections,
          page = page + 1
        )
      )
  } else if (method == "POST") {
    doc$links <- list(
      new_link(
        rel = "root",
        href = get_endpoint(host, "/")
      )
    )
    if (page > 1 && page <= pages)
      doc$links <- add_link(
        doc$links,
        rel = "prev",
        href = get_endpoint(host, "/search"),
        body = list(
          page = page - 1
        ),
        merge = TRUE
      )
    if (page < pages)
      doc$links <- add_link(
        doc$links,
        rel = "next",
        href = get_endpoint(host, "/search"),
        body = list(
          page = page + 1
        ),
        merge = TRUE
      )
  }
  doc
}
