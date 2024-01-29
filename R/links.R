new_link <- function(rel, href, ...) {
  dots <- list(...)
  c(list(rel = rel, href = href), dots)
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
  # update items links
  pages <- get_pages(doc, limit)
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
        datetime = get_datetime(start_date, end_date, exact_date),
        page = page
      )
    ),
    new_link(
      rel = "collection",
      href = get_endpoint(host, "/collections", collection_id)
    )
  )
  if (page > 1 && page >= pages)
    doc$links <- c(
      doc$links,
      new_link(
        rel = "prev",
        href = get_endpoint(
          host = host,
          "/collections",
          collection_id,
          "items",
          limit = limit,
          bbox = bbox,
          datetime = get_datetime(start_date, end_date, exact_date),
          page = page - 1
        )
      )
    )
  if (page < pages)
    doc$links <- c(
      doc$links,
      new_link(
        rel = "next",
        href = get_endpoint(
          host = host,
          "/collections",
          collection_id,
          "items",
          limit = limit,
          bbox = bbox,
          datetime = get_datetime(start_date, end_date, exact_date),
          page = page + 1
        )
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
  # update items links
  pages <- get_pages(items, limit)
  items$links <- list(
    new_link(
      rel = "root",
      href = get_endpoint(host, "/")
    )
  )
  if (method == "GET") {
    if (page > 1 && page >= pages)
      items$links <- c(
        items$links,
        new_link(
          rel = "prev",
          href = get_endpoint(
            api = api,
            "/search",
            limit = limit,
            bbox = bbox,
            datetime = get_datetime(start_date, end_date, exact_date),
            ids = ids,
            collections = collections,
            page = page - 1
          )
        )
      )
    if (page < pages)
      items$links <- c(
        items$links,
        new_link(
          rel = "next",
          href = get_endpoint(
            api = api,
            "/search",
            limit = limit,
            bbox = bbox,
            datetime = get_datetime(start_date, end_date, exact_date),
            ids = ids,
            collections = collections,
            page = page + 1
          )
        )
      )
  } else if (method == "POST") {
    items$links <- list(
      new_link(
        rel = "root",
        href = get_endpoint(host, "/")
      )
    )
    if (page > 1 && page >= pages)
      items$links <- c(
        items$links,
        new_link(
          rel = "prev",
          href = get_endpoint(host, "/search"),
          body = list(
            page = page - 1
          ),
          merge = TRUE
        )
      )
    if (page < pages)
      items$links <- c(
        items$links,
        new_link(
          rel = "next",
          href = get_endpoint(host, "/search"),
          body = list(
            page = page + 1
          ),
          merge = TRUE
        )
      )
  }
  items
}
