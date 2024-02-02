new_link <- function(rel, href, ...) {
  dots <- list(...)
  c(list(rel = rel, href = href), dots)
}

add_link <- function(links, rel, href, ...) {
  c(links, list(new_link(rel, href, ...)))
}

#' @export
doc_links_landing_page <- function(doc, host, ...) {
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
    ),
    new_link(
      rel = "search",
      href = get_endpoint(host, "/search")
    )
  )
  doc
}

#' @export
doc_links_collection <- function(doc, host, ...) {
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
doc_links_collections <- function(doc, host, ...) {
  # add collection links
  doc$collections <- lapply(doc$collections, function(collection) {
    doc_links_collection(collection, host)
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
doc_links_item <- function(doc, host, collection_id, ...) {
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
doc_links_items <- function(doc,
                            host,
                            collection_id,
                            limit,
                            bbox,
                            datetime,
                            page, ...) {
  pages <- get_pages(doc, limit)
  # update item links
  doc$features <- lapply(doc$features, function(item) {
    doc_links_item(item, host, collection_id)
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
        datetime = datetime_as_str(datetime),
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
        datetime = datetime_as_str(datetime),
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
        datetime = datetime_as_str(datetime),
        page = page + 1
      )
    )
  doc
}

#' @export
doc_links_search <- function(doc,
                             host,
                             limit,
                             bbox,
                             datetime,
                             ids,
                             collections,
                             page,
                             method, ...) {
  pages <- get_pages(doc, limit)
  # update item links
  doc$features <- lapply(doc$features, function(item) {
    doc_links_item(item, host, item$collection)
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
          host = host,
          "/search",
          limit = limit,
          bbox = bbox,
          datetime = datetime_as_str(datetime),
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
          host = host,
          "/search",
          limit = limit,
          bbox = bbox,
          datetime = datetime_as_str(datetime),
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
