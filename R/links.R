
new_link <- function(rel, href, ...) {
  dots <- list(...)
  c(list(rel = rel, href = href), dots)
}

update_links <- function(doc_type, doc, ...) {
  class(doc_type) <- doc_type
  UseMethod("update_links", doc_type)
}

#' @export
update_links.landing_page <- function(doc_type, doc, ...) {
  doc$links  <- list(
    new_link(
      rel = "self",
      href = get_endpoint("/")
    ),
    new_link(
      rel = "conformance",
      href = get_endpoint("/conformance")
    ),
    new_link(
      rel = "data",
      href = get_endpoint("/collections")
    )
  )
  doc
}

#' @export
update_links.collection <- function(doc_type, doc, ...) {
  doc$links <- list(
    new_link(
      rel = "root",
      href = get_endpoint("/")
    ),
    new_link(
      rel = "self",
      href = get_endpoint("/collections", doc$id)
    ),
    new_link(
      rel = "item",
      href = get_endpoint("/collections", doc$id, "items")
    )
  )
  doc
}

#' @export
update_links.collections <- function(doc_type, doc, ...) {
  doc$links <- list(
    new_link(
      rel = "root",
      href = get_endpoint("/")
    ),
    new_link(
      rel = "self",
      href = get_endpoint("/collections")
    )
  )
  doc
}

#' @export
update_links.item <- function(doc_type, doc, ..., collection_id) {
  doc$links <- list(
    new_link(
      rel = "root",
      href = get_endpoint("/")
    ),
    new_link(
      rel = "self",
      href = get_endpoint("/collections", collection_id, "items", doc$id)
    ),
    new_link(
      rel = "collection",
      href = get_endpoint("/collections", collection_id)
    )
  )
  doc
}

update_each_item_links <- function(items, collection_id) {
  items$features <- unname(lapply(
    items$features, update_item_links, collection_id = collection_id
  ))
  items
}

get_datetime <- function(start_date = NULL,
                         end_date = NULL,
                         exact_date = NULL) {
  if (is.null(start_date) && is.null(end_date))
    return(as.character(exact_date))
  if (is.null(start_date)) start_date <- ".."
  if (is.null(end_date)) end_date <- ".."
  paste0(start_date, "/", end_date)
}

update_get_items_links <- function(items,
                                   collection_id,
                                   limit,
                                   bbox,
                                   exact_date,
                                   start_date,
                                   end_date,
                                   page) {
  # update each item links
  items <- update_each_item_links(items, collection_id)
  # update items links
  items$links <- list(
    new_link(
      rel = "root",
      href = get_endpoint("/")
    ),
    new_link(
      rel = "self",
      href = get_endpoint(
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
      href = get_endpoint("/collections", collection_id)
    )
  )
  pages <- get_pages(items, limit)
  if (page > 1 && page >= pages)
    items$links <- c(
      items$links,
      new_link(
        rel = "prev",
        href = get_endpoint(
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
    items$links <- c(
      items$links,
      new_link(
        rel = "next",
        href = get_endpoint(
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
  items
}

update_search_items_links <- function(items,
                                      limit,
                                      bbox,
                                      exact_date,
                                      start_date,
                                      end_date,
                                      intersects,
                                      ids,
                                      collections,
                                      page) {
  # update items links
  items$links <- list(
    new_link(
      rel = "root",
      href = get_endpoint("/")
    ),
    new_link(
      rel = "self",
      href = get_endpoint(
        "/search",
        limit = limit,
        bbox = bbox,
        datetime = get_datetime(start_date, end_date, exact_date),
        intersects = intersects,
        ids = ids,
        collections = collections,
        page = page
      )
    )
  )
  pages <- get_pages(items, limit)
  if (page > 1 && page >= pages)
    items$links <- c(
      items$links,
      new_link(
        rel = "prev",
        href = get_endpoint(
          "/search",
          limit = limit,
          bbox = bbox,
          datetime = get_datetime(start_date, end_date, exact_date),
          intersects = intersects,
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
          "/search",
          limit = limit,
          bbox = bbox,
          datetime = get_datetime(start_date, end_date, exact_date),
          intersects = intersects,
          ids = ids,
          collections = collections,
          page = page + 1
        )
      )
    )
  items
}
