
new_link <- function(rel, href, ...) {
  dots <- list(...)
  c(list(rel = rel, href = href), dots)
}

update_catalog_links <- function(data) {
  data$links  <- list(
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
  data
}

update_collection_links <- function(data) {
  data$links <- list(
    new_link(
      rel = "root",
      href = get_endpoint("/")
    ),
    new_link(
      rel = "self",
      href = get_endpoint("/collections", data$id)
    ),
    new_link(
      rel = "item",
      href = get_endpoint("/collections", data$id, "items")
    )
  )
  data
}

update_collections_links <- function(data) {
  data$links <- list(
    new_link(
      rel = "root",
      href = get_endpoint("/")
    ),
    new_link(
      rel = "self",
      href = get_endpoint("/collections")
    )
  )
  data
}

update_item_links <- function(data, collection_id, item_id) {
  data$links <- list(
    new_link(
      rel = "root",
      href = get_endpoint("/")
    ),
    new_link(
      rel = "self",
      href = get_endpoint("/collections", collection_id, "items", item_id)
    ),
    new_link(
      rel = "collection",
      href = get_endpoint("/collections", collection_id)
    )
  )
  data
}

get_datetime <- function(start_date, end_date) {
  if (is.null(start_date) && is.null(end_date)) return(NULL)
  if (is.null(start_date)) start_date <- ".."
  if (is.null(end_date)) end_date <- ".."
  paste0(start_date, "/", end_date)
}

update_items_links <- function(data,
                               collection_id,
                               limit,
                               bbox,
                               start_date,
                               end_date,
                               page,
                               pages) {
  data$links <- list(
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
        datetime = get_datetime(start_date, end_date),
        page = page
      )
    ),
    new_link(
      rel = "collection",
      href = get_endpoint("/collections", collection_id)
    )
  )
  if (page > 1 && page >= pages)
    data$links <- c(
      data$links,
      new_link(
        rel = "prev",
        href = get_endpoint(
          "/collections",
          collection_id,
          "items",
          limit = limit,
          bbox = bbox,
          datetime = get_datetime(start_date, end_date),
          page = page - 1
        )
      )
    )
  if (page < pages)
    data$links <- c(
      data$links,
      new_link(
        rel = "next",
        href = get_endpoint(
          "/collections",
          collection_id,
          "items",
          limit = limit,
          bbox = bbox,
          datetime = get_datetime(start_date, end_date),
          page = page + 1
        )
      )
    )
  data
}
