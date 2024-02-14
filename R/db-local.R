#' @export
new_db.local <- function(driver, file, ...) {
  # driver checkers
  stopifnot(requireNamespace("rstac"))
  stopifnot(file.exists(file))
  data <- readRDS(file)
  stopifnot(is.list(data))
  stopifnot(!is.null(names(data)))
  stopifnot("collections" %in% names(data))
  stopifnot("items" %in% names(data))
  stopifnot(all(names(data$collections) %in% names(data$items)))
  stopifnot(all(names(data$items) %in% names(data$collections)))
  structure(data, class = driver[[1]])
}

#' @export
db_collections_id_exist.local <- function(db, ids) {
  ids %in% names(db$collections)
}

#' @export
db_collections.local <- function(db) {
  unname(db$collections)
}

#' @export
db_collection.local <- function(db, collection_id) {
  local_collection(db, collection_id)
}

#' @export
db_items_id_exist.local <- function(db, collection_id, ids) {
  items <- local_items(db, collection_id)
  ids %in% local_items_id(items)
}

#' @export
db_items.local <- function(db, collection_id, limit, bbox, datetime, page) {
  items <- local_items(db, collection_id)
  # datetime filter...
  exact_date <- get_datetime_exact(datetime)
  start_date <- get_datetime_start(datetime)
  end_date <- get_datetime_end(datetime)
  # ...exact_date
  if (!is.null(exact_date)) {
    items <- local_filter_exact_date(items, exact_date)
  } else {
    # ...start_date
    if (!is.null(start_date))
      items <- local_filter_start_date(items, start_date)
    # ...end_date
    if (!is.null(end_date))
      items <- local_filter_end_date(items, end_date)
  }
  # spatial filter
  if (!is.null(bbox)) {
    items <- local_filter_spatial(items, bbox_as_polygon(bbox))
  }
  items$numberMatched <- length(items$features)
  # manage pagination
  local_paginate_items(items, limit, page)
}

#' @export
db_item.local <- function(db, collection_id, item_id) {
  item <- local_items(db, collection_id)
  item <- local_filter_ids(item, item_id)
  item <- item$features[[1]]
  item$collection <- collection_id
  class(item) <- c("doc_item", "list")
  item
}

#' @export
db_search.local <- function(db,
                            limit,
                            bbox,
                            datetime,
                            intersects,
                            ids,
                            collections,
                            page) {
  features <- list()
  for (collection_id in collections) {
    items <- local_items(db, collection_id)
    # id filter
    if (!is.null(ids))
      items <- local_filter_ids(items, ids)
    # datetime filter...
    exact_date <- get_datetime_exact(datetime)
    start_date <- get_datetime_start(datetime)
    end_date <- get_datetime_end(datetime)
    # ...exact_date
    if (!is.null(exact_date)) {
      items <- local_filter_exact_date(items, exact_date)
    } else {
      # ...start_date
      if (!is.null(start_date))
        items <- local_filter_start_date(items, start_date)
      # ...end_date
      if (!is.null(end_date))
        items <- local_filter_end_date(items, end_date)
    }
    # spatial filter...
    # ...bbox
    if (!is.null(bbox)) {
      items <- local_filter_spatial(items, bbox_as_polygon(bbox))
    } else if (!is.null(intersects)) {
      # ...intersects
      items <- local_filter_spatial(items, get_geom(intersects))
    }
    # make sure to have the collection_id for each item
    items$features <- lapply(items$features, function(item) {
      item$collection <- collection_id
      class(item) <- c("doc_item", "list")
      item
    })
    features <- c(features, items$features)
  }
  items <- local_new_items(features)
  items$numberMatched <- length(items$features)
  # manage pagination
  local_paginate_items(items, limit, page)
}

local_new_items <- function(features) {
  structure(list(
    type = "FeatureCollection",
    features = features
  ), class = c("doc_items", "list"))
}

local_collection <- function(db, collection_id) {
  doc <- db$collections[[collection_id]]
  class(doc) <- c("doc_collection", "list")
  doc
}

local_items <- function(db, collection_id) {
  doc <- db$items[[collection_id]]
  class(doc) <- c("doc_items", "list")
  doc
}

local_items_id <- function(items) {
  rstac::items_reap(items, "id")
}

local_items_datetime <- function(items) {
  as.Date(rstac::items_datetime(items))
}

local_filter_ids <- function(items, ids) {
  select <- which(local_items_id(items) %in% ids)
  items$features <- items$features[select]
  items
}

local_filter_exact_date <- function(items, exact_date) {
  select <- local_items_datetime(items) == as.Date(exact_date)
  items$features <- items$features[select]
  items
}

local_filter_start_date <- function(items, start_date) {
  select <- local_items_datetime(items) >= as.Date(start_date)
  items$features <- items$features[select]
  items
}

local_filter_end_date <- function(items, end_date) {
  select <- local_items_datetime(items) <= as.Date(end_date)
  items$features <- items$features[select]
  items
}

local_filter_spatial <- function(items, geom) {
  select <- rstac::items_intersects(items, geom)
  items$features <- items$features[select]
  items
}

local_paginate_items <- function(items, limit, page) {
  if (is.null(limit)) limit <- 10
  if (is.null(page)) page <- 1
  pages <- get_pages(items, limit)
  if (pages > 0) {
    api_stopifnot(
      page <= pages,
      status = 400,
      "page not less than or equal to ", pages
    )
    # select page items
    index_from <- (page - 1) * limit + 1
    index_to <- if (page == pages) {
      length(items$features)
    } else {
      page * limit
    }
    select <- seq(index_from, index_to)
    items$features <- items$features[select]
  }
  items$numberReturned <- length(items$features)
  items
}
