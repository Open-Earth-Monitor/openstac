#' @importFrom mongolite mongo

#' @export
new_db.mongodb <- function(driver, db, url, ...) {
  # driver checkers
  stopifnot(requireNamespace("mongolite"))
  data <- list(
    collections = mongolite::mongo(
      collection = "collections",
      db = db,
      url = url, ...
    ),
    items = mongolite::mongo(
      collection = "items",
      db = db,
      url = url, ...
    )
  )
  structure(data, class = driver[[1]])
}

#' @export
db_collections_id_exist.mongodb <- function(db, ids) {
  ids %in% names(db$collections)
}

#' @export
db_collections.mongodb <- function(db) {
  unname(db$collections)
}

#' @export
db_collection.mongodb <- function(db, collection_id) {
  mongo_collection(db, collection_id)
}

#' @export
db_items_id_exist.mongodb <- function(db, collection_id, ids) {
  items <- mongo_items(db, collection_id)
  ids %in% mongo_items_id(items)
}

#' @export
db_items.mongodb <- function(db, collection_id, limit, bbox, datetime, page) {
  items <- mongo_items(db, collection_id)
  # datetime filter...
  exact_date <- get_datetime_exact(datetime)
  start_date <- get_datetime_start(datetime)
  end_date <- get_datetime_end(datetime)
  # ...exact_date
  if (!is.null(exact_date)) {
    items <- mongo_filter_exact_date(items, exact_date)
  } else {
    # ...start_date
    if (!is.null(start_date))
      items <- mongo_filter_start_date(items, start_date)
    # ...end_date
    if (!is.null(end_date))
      items <- mongo_filter_end_date(items, end_date)
  }
  # spatial filter
  if (!is.null(bbox)) {
    items <- mongo_filter_spatial(items, bbox_as_polygon(bbox))
  }
  items$numberMatched <- length(items$features)
  # manage pagination
  mongo_paginate_items(items, limit, page)
}

#' @export
db_item.mongodb <- function(db, collection_id, item_id) {
  item <- mongo_items(db, collection_id)
  item <- mongo_filter_ids(item, item_id)
  item <- item$features[[1]]
  item$collection <- collection_id
  class(item) <- c("doc_item", "list")
  item
}

#' @export
db_search.mongodb <- function(db,
                            limit,
                            bbox,
                            datetime,
                            intersects,
                            ids,
                            collections,
                            page) {
  items <- mongo_search_items(features)
  items$numberMatched <- length(items$features)
  # manage pagination
  mongo_paginate_items(items, limit, page)
}

mongo_new_items <- function(features) {
  structure(list(
    type = "FeatureCollection",
    features = features
  ), class = c("doc_items", "list"))
}

mongo_collection <- function(db, collection_id) {
  doc <- db$collections[[collection_id]]
  class(doc) <- c("doc_collection", "list")
  doc
}

mongo_items <- function(db, collection_id) {
  doc <- db$items[[collection_id]]
  class(doc) <- c("doc_items", "list")
  doc
}

mongo_items_id <- function(items) {
  rstac::items_reap(items, "id")
}

mongo_items_datetime <- function(items) {
  as.Date(rstac::items_datetime(items))
}

mongo_filter_ids <- function(items, ids) {
  select <- which(mongo_items_id(items) %in% ids)
  items$features <- items$features[select]
  items
}

mongo_filter_exact_date <- function(items, exact_date) {
  select <- mongo_items_datetime(items) == as.Date(exact_date)
  items$features <- items$features[select]
  items
}

mongo_filter_start_date <- function(items, start_date) {
  select <- mongo_items_datetime(items) >= as.Date(start_date)
  items$features <- items$features[select]
  items
}

mongo_filter_end_date <- function(items, end_date) {
  select <- mongo_items_datetime(items) <= as.Date(end_date)
  items$features <- items$features[select]
  items
}

mongo_filter_spatial <- function(items, geom) {
  select <- rstac::items_intersects(items, geom)
  items$features <- items$features[select]
  items
}

mongo_paginate_items <- function(items, limit, page) {
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
