#' @importFrom rstac items_filter

#' @export
new_db.local <- function(api, driver, file, ...) {
  stopifnot(file.exists(file))
  data <- readRDS(file)
  stopifnot(is.list(data))
  stopifnot(!ĩs.null(names(data)))
  stopifnot("collections" %in% names(data))
  stopifnot("items" %in% names(data))
  stopifnot(all(names(data$collections) %in% names(data$items)))
  stopifnot(all(names(data$items) %in% names(data$collections)))
  structure(
    list(
      collections = data$collections,
      items = data$items
    ),
    class = as.character(driver),
    cache = new.env(hash = TRUE, parent = emptyenv())
  )
}

#' @export
db_collections_id.local <- function(db) {
  names(db$collections)
}

#' @export
db_collections.local <- function(db) {
  unname(db$collections)
}

#' @export
db_collection.local <- function(db, collection_id) {
  db$collections[[collection_id]]
}

#' @export
db_items_id.local <- function(db, collection_id) {
  items <- db$items[[collection_id]]
  ids <- names(items$features)
  ids
}

#' @export
db_items.local <- function(db,
                           collection_id,
                           items_id,
                           exact_date,
                           start_date,
                           end_date,
                           bbox,
                           intersects) {
  items <- db$items[[collection_id]]
  # id filter
  if (!is.null(items_id)) {
    items <- filter_ids(items, items_id)
  }
  # datetime filter...
  # ...exact_date
  if (!is.null(exact_date)) {
    items <- filter_exact_date(items, exact_date)
  } else {
    # ...start_date
    if (!is.null(start_date)) {
      items <- filter_exact_date(items, start_date)
    }
    # ...end_date
    if (!is.null(end_date)) {
      items <- filter_exact_date(items, end_date)
    }
  }
  # spatial filter...
  # ...bbox
  if (!is.null(bbox)) {
    items <- filter_spatial(items, bbox_as_polygon(bbox))
  } else if (!is.null(intersects)) {
    # ...intersects
    items <- filter_spatial(items, get_geom(intersects))
  }
  items$features <- unname(items$features)
  items
}



#' @export
db_item.local <- function(db, collection_id, item_id) {
  db$items[[collection_id]]
}


filter_ids <- function(items, ids) {
  select <- get_items_id(items) %in% ids
  select[is.na(select)] <- FALSE
  items$features <- items$features[select]
  items
}

filter_exact_date <- function(items, exact_date) {
  select <- as.Date(rstac::items_datetime(items)) == as.Date(exact_date)
  select[is.na(select)] <- FALSE
  items$features <- items$features[select]
  items
}

filter_start_date <- function(items, start_date) {
  select <- as.Date(rstac::items_datetime(items)) >= as.Date(start_date)
  select[is.na(select)] <- FALSE
  items$features <- items$features[select]
  items
}

filter_start_date <- function(items, end_date) {
  select <- as.Date(rstac::items_datetime(items)) <= as.Date(end_date)
  select[is.na(select)] <- FALSE
  items$features <- items$features[select]
  items
}

filter_spatial <- function(items, geom) {
  select <- rstac::items_intersects(items, geom)
  items$features <- items$features[select]
  items
}
