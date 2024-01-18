#' @importFrom rstac items_filter

set_db.local <- function(api, driver, file, ...) {
  stopifnot(file.exists(file))
  data <- readRDS(file)
  stopifnot(is.list(data))
  stopifnot(!ĩs.null(names(data)))
  db <- structure(
    list(
      collections = data$collections,
      items = data$items
    ),
    class = as.character(driver)
  )
  set_attr(api, "db", db)
}

get_collections.local <- function(api) {
  db <- get_db(api)
  data <- lapply(db$collections, update_collection_links, api = api)
  data <- list(
    collections = unname(data)
  )
  update_collections_links(data, api)
}

get_collection.local <- function(api, collection_id) {
  db <- get_db(api)
  stopifnot(collection_id %in% names(db$collections))
  collection <- db$collections[[collection_id]]
  update_collection_links(collection, api)
}

item_after <- function(date) {
  \(item) as.Date(item$datetime) >= date
}

item_before <- function(date) {
  \(item) as.Date(item$datetime) <= date
}

# TODO: resume from here
get_geometry <- function(item) {
  item$geometry$coordinates
}

# TODO: resume from here
item_intersects <- function(bbox) {
  xmin <- bbox[[1]]
  ymin <- bbox[[2]]
  xmax <- bbox[[3]]
  ymax <- bbox[[4]]
  \(item) item
}

get_items.local <- function(api, collection_id, limit, bbox, datetime) {
  db <- get_db(api)
  stopifnot(collection_id %in% names(db$items))
  items <- db$items[[collection_id]]
  # prepare limit parameter
  if (is.null(limit) || !is.numeric(limit)) limit <- 10
  if (limit > 10000) limit <- 10000
  if (limit > length(items$features)) limit <- length(items$features)
  # filter datetime
  if (!is.null(datetime)) {
    stopifnot(is.character(datetime))
    datetime <- strsplit(datetime, "/")[[1]]
    if (datetime[[1]] != "..") {
      start_date <- as.Date(datetime[[1]])
      stopifnot(!is.na(start_date))
      items <- rstac::items_filter(
        items = items,
        filter_fn = item_after(start_date)
      )
    }
    if (length(datetime) > 1 && datetime[[2]] != "..") {
      end_date <- as.Date(datetime[[2]])
      stopifnot(!is.na(end_date))
      items <- rstac::items_filter(
        items = items,
        filter_fn = item_before(end_date)
      )
    }
  }
  # filter spatial
  if (!is.null(bbox)) {
    stopifnot(is.numeric(bbox))
    stopifnot(length(bbox) == 4)
    # ...do spatial filtering
    # TODO: resume from here
  }
  # update links and return
  items$features <- items$features[seq_len(limit)]
  items$features <- lapply(items$features, update_item_links,
                           api = api, collection_id = collection_id)
  update_items_links(items, api, collection_id)
}

get_item.local <- function(api, collection_id, item_id) {
  db <- get_db(api)
  stopifnot(collection_id %in% names(db$items))
  items <- db$items[[collection_id]]
  stopifnot(item_id %in% names(items))
  item <- items[[item_id]]
  update_item_links(item, api, collection_id)
}
