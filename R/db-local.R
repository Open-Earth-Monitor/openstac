#' @importFrom rstac items_filter

#' @export
new_db.local <- function(api, driver, file, ...) {
  stopifnot(file.exists(file))
  data <- readRDS(file)
  stopifnot(is.list(data))
  stopifnot(!ĩs.null(names(data)))
  structure(
    list(
      collections = data$collections,
      items = data$items
    ),
    class = as.character(driver)
  )
}

#' @export
get_collections.local <- function(db) {
  data <- lapply(db$collections, update_collection_links)
  data <- list(
    collections = unname(data)
  )
  update_collections_links(data)
}

#' @export
get_collection.local <- function(db, collection_id) {
  stopifnot(collection_id %in% names(db$collections))
  collection <- db$collections[[collection_id]]
  update_collection_links(collection)
}

bbox_as_polygon <- function(x) {
  coords <- matrix(
    unlist(x)[c(1, 3, 3, 1, 1, 2, 2, 4, 4, 2)],
    ncol = 2, byrow = FALSE
  )
  sf::st_sfc(structure(
    list(structure(c(coords), dim = dim(coords))),
    class = c("XY", "POLYGON", "sfg")
  ), crs = 4326)
}

get_items_id <- function(items) {
  if (is.null(items$features))
    rstac::items_reap(items, "id")
  ids <- names(items$features)
  ids[ids == ""] <- NA_character_
  ids
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

get_pages <- function(items, limit) {
  ceiling(length(items$features) / limit)
}

paginate_items <- function(items, limit, page) {
  if (is.null(limit)) limit <- 10
  if (is.null(page)) page <- 1
  pages <- get_pages(items, limit)
  if (pages > 0) {
    stopifnot(page <= pages)
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

#' @export
get_items.local <- function(db,
                            collection_id,
                            limit,
                            bbox,
                            exact_date,
                            start_date,
                            end_date,
                            page) {
  # prepare items
  stopifnot(collection_id %in% names(db$items))
  items <- db$items[[collection_id]]
  items$features <- unname(items$features)
  # datetime filter...
  # ...exact_date
  if (!is.null(exact_date)) {
    items <- filter_exact_date(items, exact_date)
  }
  # ...start_date
  if (!is.null(start_date)) {
    items <- filter_exact_date(items, start_date)
  }
  # ...end_date
  if (!is.null(end_date)) {
    items <- filter_exact_date(items, end_date)
  }
  # spatial filter
  if (!is.null(bbox)) {
    items <- filter_spatial(items, bbox_as_polygon(bbox))
  }
  items$numberMatched <- length(items$features)
  # manage pagination
  items <- paginate_items(items, limit, page)
  # update links
  update_get_items_links(
    items = items,
    collection_id = collection_id,
    limit = limit,
    bbox = bbox,
    exact_date = exact_date,
    start_date = start_date,
    end_date = end_date,
    page = page
  )
}

#' @export
get_item.local <- function(db, collection_id, item_id) {
  stopifnot(collection_id %in% names(db$items))
  items <- db$items[[collection_id]]
  stopifnot(item_id %in% names(items))
  item <- items[[item_id]]
  update_item_links(item, collection_id)
}

empty_items <- function() {
  list(
    type = "FeatureCollection",
    features = list(),
    numberMatched = 0,
    numberReturned = 0
  )
}

#' @export
search_items.local <- function(db,
                               limit,
                               bbox,
                               exact_date,
                               start_date,
                               end_date,
                               intersects,
                               ids,
                               collections,
                               page) {
  stopifnot(all(collections %in% names(db$items)))
  items <- empty_items()
  # merge items from all collections
  for (collection_id in collections) {
    new_items <- db$items[[collection_id]]
    if (!is.null(ids))
      new_items <- filter_ids(new_items, ids)
    # update each item links
    new_items <- update_each_item_links(new_items, collection_id)
    items$features <- c(items$features, unname(new_items$features))
  }
  # datetime filter...
  # ...exact_date
  if (!is.null(exact_date)) {
    items <- filter_exact_date(items, exact_date)
  }
  # ...start_date
  if (!is.null(start_date)) {
    items <- filter_exact_date(items, start_date)
  }
  # ...end_date
  if (!is.null(end_date)) {
    items <- filter_exact_date(items, end_date)
  }
  # spatial filter...
  # ...bbox
  if (!is.null(bbox)) {
    items <- filter_spatial(items, bbox_as_polygon(bbox))
  }
  # ...intersects
  if (!is.null(intersects)) {
    items <- filter_spatial(items, get_geom(intersects))
  }
  items$numberMatched <- length(items$features)
  # manage pagination
  if (is.null(limit)) limit <- 10
  if (is.null(page)) page <- 1
  pages <- ceiling(length(items$features) / limit)
  if (pages > 0) {
    stopifnot(page <= pages)
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
  # update links
  update_search_items_links(
    items = items,
    limit = limit,
    bbox = bbox,
    exact_date = exact_date,
    start_date = start_date,
    end_date = end_date,
    intersects = intersects,
    ids = ids,
    collections = collections,
    page = page
  )
}
