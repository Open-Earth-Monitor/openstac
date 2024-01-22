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

#' @export
get_items.local <- function(db,
                            collection_id,
                            ids,
                            limit,
                            geometry,
                            exact_date,
                            start_date,
                            end_date,
                            page) {
  stopifnot(collection_id %in% names(db$items))
  items <- db$items[[collection_id]]
  # id filter
  if (!is.null(ids)) {
    select <- rstac::items_reap(items, "id") %in% ids
    select[is.na(select)] <- FALSE
    items$features <- items$features[select]
  }
  # datetime filter...
  # ...exact_date
  if (!is.null(exact_date)) {
    select <- rstac::items_datetime(items) == exact_date
    select[is.na(select)] <- FALSE
    items$features <- items$features[select]
  }
  # ...start_date
  if (!is.null(start_date)) {
    select <- rstac::items_datetime(items) >= start_date
    select[is.na(select)] <- FALSE
    items$features <- items$features[select]
  }
  # ...end_date
  if (!is.null(end_date)) {
    select <- rstac::items_datetime(items) <= end_date
    select[is.na(select)] <- FALSE
    items$features <- items$features[select]
  }
  # spatial filter
  if (!is.null(geometry)) {
    select <- rstac::items_intersects(items, geometry)
    items$features <- items$features[select]
  }
  items$numberMatched <- length(items$features)
  # manage pagination
  if (is.null(limit)) limit <- 10
  if (is.null(page)) page <- 1
  pages <- ceiling(length(items$features) / limit)
  if (pages > 0) {
    stopifnot(page <= pages)
    # select page items
    if (page == pages) {
      select <- seq((page - 1) * limit + 1, length(items$features))
    } else {
      select <- seq((page - 1) * limit + 1, page * limit)
    }
    items$features <- items$features[select]
  }
  items$numberReturned <- length(items$features)
  # update links
  items$features <- lapply(items$features, update_item_links,
                           collection_id = collection_id)
  update_items_links(items, collection_id, limit, geometry, start_date,
                     end_date, page, pages)
}

#' @export
get_item.local <- function(db, collection_id, item_id) {
  stopifnot(collection_id %in% names(db$items))
  items <- db$items[[collection_id]]
  stopifnot(item_id %in% names(items))
  item <- items[[item_id]]
  update_item_links(item, collection_id, item_id)
}

#' @export
search_items <- function(db,
                         limit = NULL,
                         bbox = NULL,
                         datetime = NULL,
                         intersects = NULL,
                         ids = NULL,
                         collections = NULL,
                         page = NULL) {
  stopifnot(all(collections %in% names(db$items)))
  for (collection_id in collections) {

  }
}
