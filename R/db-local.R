#' @export
new_db.local <- function(driver, file, ...) {
  # driver checkers
  stopifnot(file.exists(file))
  data <- readRDS(file)
  stopifnot(is.list(data))
  stopifnot(!is.null(names(data)))
  stopifnot("collections" %in% names(data))
  if ("items" %in% names(data)) {
    stopifnot(all(names(data$collections) %in% names(data$items)))
    stopifnot(all(names(data$items) %in% names(data$collections)))
  }
  structure(data, class = driver[[1]])
}
#' @export
db_collections_id.local <- function(db, ids = NULL) {
  local_collections_id(db, ids)
}
#' @export
db_collections_id_exist.local <- function(db, ids) {
  ids %in% local_collections_id(db)
}
#' @export
db_collections.local <- function(db) {
  # TODO: implement pagination limit
  local_collections(db)
}
#' @export
db_collection.local <- function(db, collection_id) {
  local_collections(db, collection_id[[1]])[[1]]
}
#' @export
db_items_id_exist.local <- function(db, collection_id, ids) {
  ids %in% local_items_id(db$items[[collection_id]])
}
#' @export
db_items.local <- function(db, collection_id, limit, bbox, datetime, page) {
  items <- local_items(db, collection_id)
  # spatial filter...
  if (!is.null(bbox)) {
    items <- local_filter_spatial(items, bbox_as_sfg(bbox))
  }
  # datetime filter...
  if (!is.null(datetime)) {
    items <- local_filter_datetime(items, datetime)
  }
  # manage pagination
  local_paginate_items(items, limit, page)
}
#' @export
db_item.local <- function(db, collection_id, item_id) {
  items <- local_items(db, collection_id, item_id[[1]])
  items$features[[1]]
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
    if (!is.null(ids)) {
      items <- local_filter_id(items, ids)
    }
    # spatial filter...
    # ...bbox
    if (!is.null(bbox)) {
      items <- local_filter_spatial(items, bbox_as_sfg(bbox))
    } else if (!is.null(intersects)) {
      # ...intersects
      items <- local_filter_spatial(items, geom_as_sfg(intersects))
    }
    # datetime filter...
    if (!is.null(datetime)) {
      items <- local_filter_datetime(items, datetime)
    }
    features <- c(features, items$features)
  }
  items <- create_items(features)
  # manage pagination
  local_paginate_items(items, limit, page)
}

#---- internal functions ----
#' @keywords internal
local_collections_id <- function(db, ids = NULL) {
  col_id <- names(db$collections)
  if (!is.null(ids)) {
    col_id <- col_id[col_id %in% ids]
  }
  col_id
}
#' @keywords internal
local_collections <- function(db, collection_id = NULL) {
  collections <- db$collections
  if (!is.null(collection_id)) {
    collection_id <- local_collections_id(db, collection_id)
    collections <- db$collections[collection_id]
  }
  unname(collections)
}
#' @keywords internal
local_items_id <- function(items) {
  vapply(items$features, \(item) item$id, character(1))
}
#' @keywords internal
local_items_datetime <- function(items) {
  vapply(items$features, \(item) item$properties$datetime, character(1))
}
#' @keywords internal
local_filter_spatial <- function(items, geom) {
  if (length(items$features) == 0) {
    return(items)
  }
  items_geom <- sf::st_sfc(
    lapply(items$features, \(item) {
      api_stopifnot(!is.null(item$geometry), 500)
      api_stopifnot(item$geometry$type == "Polygon", 500)
      sf::st_polygon(
        lapply(item$geometry$coordinates, \(ring) {
          matrix(unlist(ring), ncol = 2, byrow = TRUE)
        })
      )
    }),
    check_ring_dir = TRUE
  )
  select <- apply(sf::st_intersects(items_geom, geom), 1, any) > 0
  items$features <- items$features[select]
  items
}
#' @keywords internal
local_filter_datetime <- function(items, datetime) {
  exact_date <- datetime$exact
  start_date <- datetime$start
  end_date <- datetime$end
  # ...exact_date
  if (!is.null(exact_date)) {
    items <- local_filter_exact_date(items, exact_date)
  } else {
    # ...start_date
    if (!is.null(start_date)) {
      items <- local_filter_start_date(items, start_date)
    }
    # ...end_date
    if (!is.null(end_date)) {
      items <- local_filter_end_date(items, end_date)
    }
  }
  items
}
#' @keywords internal
local_filter_exact_date <- function(items, exact_date) {
  if (length(items$features) == 0) {
    return(items)
  }
  select <- local_items_datetime(items) == as.Date(exact_date)
  items$features <- items$features[select]
  items
}
#' @keywords internal
local_filter_start_date <- function(items, start_date) {
  if (length(items$features) == 0) {
    return(items)
  }
  select <- local_items_datetime(items) >= as.Date(start_date)
  items$features <- items$features[select]
  items
}
#' @keywords internal
local_filter_end_date <- function(items, end_date) {
  if (length(items$features) == 0) {
    return(items)
  }
  select <- local_items_datetime(items) < as.Date(end_date)
  items$features <- items$features[select]
  items
}
#' @keywords internal
local_paginate_items <- function(items, limit, page) {
  items$numberMatched <- length(items$features)
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
#' @keywords internal
local_items <- function(db, collection_id, items_id = NULL) {
  collection_id <- collection_id[[1]]
  items <- db$items[[collection_id]]
  items <- map_features(items, \(item) {
    item$collection <- collection_id
    item
  })
  local_filter_id(items, items_id)
}
#' @keywords internal
local_filter_id <- function(items, items_id = NULL) {
  if (is.null(items_id)) {
    return(items)
  }
  ids <- local_items_id(items)
  items$features <- items$features[ids %in% items_id]
  items
}
