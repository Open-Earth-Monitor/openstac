# TODO: filter date is not working
# TODO
#' @export
new_db.mongodb <- function(driver, db, url, ..., batch_size = 1000) {
  # driver checkers
  stopifnot(requireNamespace("mongolite"))
  data <- list(
    collections = mongolite::mongo("collections", db, url, ...),
    items = mongolite::mongo("items", db, url, ...),
    config = list(batch_size = batch_size)
  )
  structure(data, class = driver[[1]])
}
#' @export
db_collections_id.mongodb <- function(db, ids = NULL) {
  mongo_collections_id(db, ids)
}
#' @export
db_collections_id_exist.mongodb <- function(db, ids) {
  ids %in% mongo_collections_id(db)
}
#' @export
db_collections.mongodb <- function(db) {
  # TODO: implement pagination limit
  mongo_collections(db)
}
#' @export
db_collection.mongodb <- function(db, collection_id) {
  mongo_collections(db, collection_id[[1]])[[1]]
}
#' @export
db_items_id_exist.mongodb <- function(db, collection_id, ids) {
  ids %in% mongo_items_id(db, collection_id, mongo_in("id", ids))
}
#' @export
db_items.mongodb <- function(db, collection_id, limit, bbox, datetime, page) {
  query <- NULL
  # spatial filter...
  if (!is.null(bbox)) {
    query <- mongo_and(query, mongo_intersects("geometry", bbox_as_geom(bbox)))
  }
  # datetime filter...
  if (!is.null(datetime)) {
    query <- mongo_and(query, mongo_filter_datetime(
      field = "properties.datetime",
      datetime = datetime
    ))
  }
  items <- mongo_items(
    db = db,
    collection_id = collection_id,
    query = query,
    limit = limit,
    page = page
  )
  pages <- get_pages(items, limit)
  if (pages > 0 && page > 1) {
    api_stopifnot(
      page <= pages,
      status = 400,
      "page not less than or equal to ", pages
    )
  }
  items
}
#' @export
db_item.mongodb <- function(db, collection_id, item_id) {
  items <- mongo_items(
    db = db,
    collection_id = collection_id[[1]],
    query = mongo_equal("id", item_id[[1]]),
    limit = 1,
    page = 1
  )
  item <- items$features[[1]]
  item$collection <- collection_id[[1]]
  as_item(item)
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
  query <- NULL
  if (!is.null(bbox)) {
    query <- mongo_and(query, mongo_intersects("geometry", bbox_as_geom(bbox)))
  }
  if (!is.null(datetime)) {
    query <- mongo_and(query, mongo_filter_datetime(
      field = "properties.datetime", datetime = datetime
    ))
  }
  if (!is.null(intersects)) {
    query <- mongo_and(query, mongo_intersects("geometry", intersects))
  }
  if (!is.null(ids)) {
    query <- mongo_and(query, mongo_in("id", ids))
  }
  items <- mongo_items(
    db = db,
    collection_id = collections,
    query = query,
    limit = limit,
    page = page
  )
  pages <- get_pages(items, limit)
  if (pages > 0 && page > 1) {
    api_stopifnot(
      page <= pages,
      status = 400,
      "page not less than or equal to ", pages
    )
  }
  items
}
#---- internal functions ----
#' @keywords internal
mongo_collections_id <- function(db, ids = NULL, query = NULL) {
  if (!is.null(ids)) {
    query <- mongo_and(query, mongo_in("id", ids))
  }
  query_json <- jsonlite::toJSON(query, null = "list", auto_unbox = TRUE)
  db$collections$distinct("id", query_json)
}
#' @keywords internal
mongo_collections <- function(db, collection_id = NULL, query = NULL) {
  if (!is.null(collection_id)) {
    query <- mongo_and(query, mongo_in("id", collection_id))
  }
  query_json <- jsonlite::toJSON(query, null = "list", auto_unbox = TRUE)
  cursor <- db$collections$iterate(query_json)
  batch <- cursor$batch(db$config$batch_size)
  collections <- list()
  while (!is.null(batch)) {
    collections <- c(collections, batch)
    batch <- possibly(cursor$batch(db$config$batch_size))
  }
  unname(collections)
}
#' @keywords internal
mongo_items_id <- function(db, collection_id, query = NULL) {
  query <- mongo_and(query, mongo_equal("collection", collection_id))
  query_json <- jsonlite::toJSON(query, auto_unbox = TRUE, null = "list")
  db$items$distinct("id", query_json)
}
#' @keywords internal
mongo_filter_datetime <- function(field, datetime) {
  query <- NULL
  exact_date <- datetime$exact
  start_date <- datetime$start
  end_date <- datetime$end
  # ...exact_date
  if (!is.null(exact_date)) {
    query <- mongo_filter_exact_date(field, exact_date)
  } else {
    # ...start_date
    if (!is.null(start_date)) {
      query <- mongo_filter_start_date(field, start_date)
    }
    # ...end_date
    if (!is.null(end_date)) {
      query <- mongo_and(query, mongo_filter_end_date(field, end_date))
    }
  }
  query
}
#' @keywords internal
mongo_filter_exact_date <- function(field, exact_date, query = NULL) {
  query <- mongo_and(query, mongo_equal(field, mongo_datetime(exact_date)))
  query
}
#' @keywords internal
mongo_filter_start_date <- function(field, start_date, query = NULL) {
  query <- mongo_and(query, mongo_gte(field, mongo_datetime(start_date)))
  query
}
#' @keywords internal
mongo_filter_end_date <- function(field, end_date, query = NULL) {
  query <- mongo_and(query, mongo_lt(field, mongo_datetime(end_date)))
  query
}
#' @keywords internal
mongo_items_matched <- function(db, query = NULL) {
  browser()
  query <- list(
    list(`$match` = query),
    list(`$group` = list(`_id` = 1, count = list(`$sum` = 1)))
  )
  query_json <- jsonlite::toJSON(query, auto_unbox = TRUE, null = "list")
  db$items$aggregate(query_json)$count
}
#' @keywords internal
mongo_items <- function(db,
                        collection_id = NULL,
                        query = NULL,
                        limit = 10,
                        page = 1) {
  if (!is.null(collection_id)) {
    query <- mongo_and(query, mongo_in("collection", collection_id))
  }
  query_json <- jsonlite::toJSON(query, auto_unbox = TRUE, null = "list")
  cursor <- db$items$iterate(query_json, skip = (page - 1) * limit)
  features <- cursor$batch(limit)
  create_items(
    features,
    numberMatched = mongo_items_matched(db, query),
    numberReturned = length(features)
  )
}

# ---- operators ----

mongo_in <- function(field, values) {
  query <- list(list("$in" = as.list(values)))
  names(query) <- field
  query
}
mongo_and <- function(expr1, expr2) {
  if (is.null(expr1)) expr1 <- list()
  utils::modifyList(expr1, expr2)
}
mongo_equal <- function(field, value) {
  query <- list(list("$eq" = value))
  names(query) <- field
  query
}
mongo_gte <- function(field, value) {
  query <- list(list("$gte" = value))
  names(query) <- field
  query
}
mongo_lt <- function(field, value) {
  query <- list(list("$lt" = value))
  names(query) <- field
  query
}
mongo_datetime <- function(datetime) {
  list("$date" = as_datetime(datetime))
}
mongo_geometry <- function(geom) {
  list("$geometry" = geom)
}
mongo_intersects <- function(field, geom) {
  query <- list(list("$geoIntersects" = mongo_geometry(geom)))
  names(query) <- field
  query
}
