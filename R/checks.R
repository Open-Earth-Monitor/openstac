#' @export
check_limit <- function(limit, min, max) {
  api_stopifnot(
    !is.na(limit),
    status = 400,
    "limit is not an integer"
  )
  api_stopifnot(
    limit >= min && limit <= max,
    status = 400,
    "limit not between ", min, " and ", max
  )
}

#' @export
check_bbox <- function(bbox) {
  api_stopifnot(
    all(!is.na(bbox)),
    status = 400,
    "bbox coordinates are not numeric"
  )
  api_stopifnot(
    length(bbox) == 4,
    status = 400,
    "bbox does not have 4 numbers"
  )
}

check_rfc3339 <- function(exact_date) {
  api_stopifnot(
    !is.na(as.Date(extact_date)),
    status = 400,
    "datetime is not a valid time stamp or time interval"
  )
}

#' @export
check_datetime <- function(datetime) {
  api_stopifnot(
    length(datetime) == 1 || length(datetime) == 2,
    status = 400,
    "datetime is not a valid time stamp or time interval"
  )
  if (length(datetime) == 1) {
    check_rfc3339(datetime)
  } else {
    if (datetime[[1]] != "..")
      check_rfc3339(datetime[[1]])
    if (datetime[[2]] != "..")
      check_rfc3339(datetime[[2]])
  }
}

#' @export
check_page <- function(page) {
  api_stopifnot(
    !is.na(page),
    status = 400,
    "page is not an integer"
  )
  api_stopifnot(
    page >= 1,
    status = 400,
    "page not greater than or equal to 1"
  )
}

#' @export
check_intersects <- function(intersects) {
  api_stopifnot(
    is_geom(intersects),
    status = 400,
    "intersects is not a valid geometry"
  )
}

#' @export
check_collections <- function(collections) {
  api_stopifnot(
    length(collections) >= 1,
    status = 400,
    "at least one collection must be provided"
  )
}

#' @export
check_collection_in_db <- function(db, collection_id) {
  api_stopifnot(
    all(db_collections_id_exist(db, collection_id)),
    status = 404,
    "collection not found on the server"
  )
}

#' @export
check_item_in_db <- function(db, collection_id, item_id) {
  api_stopifnot(
    all(db_items_id_exist(db, collection_id, item_id)),
    status = 404,
    "item not found on the server"
  )
}
