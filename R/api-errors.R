check_limit <- function(limit) {
  api_stopifnot(
    value = !is.na(limit),
    code = 400,
    message = "limit is not an integer"
  )
  api_stopifnot(
    value = limit >= 1 && limit <= 10000,
    code = 400,
    message = "limit not between 1 and 10000"
  )
}

check_bbox <- function(bbox) {
  api_stopifnot(
    value = all(!is.na(bbox)),
    code = 400,
    message = "bbox coordinates are not numeric"
  )
  api_stopifnot(
    value = length(bbox) == 4,
    code = 400,
    message = "bbox does not have 4 numbers"
  )
}

check_time_stamp <- function(exact_date) {
  api_stopifnot(
    value = !is.na(extact_date),
    code = 400,
    message = "datetime is not a valid time stamp or time interval"
  )
}

check_datetime <- function(datetime) {
  api_stopifnot(
    value = length(datetime) == 1 || length(datetime) == 2,
    code = 400,
    message = "datetime is not a valid time stamp or time interval"
  )
  if (length(datetime) == 1) {
    check_time_stamp(as.Date(datetime))
  } else {
    if (datetime[[1]] != "..")
      check_time_stamp(as.Date(datetime[[1]]))
    if (datetime[[2]] != "..")
      check_time_stamp(as.Date(datetime[[2]]))
  }
}

check_page_param <- function(page, pages = NULL) {
  api_stopifnot(
    value = !is.na(page),
    code = 400,
    message = "page is not an integer"
  )
  api_stopifnot(
    value = page >= 1,
    code = 400,
    message = "page not greater than or equal to 1"
  )
  if (!is.null(pages))
    api_stopifnot(
      value = page >= 1,
      code = 400,
      message = paste0("page not less than or equal to ", pages)
    )
}

check_intersects_param <- function(intersects) {
  api_stopifnot(
    value = is_geom(intersects),
    code = 400,
    message = "intersects is not a valid geometry"
  )
}

check_collections_param <- function(collections) {
  api_stopifnot(
    value = length(collections) >= 1,
    code = 400,
    message = "at least one collection must be provided"
  )
}

check_collection_db <- function(db, collection_id) {
  api_stopifnot(
    value = all(db_collections_id_exist(db, collection_id)),
    code = 404,
    message = "collection not found on the server"
  )
}

check_item_db <- function(db, collection_id, item_id) {
  api_stopifnot(
    value = all(db_items_id_exist(db, collection_id, item_id)),
    code = 404,
    message = "item not found on the server"
  )
}
