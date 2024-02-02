
#' @export
api_landing_page.stac <- function(api, ...) {
  list(title = api$title, description = api$description)
}

#' @export
api_conformance.stac <- function(api, ...) {
  list(conformsTo = api$conforms_to)
}

#' @export
api_collections.stac <- function(api, ...) {
  db <- api_db(api)
  list(collections = db_collections(db))
}

#' @export
api_collection.stac <- function(api, collection_id, ...) {
  db <- api_db(api)
  check_collection_in_db(db, collection_id)
  db_collection(db, collection_id)
}

#' @export
api_items.stac <- function(api,
                           collection_id,
                           limit,
                           bbox,
                           datetime,
                           page, ...) {
  db <- api_db(api)
  check_collection_in_db(db, collection_id)
  db_items(
    db = db,
    collection_id = collection_id,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    page = page
  )
}

#' @export
api_item.stac <- function(api, collection_id, item_id, ...) {
  db <- api_db(api)
  check_collection_in_db(db, collection_id)
  check_item_in_db(db, collection_id, item_id)
  db_item(db, collection_id, item_id)
}

#' @export
api_search.stac <- function(api,
                            limit,
                            bbox,
                            datetime,
                            intersects,
                            ids,
                            collections,
                            page, ...) {
  db <- api_db(api)
  db_collections_id_exist(db, collections)
  db_search(
    db = db,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    intersects = intersects,
    ids = ids,
    collections = collections,
    page = page
  )
}
