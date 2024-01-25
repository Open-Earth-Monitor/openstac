api <- NULL

create_api <- function(id, title, description) {
  structure(
    list(
      id = id,
      title = title,
      description = description,
      stac_version = stac_version
    ),
    class = c(id, "stac_api"),
    env = new.env(hash = TRUE, parent = parent.frame())
  )
}

get_env <- function(api) {
  attr(api, "env")
}

get_attr <- function(api, name) {
  if (exists(name, envir = get_env(api)))
    get(name, envir = get_env(api), inherits = FALSE)
}

set_attr <- function(api, name, value) {
  assign(name, value, envir = get_env(api), inherits = FALSE)
  api
}

get_plumb <- function(api) {
  get_attr(api, "plumb")
}

set_plumb <- function(api, plumb) {
  set_attr(api, "plumb", plumb)
}

get_scheme <- function(api) {
  get("scheme", envir = get_env(api), inherits = FALSE)
}

get_host <- function(api) {
  get("host", envir = get_env(api), inherits = FALSE)
}

get_port <- function(api) {
  get("port", envir = get_env(api), inherits = FALSE)
}

start_api <- function(api, envir) {
  api_file <- system.file("R/stac-api.R", package = "stacserver")
  plumb <- plumber::pr(api_file, envir = envir)
  set_plumb(api, plumb)
  plumber::pr_run(plumb, host = get_host(api), port = get_port(api))
}

run_api <- function(api, host = "http://127.0.0.1", port = 8000) {
  stopifnot(grepl("^.+://", host))
  set_attr(api, "scheme", gsub("://.*$", "", host))
  set_attr(api, "host", gsub("^.+://", "", host))
  set_attr(api, "port", port)
  start_api(api, envir = environment())
}

api_landing_page <- function() {
  doc <- list(
    type = "Catalog",
    id = api$id,
    title = api$title,
    description = api$description,
    stac_version = api$stac_version,
    conformsTo = conforms_to
  )
  update_links("landing_page", doc)
}

api_conformance <- function() {
  doc <- list(conformsTo = conforms_to)
  doc
}

api_collections <- function() {
  db <- get_db(api)
  doc <- list(collections = db_collections(db))
  doc <- update_links("collections", doc)
  doc
}

api_collection <- function(collection_id) {
  db <- get_db(api)
  stopifnot(collection_id %in% db_collections_id(db))
  doc <- db_collection(db, collection_id)
  doc <- update_links("collection", doc)
  doc
}

#' @export
get_items.local <- function(collection_id,
                            limit,
                            bbox,
                            exact_date,
                            start_date,
                            end_date,
                            page) {
  db <- get_db(api)
  stopifnot(collection_id %in% db_collections(db))
  items <- db_item_list(db, collection_id)
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
  query_params <- list(
    collection_id = collection_id,
    limit = limit,
    bbox = bbox,
    exact_date = exact_date,
    start_date = start_date,
    end_date = end_date,
    page = page
  )
  items <- update_links("items", items, query_params)
  # update_get_items_links(
  #   items = items,
  #   collection_id = collection_id,
  #   limit = limit,
  #   bbox = bbox,
  #   exact_date = exact_date,
  #   start_date = start_date,
  #   end_date = end_date,
  #   page = page
  # )
  items
}

#' @export
get_item.local <- function(db, collection_id, item_id) {
  stopifnot(collection_id %in% names(db$items))
  items <- db$items[[collection_id]]
  stopifnot(item_id %in% names(items))
  item <- items[[item_id]]
  update_item_links(item, collection_id)
}

#' @export
api_search_items <- function(limit,
                             bbox,
                             exact_date,
                             start_date,
                             end_date,
                             intersects,
                             ids,
                             collections,
                             page) {
  db <- get_db(api)
  stopifnot(all(collections %in% names(db$items)))
  names(collections) <- unname(collections)
  collections <- lapply(collections, function(collection_id) {
    db_items(
      db = db,
      collection_id = collection_id,
      items_id = ids,
      exact_date = exact_date,
      start_date = start_date,
      end_date = end_date,
      bbox = bbox,
      intersects = intersects
    )
  })
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

empty_items <- function() {
  list(
    type = "FeatureCollection",
    features = list(),
    numberMatched = 0,
    numberReturned = 0
  )
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

# TODO: implement error handling for the API
api_error <- function(res, code, message) {
  res$status <- code

}
