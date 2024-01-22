#* @apiTitle STAC API
#* @apiDescription R STAC API server.
#* @apiVersion 1.0.0
stac_version <- "1.0.0"

# A list of all conformance classes specified in a standard that the
# server conforms to.
# TODO: update dynamically using extensions
conforms_to <- c(
  "https://api.stacspec.org/v1.0.0/core",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/core",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/oas30",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/geojson"
)

#* Landing page
#* @get /
#* @serializer unboxedJSON
function() {
  get_landing_page()
}

#* Conformance endpoint
#* @get /conformance
#* @serializer unboxedJSON
function() {
  list(
    conformsTo = conforms_to
  )
}

#* Collections endpoint
#* @get /collections
#* @serializer unboxedJSON
function() {
  db <- get_db(api)
  get_collections(db)
}

#* Collection endpoint
#* @get /collections/<collection_id>
#* @param collection_id:str The ID of the collection
#* @serializer unboxedJSON
function(collection_id) {
  db <- get_db(api)
  get_collection(db, collection_id)
}

#* Items endpoint
#* @get /collections/<collection_id>/items
#* @param collection_id:str The ID of the collection
#* @param limit:int Maximum number of features to return
#* @param bbox:[int] Bounding box (minx,miny,maxx,maxy)
#* @param datetime:str Datetime filter
#* @param page:int Pagination parameter
#* @serializer unboxedJSON
function(collection_id,
         limit = NULL,
         bbox = NULL,
         datetime = NULL,
         page = NULL) {
  db <- get_db(api)
  if (!is.null(limit)) {
    limit <- as.integer(limit)
    stopifnot(!is.na(limit))
    stopifnot(limit >= 1)
    stopifnot(limit <= 10000)
  }
  if (!is.null(bbox)) {
    bbox <- as.numeric(strsplit(bbox, ",")[[1]])
    stopifnot(length(bbox) == 4)
    stopifnot(all(!is.na(bbox)))
    bbox <- bbox_as_polygon(bbox)
  }
  if (!is.null(datetime)) {
    datetime <- strsplit(datetime, "/")[[1]]
    stopifnot(length(datetime) >= 1)
    stopifnot(length(datetime) <= 2)
    exact_date <- NULL
    start_date <- NULL
    end_date <- NULL
    if (length(datetime) == 1) {
      exact_date <- as.Date(datetime)
      stopifnot(!is.na(extact_date))
    } else {
      start_date <- datetime[[1]]
      if (start_date != "..") {
        start_date <- as.Date(start_date)
        stopifnot(!is.na(start_date))
      }
      end_date <- datetime[[2]]
      if (end_date != "..") {
        end_date <- as.Date(end_date)
        stopifnot(!is.na(end_date))
      }
    }
  }
  if (!is.null(page)) {
    page <- as.integer(page)
    stopifnot(!is.na(page))
    stopifnot(page >= 1)
  }
  get_items(
    db = db,
    collection_id = collection_id,
    ids = NULL,
    limit = limit,
    geometry = bbox,
    exact_date = exact_date,
    start_date = start_date,
    end_date = end_date,
    page = page
  )
}

#* Item endpoint
#* @get /collections/<collection_id>/items/<item_id>
#* @param collection_id:str The ID of the collection
#* @param item_id:str The ID of the item
#* @serializer unboxedJSON
function(collection_id, item_id) {
  db <- get_db(api)
  get_item(db, collection_id, item_id)
}

#* Search endpoint
#* @get /search
#* @post /search
#* @param limit:int Maximum number of features to return
#* @param bbox:[int] Bounding box (minx,miny,maxx,maxy)
#* @param datetime:str Datetime filter
#* @param intersects:list GeoJSON geometry to do spatial search
#* @param ids:[str] Array of items ID to return
#* @param collections:[str] Array of collection ID
#* @serializer unboxedJSON
function(limit = NULL,
         bbox = NULL,
         datetime = NULL,
         intersects = NULL,
         ids = NULL,
         collections = NULL,
         page = NULL) {
  db <- get_db(api)
  if (!is.null(limit)) {
    limit <- as.integer(limit)
    stopifnot(!is.na(limit))
    stopifnot(limit >= 1)
    stopifnot(limit <= 10000)
  }
  stopifnot(is.null(bbox) || is.null(intersects))
  if (!is.null(bbox)) {
    bbox <- as.numeric(strsplit(bbox, ",")[[1]])
    stopifnot(length(bbox) == 4)
    stopifnot(all(!is.na(bbox)))
    bbox <- bbox_as_polygon(bbox)
  }
  if (!is.null(datetime)) {
    datetime <- strsplit(datetime, "/")[[1]]
    stopifnot(length(datetime) >= 1)
    stopifnot(length(datetime) <= 2)
    exact_date <- NULL
    start_date <- NULL
    end_date <- NULL
    if (length(datetime) == 1) {
      exact_date <- as.Date(datetime)
      stopifnot(!is.na(extact_date))
    } else {
      start_date <- datetime[[1]]
      if (start_date != "..") {
        start_date <- as.Date(start_date)
        stopifnot(!is.na(start_date))
      }
      end_date <- datetime[[2]]
      if (end_date != "..") {
        end_date <- as.Date(end_date)
        stopifnot(!is.na(end_date))
      }
    }
  }
  if (!is.null(intersects)) {
    bbox <- as.numeric(strsplit(bbox, ",")[[1]])
    stopifnot(length(bbox) == 4)
    stopifnot(all(!is.na(bbox)))
    bbox <- bbox_as_polygon(bbox)
  }
  if (!is.null(page)) {
    page <- as.integer(page)
    stopifnot(!is.na(page))
    stopifnot(page >= 1)
  }
}
