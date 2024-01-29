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
  api_landing_page(api)
}

#* Conformance endpoint
#* @get /conformance
#* @serializer unboxedJSON
function() {
  api_conformance(api)
}

#* Collections endpoint
#* @get /collections
#* @serializer unboxedJSON
function() {
  api_collections(api)
}

#* Collection endpoint
#* @get /collections/<collection_id>
#* @param collection_id:str The ID of the collection
#* @serializer unboxedJSON
function(collection_id) {
  api_collection(api, collection_id)
}

#* Items endpoint
#* @get /collections/<collection_id>/items
#* @param collection_id:str The ID of the collection
#* @param limit:int Maximum number of features to return (default: 10)
#* @param bbox:[int] Bounding box (minx,miny,maxx,maxy)
#* @param datetime:str Datetime filter
#* @param page:int Pagination parameter (default: 1)
#* @serializer unboxedJSON
function(collection_id,
         limit = 10,
         bbox,
         datetime,
         page = 1) {
  # check parameters
  if (!is.null(limit)) {
    limit <- as.integer(limit)
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
  if (missing(bbox)) bbox <- NULL
  if (!is.null(bbox)) {
    bbox <- as.numeric(bbox)
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
  if (missing(datetime)) datetime <- NULL
  exact_date <- NULL
  start_date <- NULL
  end_date <- NULL
  if (!is.null(datetime)) {
    datetime <- strsplit(datetime, "/")[[1]]
    api_stopifnot(
      value = length(datetime) == 1 || length(datetime) == 2,
      code = 400,
      message = "datetime is not a valid time stamp or time interval"
    )
    if (length(datetime) == 1) {
      exact_date <- as.Date(datetime)
      api_stopifnot(
        value = !is.na(extact_date),
        code = 400,
        message = "datetime is not a valid time stamp or time interval"
      )
    } else {
      start_date <- datetime[[1]]
      if (start_date != "..") {
        start_date <- as.Date(start_date)
        api_stopifnot(
          value = !is.na(start_date),
          code = 400,
          message = "datetime is not a valid time stamp or time interval"
        )
      }
      end_date <- datetime[[2]]
      if (end_date != "..") {
        end_date <- as.Date(end_date)
        api_stopifnot(
          value = !is.na(end_date),
          code = 400,
          message = "datetime is not a valid time stamp or time interval"
        )
      }
    }
  }
  if (!is.null(page)) {
    page <- as.integer(page)
    stopifnot(!is.na(page))
    stopifnot(page >= 1)
  }
  # call api items
  api_items(
    api = api,
    collection_id = collection_id,
    limit = limit,
    bbox = bbox,
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
  api_item(api, collection_id, item_id)
}

#* Search endpoint
#* @get /search
#* @post /search
#* @param limit:int Maximum number of features to return (default: 10)
#* @param bbox:[int] Bounding box (minx,miny,maxx,maxy)
#* @param datetime:str Datetime filter
#* @param intersects:list GeoJSON geometry to do spatial search
#* @param ids:[str] Array of items ID to return
#* @param collections:[str] Array of collection ID
#* @param page:int Pagination parameter (default: 1)
#* @serializer unboxedJSON
function(req,
         limit = 10,
         bbox,
         datetime,
         intersects,
         ids,
         collections,
         page = 1) {
  # check parameters
  if (!is.null(limit)) {
    limit <- as.integer(limit)
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
  stopifnot(is.null(bbox) || is.null(intersects))
  if (missing(bbox)) bbox <- NULL
  if (!is.null(bbox)) {
    bbox <- as.numeric(bbox)
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
  if (missing(datetime)) datetime <- NULL
  exact_date <- NULL
  start_date <- NULL
  end_date <- NULL
  if (!is.null(datetime)) {
    datetime <- strsplit(datetime, "/")[[1]]
    api_stopifnot(
      value = length(datetime) == 1 || length(datetime) == 2,
      code = 400,
      message = "datetime is not a valid time stamp or time interval"
    )
    if (length(datetime) == 1) {
      exact_date <- as.Date(datetime)
      api_stopifnot(
        value = !is.na(extact_date),
        code = 400,
        message = "datetime is not a valid time stamp or time interval"
      )
    } else {
      start_date <- datetime[[1]]
      if (start_date != "..") {
        start_date <- as.Date(start_date)
        api_stopifnot(
          value = !is.na(start_date),
          code = 400,
          message = "datetime is not a valid time stamp or time interval"
        )
      }
      end_date <- datetime[[2]]
      if (end_date != "..") {
        end_date <- as.Date(end_date)
        api_stopifnot(
          value = !is.na(end_date),
          code = 400,
          message = "datetime is not a valid time stamp or time interval"
        )
      }
    }
  }
  if (missing(intersects)) intersects <- NULL
  if (!is.null(intersects)) {
    stopifnot(is_geom(intersects))
  }
  if (missing(ids)) ids <- NULL
  if (!is.null(ids)) ids <- as.character(ids)
  if (missing(collections)) collections <- NULL
  if (!is.null(collections)) {
    collections <- as.character(collections)
    stopifnot(length(collections) >= 1)
  }
  if (!is.null(page)) {
    page <- as.integer(page)
    stopifnot(!is.na(page))
    stopifnot(page >= 1)
  }
  # call api search
  api_search(
    api = api,
    limit = limit,
    bbox = bbox,
    exact_date = exact_date,
    start_date = start_date,
    end_date = end_date,
    intersects = intersects,
    ids = ids,
    collections = collections,
    page = page,
    method = req$REQUEST_METHOD
  )
}
