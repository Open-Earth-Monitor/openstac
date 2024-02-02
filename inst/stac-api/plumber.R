library(stacserver)

#* @apiTitle STAC API
#* @apiDescription R STAC API server.
#* @apiVersion 1.0.0

# Create stacserver API object
api <- create_api_stac(
  title = "R STAC API server",
  description = "This is a STAC API 1.0.0 compliant R backend."
)

# Set API database
db_file <- system.file("db/openlandmap.rds", package = "stacserver")
api <- set_db(api, driver = "local", file = db_file)

#* Landing page
#* @get /
#* @serializer unboxedJSON
function(req, res) {
  api_landing_page(api) |>
    doc_links_landing_page(get_host(req))
}

#* Conformance endpoint
#* @get /conformance
#* @serializer unboxedJSON
function(req, res) {
  api_conformance(api)
}

#* Collections endpoint
#* @get /collections
#* @serializer unboxedJSON
function(req, res) {
  api_collections(api) |>
    doc_links_collections(get_host(req))
}

#* Collection endpoint
#* @get /collections/<collection_id>
#* @param collection_id:str The ID of the collection
#* @serializer unboxedJSON
function(req, res, collection_id) {
  api_collection(api, collection_id) |>
    doc_links_collection(get_host(req))
}

#* Items endpoint
#* @get /collections/<collection_id>/items
#* @param collection_id:str The ID of the collection
#* @param limit:int Maximum number of features to return (default: 10)
#* @param bbox:str Bounding box (minx,miny,maxx,maxy)
#* @param datetime:str Datetime filter
#* @param page:int Pagination parameter (default: 1)
#* @serializer unboxedJSON
function(req,
         res,
         collection_id,
         limit = 10,
         bbox,
         datetime,
         page = 1) {
  # check parameters
  if (!is.null(limit)) {
    limit <- parse_int(limit[[1]])
    check_limit(limit, min = 1, max = 10000)
  }
  if (missing(bbox)) bbox <- NULL
  if (!is.null(bbox)) {
    bbox <- parse_dbl(bbox)
    check_bbox(bbox)
  }
  if (missing(datetime)) datetime <- NULL
  if (!is.null(datetime)) {
    datetime <- parse_datetime(datetime[[1]])
    check_datetime(datetime)
  }
  if (!is.null(page)) {
    page <- parse_int(page[[1]])
    check_page(page)
  }
  # call api items
  doc <- api_items(
    api = api,
    collection_id = collection_id,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    page = page
  )
  doc_links_items(
    doc = doc,
    host = get_host(req),
    collection_id = collection_id,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    page = page
  )
}

#* Item endpoint
#* @get /collections/<collection_id>/items/<item_id>
#* @param collection_id:str The ID of the collection
#* @param item_id:str The ID of the item
#* @serializer unboxedJSON
function(req, res, collection_id, item_id) {
  api_item(api, collection_id, item_id) |>
    doc_links_item(get_host(req), collection_id)
}

#* Search endpoint
#* @get /search
#* @post /search
#* @param limit:int Maximum number of features to return (default: 10)
#* @param bbox:str Bounding box (minx,miny,maxx,maxy)
#* @param datetime:str Datetime filter
#* @param intersects:str GeoJSON geometry to do spatial search
#* @param ids:str Array of items ID to return
#* @param collections:str Array of collection ID
#* @param page:int Pagination parameter (default: 1)
#* @serializer unboxedJSON
function(req,
         res,
         limit = 10,
         bbox,
         datetime,
         intersects,
         ids,
         collections,
         page = 1) {
  # check parameters
  if (!is.null(limit)) {
    limit <- parse_int(limit[[1]])
    check_limit(limit, min = 1, max = 10000)
  }
  if (missing(bbox)) bbox <- NULL
  if (missing(intersects)) intersects <- NULL
  api_stopifnot(
    is.null(bbox) || is.null(intersects),
    status = 400,
    "only one of either intersects or bbox may be provided"
  )
  if (!is.null(bbox)) {
    bbox <- parse_dbl(bbox)
    check_bbox(bbox)
  }
  if (missing(datetime)) datetime <- NULL
  if (!is.null(datetime)) {
    datetime <- parse_datetime(datetime[[1]])
    check_datetime(datetime)
  }
  method <- get_method(req)
  if (!is.null(intersects)) {
    api_stopifnot(
      method == "POST",
      status = 405,
      "the request method is not supported"
    )
    intersects <- parse_json(intersects)
    check_intersects(intersects)
  }
  if (missing(ids)) ids <- NULL
  if (!is.null(ids)) ids <- parse_str(ids)
  if (!is.null(collections)) {
    collections <- parse_str(collections)
    check_collections(collections)
  }
  if (!is.null(page)) {
    page <- parse_int(page[[1]])
    check_page(page)
  }
  # call api search
  doc <- api_search(
    api = api,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    intersects = intersects,
    ids = ids,
    collections = collections,
    page = page
  )
  doc_links_search(
    doc = doc,
    host = get_host(req),
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    ids = ids,
    collections = collections,
    page = page,
    method = method
  )
}
