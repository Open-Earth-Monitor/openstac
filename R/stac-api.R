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
  data <- list(
    type = "Catalog",
    conformsTo = conforms_to,
    links = list(
      new_link(
        rel = "self",
        href = get_endpoint(api, "/")
      ),
      new_link(
        rel = "conformance",
        href = get_endpoint(api, "/conformance")
      ),
      new_link(
        rel = "data",
        href = get_endpoint(api, "/collections")
      )
    )
  )
  data <- c(api, data)
  data
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
  get_collections(api)
}

#* Collection endpoint
#* @get /collections/<collection_id>
#* @param collection_id The ID of the collection
#* @serializer unboxedJSON
function(collection_id) {
  get_collection(api, collection_id)
}

#* Items endpoint
#* @get /collections/<collection_id>/items
#* @param collection_id The ID of the collection
#* @param limit Maximum number of features to return
#* @param bbox Bounding box (minx,miny,maxx,maxy)
#* @param datetime Datetime filter
#* @serializer unboxedJSON
function(collection_id, limit, bbox, datetime) {
  get_items(api, collection_id, limit, bbox, datetime)
}

#* Item endpoint
#* @get /collections/<collection_id>/items/<item_id>
#* @param collection_id The ID of the collection
#* @param item_id The ID of the item
#* @serializer unboxedJSON
function(collection_id, item_id) {
  get_item(api, collection_id, item_id)
}

#* Search endpoint
#* @get /search
#* @param collection_ids The list of collections' ID
#* @param limit Maximum number of features to return
#* @param bbox Bounding box (minx,miny,maxx,maxy)
#* @param datetime Datetime filter
#* @param intersects Maximum number of features to return
#* @param ids Array of items ID to return
#* @param collections Array of collection ID
#* @serializer unboxedJSON
function(limit, bbox, datetime, intersects, ids, collections) {
  # Add implementation code here
}
