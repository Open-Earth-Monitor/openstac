#* @apiTitle STAC API
#* @apiDescription R STAC API server.
#* @apiVersion 1.0.0

#* Landing page
#* @get /
#* @serializer unboxedJSON
function() {
  # Add implementation code here
}

#* Collections endpoint
#* @get /collections
#* @serializer unboxedJSON
function() {
  # Add implementation code here
}

#* Collection endpoint
#* @get /collections/<collection_id>
#* @param collection_id The ID of the collection
#* @serializer unboxedJSON
function(collection_id) {
  # Add implementation code here
}

#* Items endpoint
#* @get /collections/<collection_id>/items
#* @param collection_id The ID of the collection
#* @param bbox Bounding box (minx,miny,maxx,maxy)
#* @param datetime Datetime filter
#* @serializer unboxedJSON
function(collection_id, bbox, datetime) {
  # Add implementation code here
}

#* Item endpoint
#* @get /collections/<collection_id>/items/<item_id>
#* @param collection_id The ID of the collection
#* @param item_id The ID of the item
#* @serializer unboxedJSON
function(collection_id, item_id) {
  # Add implementation code here
}

#* Search endpoint
#* @get /search
#* @param collection_ids The list of collections' ID
#* @param bbox Bounding box (minx,miny,maxx,maxy)
#* @param datetime Datetime filter
#* @serializer unboxedJSON
function(collection_ids, bbox, datetime) {
  # Add implementation code here
}
