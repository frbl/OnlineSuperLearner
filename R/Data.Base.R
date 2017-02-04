#' Data.Base
Data.Base <- setClass("Data.Base",
  representation = representation(
     family = "character"
    )
  )

setGeneric("getNext", function(obj) standardGeneric("getNext"))
setMethod("getNext", signature(obj = "Data.Base"),
  function(obj) {
    throw('This method needs to be inherited')
  }
)
