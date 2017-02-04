#' Data.Static
#' @include Data.Base.R
Data.Static <- setClass("Data.Static",
  representation = representation(
    dataset = "data.frame",
    url = "character",
    lazyLoad = "logical"
  ),

  prototype(
    lazyLoad = TRUE
  ),

  validity = function(object) {
    errors <- character()
    if (length(errors) == 0) TRUE else errors
  },
  contains = 'Data.Base'
)

#' getAll
setGeneric(name="getAll", def = function(obj) { standardGeneric("getAll") } )
setMethod("getAll", signature(obj = "Data.Static"),
  function(obj) {
    if (obj@lazyLoad && !is.null(obj@url)) {
      return(obj@url)
    }

    if (!is.null(obj@dataset)) {
      return(obj@dataset)
    }

    # If all fails, load the data locally in a dataframe and return that
  }
)


setMethod("getNext", signature(obj = "Data.Base"),
  function(obj) {
    if (is.null(dataset)) {
    }
  }
)
