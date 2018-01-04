#' Data.Stream
#' Class to provide the functionality to read data from an input stream.
#'
#' @docType class
#' @include Data.Base.R
#' @importFrom R6 R6Class
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(kafka.host = 'imac.evionix.org:2181', kafka.stream = 'test')}}{
#'    This method is used to create object of this class. It expects a host for
#'    a kafka cluster (or the coordinating zookeeper therof) as input in the
#'    \code{kafka.host}. Secondly, it also expects the stream on the kafka host
#'    to listen on (\code{kafka.stream}. Warning! This function is currently
#'    not implemented due to difficulties with the kafka R package (it needs
#'    java and rjava, which causes problems on some systmes). Although
#'    implementation is not a difficult task, it is currently removed for
#'    compatibility reasons. One can recreate this function / class when
#'    needed.
#'    @param kafka.host the host address for the kafka stream
#'    @param kafka.stream the particular stream to read from
#'   }
#'   \item{\code{getNext()}}{
#'     Currently not implemented (see \code{initialize} for reasons)
#'     Returns the next observation from the stream
#'     @return one row of the dataframe the stream yields
#'   }
#'
#'   \item{\code{getNextN(n=1)}}{
#'     Currently not implemented (see \code{initialize} for reasons)
#'     Returns the next \code{n} observations from the stream
#'     @return data.table a number of \code{n} rows concatenated in a data.table
#'   }
#'
#'   \item{\code{getNext()}}{Method to retrieve the next observation from the stream.}
#' }
#' @export
Data.Stream <- R6Class ("Data.Stream",
  inherit = Data.Base,
  public =
    list(
      initialize = function(kafka.host = 'imac.evionix.org:2181', kafka.stream = 'test' ) {
        ## importFrom rkafka rkafka.read rkafka.createConsumer rkafka.closeConsumer
        throw('Currently removed because of the many problems with rJava')
        ## private$consumer <- rkafka.createConsumer(zookeeperConnect = kafka.host,
                                                  ##topicName = kafka.stream,
                                                  ##consumerTimeoutMs = '100')
      },

      getNext = function() {
        ##rkafka.read(private$consumer)
      },

      getNextN = function(n = 1) {
        ##rkafka.read(private$consumer)
      }
    ),
  private =
    list(
      consumer = NULL
    )
)
