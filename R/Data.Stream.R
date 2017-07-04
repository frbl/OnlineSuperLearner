#' Class to provide the functionality to read data from an input stream
#'
#' @docType class
#' @include Data.Base.R
#' @importFrom R6 R6Class
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(kafka.host = 'imac.evionix.org:2181', kafka.stream = 'test')}}{
#'    This method is used to create object of this class. It expects a host for a kafka cluster (or the coordinating
#'    zookeeper therof) as input in the \code{kafka.host}. Secondly, it also expects the stream on the kafka host to
#'    listen on (\code{kafka.stream}
#'    @param kafka.host the host address for the kafka stream
#'    @param kafka.stream the particular stream to read from
#'   }
#'   \item{\code{getNext()}}{
#'     Not implemented
#'     Returns the next observation from the stream
#'     @return one row of the dataframe the stream yields
#'   }
#'   \item{\code{getNext()}}{Method to retrieve the next observation from the stream.}
#' }
#' @export
Data.Stream <- R6Class ("Data.Stream",
  inherit = Data.Base,
  private =
    list(
        consumer = NULL
        ),
  public =
    list(
        initialize = function(kafka.host = 'imac.evionix.org:2181', kafka.stream = 'test' ) {
          ########## importFrom rkafka rkafka.read rkafka.createConsumer rkafka.closeConsumer
          throw('Currently removed because of the many problems with rJava')
          ## private$consumer <- rkafka.createConsumer(zookeeperConnect = kafka.host,
                                                    ##topicName = kafka.stream,
                                                    ##consumerTimeoutMs = '100')
        },

        getNext = function() {
          ##rkafka.read(private$consumer)
        }
  )
)
