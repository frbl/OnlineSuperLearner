#' Class to provide the functionality of treating a static, finite datatable as a stream of incomming
#' data.
#'
#' @docType class
#' @include Data.Base.R
#' @importFrom R6 R6Class
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(kafka.host, kafka.stream)}}{This method is used to create object of this class. It expects a host for a kafka cluster (or the coordinating zookeeper therof) as input in the \code{kafka.host}. Secondly, it also expects the stream on the kafka host to listen on (\code{kafka.stream}}
#'
#'   \item{\code{getNext()}}{Method to retrieve the next observation from the stream.}
#' }
#' @export
Data.Stream <-
  R6Class (
           "Data.Stream",
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
                    #private$consumer <- rkafka.createConsumer(zookeeperConnect = kafka.host,
                                                              #topicName = kafka.stream,
                                                              #consumerTimeoutMs = '100')
                  },

                  getNext = function() {
                    #rkafka.read(private$consumer)
                  }
                  )
           )
