#' Data.Stream
#' @include Data.Base.R
#' @importFrom rkafka rkafka.closeConsumer
#' @importFrom R6 R6Class
#' @include Data.Base.R
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
                  initialize = function(kafka.host='imac.evionix.org:2181', kafka.stream='test' ) {
                    private$consumer <- rkafka.createConsumer(zookeeperConnect=kafka.host,
                                                              topicName=kafka.stream,
                                                              consumerTimeoutMs='100')
                  },

                  getNext = function() {
                    rkafka.read(private$consumer)
                  }
                  )
           )
