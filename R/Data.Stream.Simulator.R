#' Class that serves as a very basic simulator to fill a kafka stream with data.
#'
#' @docType class
#' @include Simulator.Simple.R
#' @importFrom R6 R6Class
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(kafka.host = 'imac.evionix.org:9092', kafka.stream = 'test', simulator.continuous = Simulator.Simple$new(), sleep = 1, iterations = Inf)}}{ This method is used to create object of this class. It expects a host for a kafka cluster as input in the \code{kafka.host}, to which it can push the data.  Secondly, it expects the stream on the kafka host to listen on (\code{kafka.stream}, then it needs a simulator from which it can retrieve messages to push to the stream, and lastly, between each observation it will sleep for \code{sleep} number of seconds.}
#' }
#' @export
Data.Stream.Simulator <- R6Class("Data.Stream.Simulator",
  private =
    list(
        makeObservationSerializable = function(observation){
            paste(observation, collapse = ', ')
        }
        ),
  public =
    list(
        initialize = function(kafka.host = 'imac.evionix.org:9092',
                              kafka.stream = 'test',
                              simulator.continuous = Simulator.Simple$new(),
                              sleep = 1, iterations = Inf) {

          # importFrom rkafka rkafka.send rkafka.closeProducer rkafka.createProducer
          # Create the producer to communicate with the kafka cluster
          #producer <- rkafka.createProducer(kafka.host)

          i <- 0
          while(i < iterations){
            data <- makeObservationSerializable(simulator.continuous$getObservation()[1, ])
            #rkafka.send(producer, kafka.stream, kafka.host, data)
            Sys.sleep(sleep)
            i <- i + 1
          }

          # Close the kafka producer
          #rkafka.closeProducer(procuder)
        }
    )
)
