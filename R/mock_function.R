#' A Mock function
#'
#' Function to test wether the package has been set up correctly
#' @param test_param the parameter to test
#' @keywords
#' @export
#' @examples
#' mock_function()

mock_function <- function(test_param = TRUE){
  if(test_param == TRUE) {
    return('param was true')
  }
  return('param was false')
}
