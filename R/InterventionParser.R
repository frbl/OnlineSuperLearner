#' InterventionParser.parse_intervention
#' Function to select the current intervention from an intervention list. It
#' will check whether the current timestamp is an intervention timestamp with
#' respect to the current outcome. If it is, it will return the actual
#' intervention.
#' 
#' @param intervention the intervention to give \code{list(variable, when, what)}
#' @param current_time integer with the current time
#' @param current_outcome the current random variable
#' @return a list with when, what and if an intervention should be given at this time (\code{list(when, what, should_intervene)})
InterventionParser.parse_intervention <- function(intervention, current_time, current_outcome) {
  when <- -1
  what <- -1
  # If no intervention is provided, we should never intervene
  if (is.null(intervention)) {
    return(list(when = when, what = what, should_intervene = FALSE))
  }
  intervention <- Arguments$getInstanceOf(intervention, 'list')
  current_time <- Arguments$getInteger(current_time)
  current_outcome <- Arguments$getCharacters(current_outcome)

  if(!is.null(intervention) && current_outcome %in% intervention$variable ) {
    var_idx <- which(intervention$variable == current_outcome)
    when_idx <- which(intervention$when[var_idx] == current_time)
    if(length(when_idx) != 0){
      when_idx <- when_idx[[1]]
      when <- intervention$when[var_idx][when_idx]
      what <- intervention$what[var_idx][when_idx]
    }
  }
  list(when = when, what = what, should_intervene = (when == current_time))
}

#' InterventionParser.first_intervention
#' Returns the time at which the first ever intervention should be given.
#' 
#' @param intervention the intervention to give \code{list(variable, when, what)}
#' @return the first ever time at which an intervention is given
InterventionParser.first_intervention <- function(intervention) {
  when = -1
  if (is.null(intervention)) {
    return(c(when = when))
  }
  intervention <- Arguments$getInstanceOf(intervention, 'list')
  return(c(when = min(intervention$when)))
}

#' InterventionParser.valid_intervention
#' Checks whether the provided intervention is valid
#'
#' @param intervention the intervention to check the validity of
#' @return boolean true if valid, false if not
InterventionParser.valid_intervention <- function(intervention) {
  intervention <- Arguments$getInstanceOf(intervention, 'list')
  is.numeric(intervention$when) &&
    is.numeric(intervention$what) &&
    is.character(intervention$variable) &&
    length(intervention$when) == length(intervention$what)
}
