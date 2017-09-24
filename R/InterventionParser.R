#' InterventionParser.parse_intervention
#' Function to select the current intervention from an intervention list. It
#' will check whether the current timestamp is an intervention timestamp with
#' respect to the current outcome. If it is, it will return the actual
#' intervention.
#' 
#' @param intervention the intervention to give \code{list(variable, when, what)}
#' @param current_time integer with the current time
#' @param current_outcome the current random variable
#' @param check boolean perform checks on the input variables
#' @return a list with when, what and if an intervention should be given at this time (\code{list(when, what, should_intervene)})
InterventionParser.parse_intervention <- function(intervention, current_time, current_outcome, check = FALSE) {
  when <- -1
  what <- -1
  # If no intervention is provided, we should never intervene
  if (is.null(intervention)) {
    return(list(when = when, what = what, should_intervene = FALSE))
  }
  if(check) {
    intervention <- Arguments$getInstanceOf(intervention, 'list')
    current_time <- Arguments$getInteger(current_time)
    current_outcome <- Arguments$getCharacters(current_outcome)
  }

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
  if (!is(intervention, 'list')) return(FALSE) 
  is.numeric(intervention$when) &&
    is.numeric(intervention$what) &&
    is.character(intervention$variable) &&
    length(intervention$when) == length(intervention$what)
}

#' InterventionParser.generate_intervention
#' Function that can generate interventions, especially when performing an
#' intervention on multiple nodes. What the function is do is set one of the
#' nodes to intervened (variable_intervened), and the other nodes to control. 
#' If what is 1, the intervened variable will be 1 and the rest 0, if what is
#' 0, the intervened variable will be 0 and the rest will be one.
#' @param variables the variables to create the intervention for (a list of all intervention nodes)
#' @param variable_intervened the variable to perform the intervention on
#' @param when integer when the intervention should take place
#' @param when integer when the intervention should take place
#' @export
InterventionParser.generate_intervention <- function(variables, variable_intervened, when, what) {
  what <- lapply(variables, function(a) {
    ifelse(a == variable_intervened, what, 1 - what) 
  }) %>% unlist
  list(variable = variables, when = rep(when, length(variables)), what = what)
}


#' InterventionParser.is_current_node_treatment
#' Returns whether the current nodie is a treatment node
#' @param current_time the time where we are currently
#' @param intervention the specified intervention
#' @param current_rv_output the current randomvariable output
#' @return boolean, whether this is a treatment node
InterventionParser.is_current_node_treatment = function(current_time, intervention, current_rv_output) {
  if (intervention$variable != current_rv_output) return(FALSE)
  if (!(current_time %in% intervention$when)) return(FALSE)
  return(TRUE)
}


