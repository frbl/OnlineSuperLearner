#' InterventionParser.parse_intervention
#' 
#' Function to select the current intervention from an intervention list. It
#' will check whether the current timestamp is an intervention timestamp with
#' respect to the current outcome. If it is, it will return the actual
#' intervention. The intervention needs to be specified as follows. Each
#' intervention is a list consisting of three attributes: \code{variable},
#' \code{when}, and \code{what}. The \code{variable} entry denotes the name of
#' the variable on which the intervention is done. Usually this is the 'A'
#' variable, but this could be tweaked. Secondly one has to specify the
#' \code{when} variable. This variable denotes when the intervention should be
#' performed. Lastly the \code{what} variable. This denotes the actual
#' intervention to be performed. E.g., one could denote treatment as 1 and
#' control as 0. Then an intervention could be to give everybody a treatment,
#' forcing everyone to have a 1 as treatment variable. This would then boil
#' down to a \code{what} of 1.
#' 
#' @param intervention the intervention to give \code{list(variable, when, what)}
#'
#' @param current_time integer with the current time
#'
#' @param current_outcome string the name of the current relevant variable
#'
#' @param check boolean perform checks on the input variables
#'
#' @return a list with when, what and if an intervention should be given at
#'  this time (\code{list(when, what, should_intervene)}). If one whould not
#'  intervene, the when and what are -1.
#' @export
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
#' 
#' Returns the time at which the first ever intervention should be given.
#' 
#' @param intervention the intervention to give \code{list(variable, when, what)}.
#' 
#' @return the first ever time at which an intervention is given.
#' @export
InterventionParser.first_intervention <- function(intervention) {
  when = -1
  if (is.null(intervention)) {
    return(c(when = when))
  }
  intervention <- Arguments$getInstanceOf(intervention, 'list')
  return(c(when = min(intervention$when)))
}

#' InterventionParser.valid_intervention
#' 
#' Checks whether the provided intervention is valid.
#'
#' @param intervention the intervention to check the validity of
#' @return boolean \code{TRUE} if valid, \code{FALSE} if not
#' @export
InterventionParser.valid_intervention <- function(intervention) {
  if (!is(intervention, 'list')) return(FALSE) 
  is.numeric(intervention$when) &&
    is.numeric(intervention$what) &&
    is.character(intervention$variable) &&
    length(intervention$when) == length(intervention$what)
}

#' InterventionParser.generate_intervention
#' 
#' Function that can generate interventions, especially when performing an
#' intervention on multiple nodes. What the function is do is set one of the
#' nodes to intervened (variable_intervened), and the other nodes to control. 
#' If what is 1, the intervened variable will be 1 and the rest 0, if what is
#' 0, the intervened variable will be 0 and the rest will be one.
#' 
#' @param variables the variables to create the intervention for (a list of all intervention nodes)
#' 
#' @param variable_intervened the variable to perform the intervention on
#' 
#' @param when integer when the intervention should take place
#' 
#' @param what integer hat the intervention should be
#' 
#' @return an intervention 'object' (i.e., a list following the intervention specification.
#' @export
InterventionParser.generate_intervention <- function(variables, variable_intervened, when, what) {
  # If no what is given, make them all 0
  if(is.null(variable_intervened)) {
    what <- rep(0, length(variables))
  } else {
    what <- lapply(variables, function(a) {
      ifelse(a == variable_intervened, what, 1 - what) 
    }) %>% unlist
  }
  list(variable = variables, when = rep(when, length(variables)), what = what)
}


#' InterventionParser.is_current_node_treatment
#'
#' Returns whether the current nodie is a treatment node.
#'
#' @param current_time the time where we are currently
#'
#' @param intervention the specified intervention
#'
#' @param current_rv_output the current relevantvariable output
#'
#' @return boolean, whether this is a treatment node
#' @export
InterventionParser.is_current_node_treatment = function(current_time, intervention, current_rv_output) {
  if (intervention$variable != current_rv_output) return(FALSE)
  if (!(current_time %in% intervention$when)) return(FALSE)
  return(TRUE)
}

