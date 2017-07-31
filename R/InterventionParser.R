
InterventionParser.parse_intervention <- function(intervention, current_time, current_outcome) {
  intervention <- Arguments$getInstanceOf(intervention, 'list')
  current_time <- Arguments$getInteger(current_time)
  current_outcome <- Arguments$getCharacters(current_outcome)

  when <- -1
  what <- -1
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
