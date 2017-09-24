library('devtools')
suppressWarnings(load_all('.'))
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  OnlineSuperLearner.Simulation$new()
} else if (length(args)==1) {
  OnlineSuperLearner.Simulation$new(configuration = args[1])
}
