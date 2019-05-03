# Turn off asking for enter
library('devtools')
library('magrittr')
library('doParallel')
library('foreach')
library('doParallel')
load_all(".")

par(ask=FALSE)
demo('simulation-cfg1-demo', package='OnlineSuperLearner') 
demo('simulation-cfg2-demo', package='OnlineSuperLearner') 
demo('simulation-cfg3-demo', package='OnlineSuperLearner') 
demo('simulation-cfg4-demo', package='OnlineSuperLearner') 
