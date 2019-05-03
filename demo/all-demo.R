# Turn off asking for enter
library('devtools')
library('magrittr')
library('doParallel')
library('foreach')
library('doParallel')
load_all(".")

par(ask=FALSE)
source('demo/simulation-cfg1-demo.R') 
source('demo/simulation-cfg2-demo.R') 
source('demo/simulation-cfg3-demo.R') 
source('demo/simulation-cfg4-demo.R') 
