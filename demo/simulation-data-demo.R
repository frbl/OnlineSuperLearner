# if and with how much a binary treatment (antidepressant usage) attributes to changes in  depression.   
#We  use  a  normalized  hypothetical  set  of  sum  scores  of  the  Inventory  of  DepressiveSymptomatology  (IDS)  
#questionnaire with  a  possible  range  of  0−100.   
#Anti-depressant  usage  is a  binary  variable  sampled  from  a  binomial  distribution.  
 
#w1 gender 0/1
#w2 quality of sleep 0/1
#w3 level of activity (PAL value) between 1.4 and 2.4
#A antidepressant = 1 no antdepressant = 0
#Y IDS (Inventory of Depressive Symptomatology)
#Block time block
#Patient id patient identification
#Y_Y influence of Y on Y in the next block
#Y_A influence of Y on A in the next block
#Y_w2 influence of Y on w2 in the next blok


library("dplyr")
generateData0<-function (n,prob_w2) {
  Block<-0
  Patient_id<- seq.int(nrow(n))
  #man = 1 or woman =0
  w1<-rbinom(n,size=1,prob = 0.5)
  # level of sleep good = 1 bad = 0
  w2<-rbinom(n,size=1,prob = prob_w2)
  
  # level of activity PAL <1.4 is almost doing nothing PAL > 2.4 is very active
  #Extremely inactive	Cerebral Palsy patient	<1.40
  #Sedentary	Office worker getting little or no exercise	1.40-1.69
  #Moderately active	Construction worker or person running one hour daily	1.70-1.99
  #Vigorously active	Agricultural worker (non mechanized) or person swimming two hours daily	2.00-2.40
  #Extremely active	Competitive cyclist	>2.40
  # bad sleep is reducing the level of activity
  Getw3 <- function (w2){
    if (w2 == 1) {
       min_val = 1.7
       max_val = 2.4
     }
  else {
       min_val = 1.4
       max_val = 2.0
     }
  w3<-runif(1,min = min_val,max = max_val)
  return(w3)
  }  
  
  wv<-Vectorize(Getw3)
  w3<-wv(w2)
  
  
  A<-rbinom(n, size=1, prob=plogis(-0.4+0.1*w2+0.15*w3+0.15*w2*w3))
  
  distribution_x<-function(x,max_val, minx, maxx){round((x-minx)/(maxx-minx)*max_val)}
  
  noise<-rnorm(n,mean=0, sd=0.1)
 
  Y <- (-1+-A-0.1*w1+0.4*w2+0.3*w3)+noise
  #counterfactual
  YA0 <-(-1-0-0.1*w1+0.4*w2+0.3*w3)+noise
  YA1 <-(-1-1-0.1*w1+0.4*w2+0.3*w3)+noise
  
  minx<- min(c(Y, YA0, YA1))
  maxx<- max(c(Y, YA0,YA1))
  #browser()
  Y <-distribution_x(Y,100,minx,maxx)
  YA0 <-distribution_x(YA0,100,minx,maxx)
  YA1 <-distribution_x(YA1,100,minx,maxx)
  
  data<-data.frame(Block,w1,w2,w3,A,Y,YA0,YA1,minx,maxx)
  data$Patient_id <- seq.int(nrow(data))
  return (data)
  
  
}

generateLagData<-function (simData_t0,ptn_id,from_block,to_block,prob_w2,n) {
    for(j in from_block:to_block) {
      if (j == 1){
        
         row_dag_df_0<-data.frame(filter(simData_t0,Patient_id==ptn_id))
         row_dag<-row_dag_df_0
      } else {row_dag <- row_dag_df_t}
      
      row_dag$Block<-row_dag$Block+1
      row_dag$w1<-row_dag$w1
      row_dag$Y<-row_dag$Y
      Y_w2<-row_dag$Y
      #browser()
      ##calculate w2 depending on Y and previous W2
      ##if Y> 50 increase if Y<50 decrease probability
      ##if w2 = 0 decrease if w2= 1 increase probability
      if (Y_w2<50){
       delta_prob_w2<--(-(Y_w2/250))
      } else {delta_prob_w2<-(Y_w2/500)}
      
      if (row_dag$w2==0){delta_prob_w2=delta_prob_w2-0.10
      } else{delta_prob_w2=delta_prob_w2+0.10}
      
      row_dag$w2<-rbinom(n,size=1,prob=prob_w2+delta_prob_w2)
      ##calculate w3 depending on w2
      ## categorize using the PAL scale
      ## function is neat, vectorization as in the first function is only needed when vectors are used
      Getw3 <- function (w2){
        if (w2 == 1) {
          min_val = 1.7
          max_val = 2.4
        }
        else {
          min_val = 1.4
          max_val = 2.0
        }
        w3<-runif(1,min = min_val,max = max_val)
        return(w3)
      }  
      
      row_dag$w3<-Getw3(row_dag$w2)
      
      #The use of A
      noise<-rnorm(n,mean=0, sd=0.1)
      
      if (Y_w2<50){
          A<-rbinom(n, size=1, prob=plogis(-0.4+0.1*row_dag$w2+0.15*row_dag$w3+0.15*row_dag$w2*row_dag$w3+noise))
      } else { A<-rbinom(n, size=1, prob=plogis(-0.4+0.1*row_dag$w2+0.15*row_dag$w3+0.15*row_dag$w2*row_dag$w3+noise))}
     
      #n=1 when single patient_id is used
      noise<-rnorm(n,mean=0, sd=0.1)
      
      row_dag$Y <- (-1-row_dag$A-0.1*row_dag$w1+0.4*row_dag$w2+0.3*row_dag$w3+Y_w2/100)+noise
      #counter factual
      row_dag$YA0 <- (-1-0-0.1*row_dag$w1+0.4*row_dag$w2+0.3*row_dag$w3+Y_w2/100)+noise
      row_dag$YA1 <- (-1-1-0.1*row_dag$w1+0.4*row_dag$w2+0.3*row_dag$w3+Y_w2/100)+noise
      
      
      row_dag_df_t<-data.frame(row_dag)
      #if (j == 1) {
      #simData_t <<- rbind(row_dag_df_0,row_dag_df_t)
      #} else 
      #  {
      simData_t <<- rbind (simData_t,row_dag_df_t)
      #}
    }
  
    distribution_x<-function(x,max_val, minx, maxx){round((x-minx)/(maxx-minx)*max_val)}
    
    minx<- min(c(simData_t$Y, simData_t$YA0,simData_t$YA1))
    maxx<- max(c(simData_t$Y, simData_t$YA0,simData_t$YA1))
    #browser()
    simData_t$Y <-distribution_x(simData_t$Y,100,minx,maxx)
    simData_t$YA0 <- distribution_x(simData_t$YA0,100,minx,maxx)
    simData_t$YA1 <- distribution_x(simData_t$YA1,100,minx,maxx)
    return (simData_t)
   }
  




generateDAG<-function(n,prob_w2){
  simData_t0 <- generateData0(n,prob_w2)
  df <- data.frame(simData_t0)
  return(df)
}

calc_blocks<-function(n,from_block_val,to_block_val,prob_w2_val){ 
for (ptn_id_value in 1:n) {
  simData_t_df <- generateLagData(simData_t0 = simData_t0,
                                  ptn_id = ptn_id_value,
                                  from_block = from_block_val,
                                  to_block = to_block_val,
                                  prob_w2 =prob_w2_val,
                                  n = 1)
  
  }

return(simData_t_df)
}

#call functions
set.seed(1234)
library(doParallel) 
registerDoParallel(cores=4) 
## remove previous dataframes

rm(simData_t)
rm(simData_t_df)
rm(simData_t0)

#calculate block zero
#n is number of participants
n<-10
#p_w2 is the probability of good sleep
p_w2<-0.65
## b_b is begin_block
b_b<-1
## e_b is end_block
e_b<-40

simData_t0<-generateDAG(n,p_w2)
#calculate following blocks
simData_t<-data.frame(Block = numeric(),
                      w1 = integer(),
                      w2 = integer(),
                      w3 = numeric(),
                      A  = integer(),
                      Y  = numeric(),
                      YA0 =numeric(),
                      YA1 = numeric(),
                      Patient_id = integer())


simData_t_df<-calc_blocks(n,b_b,e_b,p_w2)
#combine block zero and the following blocks
simData_t_df<<- rbind(simData_t0,simData_t_df)

# Write CSV 
#write.csv(simData_t_df, file = "simData.csv",row.names=FALSE)

True_Psi <- mean(simData_t_df$YA1-simData_t_df$YA0);
cat(" True_Psi:", True_Psi)

