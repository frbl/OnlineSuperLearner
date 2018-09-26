# if and with how much a binary treatment (antidepressant usage) attributes to changes in  depression.   
#We  use  a  normalized  hypothetical  set  of  sum  scores  of  the  Inventory  of  DepressiveSymptomatology  (IDS)  
#questionnaire with  a  possible  range  of  0−100.   
#Anti-depressant  usage  is a  binary  variable  sampled  from  a  binomial  distribution.  
 
#w1 gender 0/1
#w2 quality of sleep 0/1
#w3 level of activity (PAL value) between 1.2 and 4
#A antidepressant = 1 no antdepressant = 0
#Y IDS (Inventory of Depressive Symptomatology)
#Block time block
#Patient id patient identification
#Y_Y influence of Y on Y in the next block
#Y_A influence of Y on A in the next block
#Y_w2 influence of Y on w2 in the next blok

rm(simData_t)
rm(simData_t_df)
rm(simData_t0)
library("dplyr")
generateData0<-function (n,prob_w2) {
  Block<-0
  Patient_id<- seq.int(nrow(n))
  w1<-rbinom(n,size=1,prob=0.5)
  w2<-rbinom(n,size=1,prob=prob_w2)
  w3<-round(runif(n, min=1.2, max=4.0), digits =2)
  A<-rbinom(n, size=1, prob=plogis(-0.4+2*w2+0.15*w3+0.15*w2*w3))
  
  distribution_x<-function(x,max_val, minx, maxx){round((x-minx)/(maxx-minx)*max_val)}
  
  noise<-rnorm(n,mean=0, sd=0.1)
 
  Y <- (-1+-A-0.1*w1+0.3*w2+0.25*w3)+noise
  YA0 <-(-1+0-0.1*w1+0.3*w2+0.25*w3)+noise
  YA1 <-(-1+1-0.1*w1+0.3*w2+0.25*w3)+noise
  
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
      } else { row_dag <- row_dag_df_t}
      
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
      ##take the previous w3 if w2 = 1 add 0.1 if w2=0 subtract 0.1
      ## add noise to everything
      noise<-rnorm(n,mean=0, sd=0.1)
      
      
      if (row_dag$w2 == 1){row_dag$w3=row_dag$w3+0.1+noise
      } else {row_dag$w3=row_dag$w3-0.1+noise}
      
      noise<-rnorm(n,mean=0, sd=0.1)
      if (Y_w2<50){
          A<-rbinom(n, size=1, prob=plogis(-0.4+2*row_dag$w2+0.15*row_dag$w3+0.15*row_dag$w2*row_dag$w3+noise))
      } else { A<-rbinom(n, size=1, prob=plogis(-0.1+2*row_dag$w2+0.15*row_dag$w3+0.15*row_dag$w2*row_dag$w3+noise))}
     
      #n=1 when single patient_id is used
      noise<-rnorm(n,mean=0, sd=0.1)
      
      row_dag$Y <- (-1+row_dag$A-0.1*row_dag$w1+0.3*row_dag$w2+0.25*row_dag$w3+Y_w2/100)+noise
      row_dag$YA0 <- (-1+0-0.1*row_dag$w1+0.3*row_dag$w2+0.25*row_dag$w3+Y_w2/100)+noise
      row_dag$YA1 <- (-1+1-0.1*row_dag$w1+0.3*row_dag$w2+0.25*row_dag$w3+Y_w2/100)+noise
      
      
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
  


set.seed(1234)

generateDAG<-function(n){
  simData_t0 <- generateData0(100,0.65)
  patient_id <- unique(simData_t0$Patient_id)
  #simData_t <- generateLagData(simData_t0,patient_id,1,40,0.65)
  df <- data.frame(simData_t0)
  return(df)
}

simData_t0<-generateDAG()
simData_t<-data.frame(Block = numeric(),
                      w1 = integer(),
                      w2 = integer(),
                      w3 = numeric(),
                      A  = integer(),
                      Y  = numeric(),
                      YA0 =numeric(),
                      YA1 = numeric(),
                      Patient_id = integer())

library(doParallel) 
registerDoParallel(cores=8) 
for (ptn_id_value in 1:1) {
      simData_t_df <- generateLagData(simData_t0 = simData_t0,
                                ptn_id = ptn_id_value,
                                from_block = 1,
                                to_block = 40,
                                prob_w2 =0.65,
                                n = 1)
      print(ptn_id_value)
      }

simData_t_df<<- rbind(simData_t0,simData_t_df)

True_Psi <- mean(simData_t_df$YA1-simData_t_df$YA0);
cat(" True_Psi:", True_Psi)

