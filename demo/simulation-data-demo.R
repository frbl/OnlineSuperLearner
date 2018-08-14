# if and with how much a binary treatment (antidepressant usage) attributes to changes in  depression.   
#We  use  a  normalized  hypothetical  set  of  sum  scores  of  the  Inventory  of  DepressiveSymptomatology  (IDS)  
#questionnaire with  a  possible  range  of  0−100.   
#Anti-depressant  usage  is a  binary  variable  sampled  from  a  binomial  distribution.  
 
#w1 gender
#w2 quality of sleep
#w3 level of activity
#A antidepressant
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
  w1<-rbinom(n,size=1,prob=0.5)
  w2<-rbinom(n,size=1,prob=prob_w2)
  w3<-round(runif(n, min=0, max=4), digits =3)
  A<-rbinom(n, size=1, prob=plogis(-0.4+2*w2+0.15*w3+0.15*w2*w3))
  
  distribution_x<-function(x,max_val, minx, maxx){round((x-minx)/(maxx-minx)*max_val)}
  
  noise<-rnorm(n,mean=0, sd=0.1)
  
  Y <- (-1+A-0.1*w1+0.3*w2+0.25*w3)+noise
  YA0 <-(-1+0-0.1*w1+0.3*w2+0.25*w3)+noise
  YA1 <-(-1+1-0.1*w1+0.3*w2+0.25*w3)+noise
  
  minx<- min(c(Y, YA0, YA1))
  maxx<- max(c(Y, YA0,YA1))
  browser()
  Y <-distribution_x(Y,100,minx,maxx)
  YA0 <-distribution_x(YA0,100,minx,maxx)
  YA1 <-distribution_x(YA1,100,minx,maxx)
  
  data<-data.frame(Block,w1,w2,w3,A,Y,YA0,YA1,minx,maxx)
  data$Patient_id <- seq.int(nrow(data))
  return (data)
  
  
}

generateLagData<-function (n,prob_w2) {
  for (i in 1:n){
  row_dag<-data.frame(filter(simData_t0,Patient_id==3))
    if (i==1){
     
      row_dag$Block<-row_dag$Block+1
      row_dag$w1<-row_dag$w1
      row_dag$Y<-row_dag$Y
      Y_w2<-row_dag$Y
      ##calculate w2 depending on Y and previous W2
      ##if Y> 50 increase if Y<50 decrease probability
      ##if w2 = 0 decreas if w2= increase probability
      
      if (Y_w2<50){
       delta_prob_w2<--(Y_w2/1000)
      } else {delta_prob_w2<-(Y_w2/1000)}
      if (row_dag$w2==0){delta_prob_w2=delta_prob_w2-0.01
      } else{delta_prob_w2=delta_prob_w2+0.01}
      
      row_dag$w2<-rbinom(1,size=1,prob=prob_w2+delta_prob_w2)
      ##calculate w3 depending on w2
      ##take the previous w3 if w2 = 1 add 0.1 if w2=0 subtract 0.1
      if (row_dag$w2 == 1){row_dag$w3=row_dag$w3+0.1
      } else {row_dag$w3=row_dag$w3-0.1}
      
      noise<-rnorm(n,mean=0, sd=0.1)
      
      row_dag$Y <- (-1+row_dag$A-0.1*row_dag$w1+0.3*row_dag$w2+0.25*row_dag$w3)+noise
      row_dag$YA0 <- (-1+0-0.1*row_dag$w1+0.3*row_dag$w2+0.25*row_dag$w3)+noise
      row_dag$YA1 <- (-1+1-0.1*row_dag$w1+0.3*row_dag$w2+0.25*row_dag$w3)+noise
      distribution_x<-function(x,max_val, minx, maxx){round((x-minx)/(maxx-minx)*max_val)}
      
      minx<- row_dag$minx
      maxx<- row_dag$maxx
      browser()
      row_dag$Y <-distribution_x(row_dag$Y,100,minx,maxx)
      row_dag$YA0 <- distribution_x(row_dag$YA0,100,minx,maxx)
      row_dag$YA1 <- distribution_x(row_dag$YA1,100,minx,maxx)
      simData_t<-data.frame(row_dag)
    }
  return (simData_t)
  
  }

}
simData_t<-generateLagData(1,0.65)
set.seed(1234)

generateDAG<-function(n){
  simData_t0<-generateData0(10,0.65)
  simData_t<-generateLagData(1,0.65)
}

generateDAG(3)

w3<-round(runif(1, min=0, max=4), digits =3)

True_Psi <- mean(simData$YA1-simData$YA0);
cat(" True_Psi:", True_Psi)

min(simData$Y)

min=0
max=100
print(sample(1:2, size=10, prob=c(1,3), replace=TRUE))
sample(x=min:max, prob=dnorm(...))