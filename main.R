  step1=function(lambda_function1,t_initial1=0,t_final1){
  fixed_lambda1=sapply(t_initial1:t_final1,lambda_function1)
  fixed_lambda1=max(fixed_lambda1)
  return(fixed_lambda1)
}

step2=function(size2,t_final2,fixed_lambda2){
  # s_k2=vector()
  # t=vector()
  # while(length(s_k2<size2)){
  #    u=runif(1)
  #    t=(-1/fixed_lambda2)*log(u)
  #    s_k2=c(s_k2,sum(t))
  # }
   u=runif(size2)
   t=(-1/fixed_lambda2)*log(u)
   s_k2=cumsum(t)
   s_k2=s_k2[s_k2<t_final2]
  
  return(s_k2)
}

step3=function(size3,lambda_function3,s_k3,fixed_lambda3){
  u=runif(length(s_k3))
  indicator3=(u<=lambda_function3(s_k3)/fixed_lambda3)
  indicator3=ifelse(indicator3==T,1,0)
  return(indicator3)
}
  
step4=function(indicator4,s_k4){
  aux=1:sum(indicator4)
  time_arrival=which(indicator4==1)
  t_k4=s_k4[time_arrival]
  t_k4=c(0,t_k4)
  return(t_k4)
  
}

non_homo_pp=function(lambda_function,t_initial,t_final){
  size=100
  s1=step1(lambda_function,t_initial,t_final)
 print(paste('S1=', s1))
  s2=step2(size,t_final,s1)
  print(paste('S2=', s2))
  s3=step3(size,lambda_function,s2,s1)
  print(paste('S3=',s3))
  s4=step4(s3,s2)
  print(paste('S4=',s4))
  
  return(s4)
}

##Never use a fucntion with negative values
lambda_f=function(t)t

teste=non_homo_pp(lambda_f,0,10)

plot(stepfun(teste,0:length(teste)),
     do.points = TRUE,
     pch = 16,
     col.points = "red",
     verticals = FALSE,
     )
