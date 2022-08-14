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

nhpp_simulation=function(lambda_function,t_initial,t_final){
  s1=step1(lambda_function,t_initial,t_final)
 print(paste('S1=', s1))
 size=s1*10
  s2=step2(size,t_final,s1)
  print(paste('S2=', s2))
  s3=step3(size,lambda_function,s2,s1)
  print(paste('S3=',s3))
  s4=step4(s3,s2)
  print(paste('S4=',s4))
  
  return(s4)
}

##Never use a fucntion with negative values
lambda_f=function(t)dnorm(t)

teste=nhpp_simulation(lambda_f,0,15)

plot(stepfun(teste,0:length(teste)),
     do.points = TRUE,
     pch = 16,
     col.points = "red",
     verticals = FALSE,
     )

## Part 2
## Probability functions
simple_integral=function(func,a,b){
  size=50000
  u=runif(size,min=a,max=b)
  y_func=func(u)
  y_mean=mean(y_func)
  area=(b-a)*y_mean
  return(area)
}


nhpp_mean=function(lambda_function,t_initial,t_final){
  mean_calc=simple_integral(lambda_f,t_initial,t_final)
  return(mean_calc)
}

nhpp_probs=function(lambda_function,t_initial,t_final,k='N'){
  mean_calc=nhpp_mean(lambda_function,t_initial,t_final)
  probs_calc=function(q)exp(-1*mean_calc)*(mean_calc^q)/factorial(q)
  if(is.numeric(k)==T)
    probs_calc=probs_calc(k)
  return(probs_calc)
}

nhpp_mean(lambda_f,0,10)
teste=nhpp_probs(lambda_f,0,3)
curve(teste,0,100)
sum(sapply(1:100,teste))

url=read_html('https://statistik.d-u-v.org/getresultevent.php?event=81420&cat=M&country=all&speed=1&aktype=2&Submit.x=26&Submit.y=9')

data=url%>%
  html_nodes('#EvtRslt td:nth-child(2)')%>%
  html_text()

data=gsub('h','',data)

data=as.POSIXlt(data,format="%H:%M")
data=format(data, "%H:%M")




data=sapply(strsplit(data,":"),
       function(x) {
         x <- as.numeric(x)
         x[1]+x[2]/60
       }
)

hist(data)

qqnorm(data, main = "", xlab = "Quantis teóricos N(0,1)", pch = 20,
       ylab = "Velocidade (km/m)")
qqline(data, lty = 2, col = "red")

media=mean(data)
desvio=sd(data)

hist(data,freq=F)
curve(dnorm(x,mean=media,sd=desvio),add=T)
