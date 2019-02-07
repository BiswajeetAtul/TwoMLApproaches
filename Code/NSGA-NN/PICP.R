PICP=function(y)
{
  #print(y)
  w1=matrix(y[1:(4*10)],nrow = 4,ncol = 10, byrow = T )
  w2=matrix(y[((4*10)+1):length(y)],nrow = 10,ncol = 2, byrow = T )
  
  X.test <- traindata[,2:5]
  Y.test <- traindata[,1]
  
  X.test=as.matrix(X.test)
  Z.out=X.test%*%w1
  Z.out=1/(1+exp(-Z.out))
  
  output.t=Z.out%*%w2
  
  
  #PICP
  N=nrow(output.t)
  a=0
  for (i in 1:N) 
  {
    if(Y.test[i]>=output.t[i,1] && Y.test[i]<=output.t[i,2])
      c=1
    else
      c=0
    
    a=a+c
  }
  #PICP
  picp=a/N
  picp=1-picp
  
  #NMPIW
  a=0
  for(i in 1:N)
  {
    a=a+(output.t[i,2] - output.t[i,1])
  }
  nmpiw=a/N
  
  #Return fitness values
  return(c(picp,nmpiw))
}
