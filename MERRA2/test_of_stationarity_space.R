get_A<-function(Z,lag,tlag,index)
{
  nTime = dim(Z)[1]
  A1 = A2 = 0
  Sn = dim(lag)[1]
  for(s in 1:Sn)
  {
    for(t in index)
    {
      A1 = A1 + Z[t,lag[s,1]]*Z[t+tlag,lag[s,2]]
      A2 = A2 + Z[t+nTime/2,lag[s,1]]*Z[t+tlag+nTime/2,lag[s,2]]
    }
  }
  A1 = A1/Sn/length(index)
  A2 = A2/Sn/length(index)
  return(list(A1,A2))
}

get_G<-function(Z,splag,ulag,index)
{
  m = length(splag)
  G = integer(2*m)
  
  i = 0
  for(lag in splag)
  {
    i = i+1
    tmp = get_A(Z,lag,ulag[i],index);
    G[i] = tmp[[1]]
    G[i+m] = tmp[[2]]
  }
  
  return(G)
}

stationary.test<-function(Z,splag,ulag)
{
  m = length(splag)
  
  nTime = dim(Z)[1]
  index = 1:(nTime/2-1)
  
  G = get_G(Z,splag,ulag,index) * sqrt(length(index))
  
  G_ = NULL
  for(i in 1:(floor((nTime/2-5)/10)))
  {
    index = ((i-1)*10+1):(i*10)
    
    G_ = cbind(G_,get_G(Z,splag,ulag,index)*sqrt(length(index)))
    
    index = index + 5
    
    G_ = cbind(G_,get_G(Z,splag,ulag,index)*sqrt(length(index)))
  }
  
  Sigma = cov(t(G_))
  
  X=cbind(diag(m),-diag(m))
  
  ts = t(X%*%G)%*%solve(X%*%Sigma%*%t(X),X%*%G)
  return(1-pchisq(ts,df=m))
}