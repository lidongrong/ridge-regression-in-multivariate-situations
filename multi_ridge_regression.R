Multi_RidgeRegression<-function(y,x,K){
  
  #standardize x and y
  for(i in 1:dim(x)[2]){
    x[,i]=(x[,i]-mean(x[,i])/sd(x[,i]))
  }
  
  for(j in 1:dim(y)[2]){
    y[,j]=(y[,j]-mean(y[,j])/sd(y[,j]))
  }
  
  #generate diagnal matrix as the desired size.
  Is=diag(dim(y)[1])
  Ir=diag(dim(x)[1])
  
  
  
  #make y a vector
  vec.y=c(y)
  
  #print(vec.y)
  
  #obtain the estimation(vectorized)
  estimate=solve(kronecker(Is,x%*%t(x))+kronecker(K,Ir))%*%(kronecker(Is,x))%*%vec.y
  
  
  
  
  
  #again transform the estimation into the matrix form
  estimate=matrix(estimate,nrow=dim(y)[1],ncol=dim(x)[1])
  
  return(estimate)
  
}