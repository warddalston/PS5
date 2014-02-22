voter <- function(n=100,dist="s",sd1=1,sd2=1.1,mu1=NULL,Sigma1=NULL,mu2=NULL,Sigma2=NULL,mu3=NULL,Sigma3=NULL){
  # 'dist' means the distibution of preferences
  if(dist=="s"){
  mat1 <- matrix(rnorm(n*2),nrow=n)  
  } 
  if(dist=="n"){
  dim1 <- rnorm(n,sd=sd1)  
  dim2 <- rnorm(n,sd=sd2)
  mat1 <- cbind(dim1, dim2)
  }
  if(dist=="u"){
  mat1 <- matrix(runif(n*2),nrow=n)
  }
  if(dist=="m"){
  library(MASS)
    mat1 <- mvrnorm(n,mu1,Sigma1) 
  }
  if(dist=="mm"){
    r <- sample(3*n, n)
    mat1 <- mvrnorm(n,mu1,Sigma1)
    mat2 <- mvrnorm(n,mu2,Sigma2)
    mat3 <- mvrnorm(n,mu3,Sigma3)
    mat4 <- rbind(mat1, mat2, mat3)
    mat1 <- mat4[r,]  
  }
  return(mat1)  
}

install.packages("pdist")
library(pdist)
party <- matrix(rnorm(4),ncol=2)
ll <- pdist(mat1,party)
distance <- matrix(ll@dist, ncol=2, byrow=TRUE)
distance[,1] > distance[,2]