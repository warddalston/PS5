#################################################################################
## PS 5 Simulation Activity - Myunghoon Kang and Dalston Ward - February 2014 ###
#################################################################################

#First, clear the work space
rm(list=ls())

#Second, load necessary packages
library(MASS)
library(pdist) #don't forget to install this if necessary! 

#Voter Distributions - A function for creating distributions of voter preferences

#This function allows the user to a create a distribution of voters with preferences on two dimensions.  The user is allowed to choose one of five distributional set-ups from which preferences are drawn.  These are standard normals, normal distributions with user specified variances, uniform distributions, a multivariate normal with a user specified vector of means and variance-covariance matrix, and finally, up to three multivariate normal distributions, where each voter is equally likely to have her preferences drawn from each distribution. 

#input: n - the number of voters.  Defaults to 100
#       dist - one of four values: "s" for standard normal, "n" for user sepecified normal, "u" for uniform, "m" for one of the multivariate options. Defaults to "s"
#       vars - a vector of two variances for use in the "n" option.  Defaults to c(1,1).
#       mu - a 2 by d matrix of means for use in the multivariate normal distributions, where each column represents a vector of means for a multivariate normal distributions.  d represents the number of multivariate distributions from which preferences are drawn, it can be 1,2, or 3.  
#       Sigma - a 2 by 2 by d array, where each table is a variance-covariance matrix for a multivariate normal distribution.  d represetns the number of multivariate distributions from which preferences are drawn, it can be 1,2 or 3. 
#       ... - arguments to be passed further on (such as min and max for runif!)

#Output: an n by 2 matrix of voter preferences 

#Authors: Myunghoon Kang and Dalston Ward

VoterDistribution <- function(n=100,dist="s",vars=c(1,1),mu=NULL,Sigma=NULL,...){
  if(dist=="s"){
  mat1 <- matrix(rnorm(n*2),nrow=n)  
  } 
  if(dist=="n"){
  dim1 <- rnorm(n,sd=sqrt(vars[1]))  #it now takes variances instead of SD's (in accord with the directions from the worksheet)
  dim2 <- rnorm(n,sd=sqrt(vars[2]))
  mat1 <- cbind(dim1, dim2)
  }
  if(dist=="u"){
  mat1 <- matrix(runif(n*2,...),nrow=n) #The dots allow one to specify the min and max if desired.  
  }
  if(dist=="m"){ #I collappsed m and mm, trying to make it a bit more generalized. 
    mat1 <- mvrnorm(n,mu[,1],Sigma[,,1]) #mu (matrix) and sigma (array) are multidimensional objects now, and the function determines what to do based on the number of columns.  
    if(ncol(mu)==2){ #the user can now choose to have preferences be based on 1, 2 or 3 distributions.
    r <- sample(2*n, n)
    mat2 <- mvrnorm(n,mu[,2],Sigma[,,2])
    mat4 <- rbind(mat1, mat2)
    mat1 <- mat4[r,]
    }
    if(ncol(mu)==3){
    r <- sample(3*n, n)
    mat2 <- mvrnorm(n,mu[,2],Sigma[,,2])
    mat3 <- mvrnorm(n,mu[,3],Sigma[,,3])
    mat4 <- rbind(mat1, mat2, mat3)
    mat1 <- mat4[r,]
    }
  }
  colnames(mat1) <- c("dim1","dim2") #just so the output is a bit easier to understand
  rownames(mat1) <- 1:n 
  return(mat1)  
}


party <- matrix(rnorm(4),ncol=2)
ll <- pdist(mat1,party)
distance <- matrix(ll@dist, ncol=2, byrow=TRUE)
distance[,1] > distance[,2]