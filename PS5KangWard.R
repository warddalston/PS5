#################################################################################
## PS 5 Simulation Activity - Myunghoon Kang and Dalston Ward - February 2014 ###
#################################################################################

#First, clear the work space
rm(list=ls())

#Second, load necessary packages
library(MASS)
library(pdist) #don't forget to install this if necessary! 

##### Section A: Simulation Set Up

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


Voters <- VoterDistribution()

party <- matrix(rnorm(4),ncol=2) #generates random positions for each party along 2 dimensions. 

VoterAffiliate <- function(Voters, party){
  ll <- pdist(Voters,party)
  distance <- matrix(ll@dist, ncol=2, byrow=TRUE) #calculates distance between voters and each party
  affil <- distance[,1] < distance[,2] # If a voter is closer to party 1 than party 2 "TRUE", "FALSE" otherwise
  affil[affil==TRUE] <- "party1" #records party 1 for voters who are closer to party 1 than party 2
  affil[affil==FALSE] <- "party2" #records party 2 for voters who are closer to party 2 than party 1
  Voters <- cbind(Voters, affil) #affiliation of voters are added to the voter matrix
  return(Voters)
}

Voters <- VoterAffiliate(Voters, party)


Visual <- function(Voters, party){
  plot(Voters[,1], Voters[,2], col=ifelse(Voters[,3]=="party1", "blue", "red"), 
       xlab="Dimension 1", ylab="Dimension 2") #plot position of the voters
                                               #blue for party 1, red for party 2
                                               #x-axis is dimension 1, y-axis is dimension 2
  points(party[1,1],party[1,2], col="blue", pch=19, cex=1.2) #points the position of party 1
  points(party[2,1],party[2,2], col="red", pch=19, cex=1.2) #points the position of party 2
  text(party[1,1],party[1,2],"Party 1", col="blue", pos=1) 
  text(party[2,1],party[2,2],"Party 2", col="red", pos=1)
}
Visual(Voters, party)

par(mfrow=c(3,3))

for(i in 1:9){ # try plot the positions of the parties, the positions of the voters and their
               # affiliation 9 times.
  Voters <- VoterDistribution()
  party <- matrix(rnorm(4),ncol=2)
  Voters <- VoterAffiliate(Voters, party)
  Visual(Voters, party)
  }

par(mfrow=c(1,1))


#### Section B: Get Things Moving! 

#1. For each iteration t of the model, the parties will locate at the "mean" position of all voters who addiliated with them in period t-1

#2. Write a funciton that chooses the "starting" position of the parties at random

#PartyStarter - A function to generate random ideal points for parties on two dimensions

#This function allows the user to generate preferences for parties on 2 dimensions according to a number of distributions.  The user can specificy the parameters of all the distributions.  There are 3 distributions supported: the normal, the uniform, and the multivariate normal. For the normal distribution option, the user has the ability to choose difference mean and variance parameters for each distribution. The function is very similar to the VoterDistribution function above.  

#input: n - the number of parties.  Defaults to 2.
#       dist - the distribution used for creating preferences.  "n" for normal, "u" for uniform, and "m" for multivariate normal. Defaults to "n" 
#       vars - the vector of variances for use in a normal.  Should be of lenght two.  Defaults to c(1,1), which is the standard normal variance. 
#       means - the vector of means for use in normals. Should be of length two.  Defaults to c(0,0), which is the standard normal variance.
#       ... - additional arguments to be passed to other functions (ie, min, max, mu, or Sigma)

#output: an n by 2 matrix of party ideal points.  

#Authors: Myunghoon Kang and Dalston Ward

PartyStarter <- function(n=2,dist="n",vars=c(1,1),means=c(0,0),...){
  if(dist=="n"){
    dim1 <- rnorm(n,mean=means[1],sd=sqrt(vars[1]))
    dim2 <- rnorm(n,mean=means[2],sd=sqrt(vars[2]))
    parties <- cbind(dim1,dim2)
  }
  if(dist=="u"){
    parties <- matrix(runif(4,...),ncol=2)
  }
  if(dist=="m"){
    parties <- mvrnorm(2,...)
  }
  rownames(parties) <- c("PartyA","PartyB")
  colnames(parties) <- c("dim1","dim2")
  return(parties)
}

#3. write a function that re-locates the parties according to this hueristic

#PartyRelocator - A function that changes parties' ideal points to the mean position of thier voters.

#This function lets parties update thier ideal points to the average position of all affiliated voters.  To do this, it assumes that the Voters input has the following variables: affiliation, which is a vector with each voter's party affiliation, and dim1, and dim2, which are each voter's ideal points on each dimension.  The function then uses the tapply funciton to quickly subset the voters data according to affiliation and calculate the mean. Note: the parties need to be in the parties object in alphabetical order by party name for this to work right.  

#input: Voters - a data frame containing at least three variables: dim1 and dim2, which are numeric vectors of voter ideal points, and affiliation, which is a character or factor containing each voter's current party affiliation
#       Parties - The current party ideal point matrix.  This matrix is used to ensure that the output matrix has the same number of parties and dimensions as the current parties matrix, and the same row/column names. 

#Output: A matrix with each party's updated ideal points on both dimensions

#Author: Myunghoon Kang and Dalston Ward

PartyRelocator <- function(Voters,Parties){
  Parties[,1] <- tapply(Voters$dim1,affiliation,mean)
  Parties[,2] <- tapply(Voters$dim2,affiliation,mean)
  return(Parties)
}


#the next like ten lines are just some code I've been using to test my functions 
Voters <- VoterDistribution()
Parties <- PartyStarter()

VPdists <- pdist(Voters,Parties)
distances <- matrix(VPdists@dist, ncol=2, byrow=TRUE,dimnames=list(rownames=1:100,colnames=c("Dist from PartyA","Dist from PartyB")))
affiliation <- ifelse(distances[,1] < distances[,2],"PartyA","PartyB")
Voters <- data.frame(dim1=Voters[,1],dim2=Voters[,2],affiliation=affiliation)
PartyRelocator(Voters,Parties)
# here ends the "testing" code 

#4. Write a "master" function that runs the simulation.  This should involve:

# - Sets up the voters
# - Setus up (at random) the initial positions of the two parties
# - Iterates across the following steps: 
# -- Voters cast votes
# -- Parties re-locate
# - After a specified number of iterations, the simulation stops

#5. You will find it helpful (and fun!!!!!) to have some visualtion of this process, but note that this will slow up the speed of your simulations considerably.  
