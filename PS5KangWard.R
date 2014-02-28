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

#input: Vn - the number of voters.  Defaults to 100
#       Vdist - one of four values: "s" for standard normal, "n" for user specified normal, "u" for uniform, "m" for one of the multivariate options. Defaults to "s"
#       Vmeans - A vector of means for use in the "n" option.  Defaults to c(0,0)
#       Vvars - a vector of two variances for use in the "n" option.  Defaults to c(1,1).
#       Vmu - a 2 by d matrix of means for use in the multivariate normal distributions, where each column represents a vector of means for a multivariate normal distributions.  d represents the number of multivariate distributions from which preferences are drawn, it can be 1,2, or 3.  
#       VSigma - a 2 by 2 by d array, where each table is a variance-covariance matrix for a multivariate normal distribution.  d represetns the number of multivariate distributions from which preferences are drawn, it can be 1,2 or 3. 
#       Vmin - the minimum value for a uniform distribution.  Defaults to 0
#       Vmax - the maximum value for a uniform distribution.  Defaults to 1

#Output: an n by 2 matrix of voter preferences 

#Authors: Myunghoon Kang and Dalston Ward

VoterDistribution <- function(Vn=100,Vdist="s",Vmeans=c(0,0),Vvars=c(1,1),Vmu=NULL,VSigma=NULL,Vmin=0,Vmax=1){
  if(Vdist=="s"){
  mat1 <- matrix(rnorm(Vn*2),nrow=Vn)  
  } 
  if(Vdist=="n"){
  dim1 <- rnorm(Vn,mean=Vmeans[1],sd=sqrt(Vvars[1]))  #it now takes variances instead of SD's (in accord with the directions from the worksheet)
  dim2 <- rnorm(Vn,mean=Vmeans[2],sd=sqrt(Vvars[2]))
  mat1 <- cbind(dim1, dim2)
  }
  if(Vdist=="u"){
  mat1 <- matrix(runif(Vn*2,min=Vmin,max=Vmax),nrow=Vn) #The dots allow one to specify the min and max if desired.  
  }
  if(Vdist=="m"){ #I collappsed m and mm, trying to make it a bit more generalized. 
    mat1 <- mvrnorm(Vn,Vmu[,1],VSigma[,,1]) #mu (matrix) and sigma (array) are multidimensional objects now, and the function determines what to do based on the number of columns.  
    if(ncol(Vmu)==2){ #the user can now choose to have preferences be based on 1, 2 or 3 distributions.
    r <- sample(2*Vn, Vn)
    mat2 <- mvrnorm(Vn,mu[,2],VSigma[,,2])
    mat4 <- rbind(mat1, mat2)
    mat1 <- mat4[r,]
    }
    if(ncol(Vmu)==3){
    r <- sample(3*Vn, Vn)
    mat2 <- mvrnorm(Vn,Vmu[,2],VSigma[,,2])
    mat3 <- mvrnorm(Vn,Vmu[,3],VSigma[,,3])
    mat4 <- rbind(mat1, mat2, mat3)
    mat1 <- mat4[r,]
    }
  }
  colnames(mat1) <- c("dim1","dim2") #just so the output is a bit easier to understand
  rownames(mat1) <- 1:Vn 
  return(mat1)  
}

party <- matrix(rnorm(4),ncol=2) #generates random positions for each party along 2 dimensions. 

VoterAffiliate <- function(Voters, Parties){
  ll <- pdist(Voters[,1:2],Parties)
  distance <- matrix(ll@dist, ncol=2, byrow=TRUE) #calculates distance between voters and each party
  affiliation <- distance[,1] < distance[,2] # If a voter is closer to party 1 than party 2 "TRUE", "FALSE" otherwise
  #Note_DW: I changed "affil" to "affiliation", so that it's clearer throughout what exactly objects mean. 
  affiliation[affiliation==TRUE] <- 1 #records 1 (short for party 1 - this keeps the matrix numeric.) for voters who are closer to party 1 than party 2
  affiliation[affiliation==FALSE] <- 2 #records 2 (short for party 2 - again, this keeps the matric numeric) for voters who are closer to party 2 than party 1
  if(ncol(Voters)==2){
  Voters <- cbind(Voters, affiliation) #affiliation of voters are added to the voter matrix
  } else { Voters[,3] <- affiliation}
  return(Voters)
}

Visual <- function(Voters, party){
  plot(Voters[,1], Voters[,2], col=ifelse(Voters[,3]==1, "blue", "red"), 
       xlab="Dimension 1", ylab="Dimension 2") #plot position of the voters
                                               #blue for party 1, red for party 2
                                               #x-axis is dimension 1, y-axis is dimension 2
  points(party[1,1],party[1,2], col="blue", pch=19, cex=1.2) #points the position of party 1
  points(party[2,1],party[2,2], col="red", pch=19, cex=1.2) #points the position of party 2
  text(party[1,1],party[1,2],"Party 1", pos=1)  #changed the color of these to black, so that they show up better.  
  text(party[2,1],party[2,2],"Party 2", pos=1)
}
 #Visual(Voters, Parties)

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

#input: Pn - the number of parties.  Defaults to 2.
#       Pdist - the distribution used for creating preferences.  "n" for normal, "u" for uniform, and "m" for multivariate normal. Defaults to "n" 
#       Pvars - the vector of variances for use in a normal.  Should be of lenght two.  Defaults to c(1,1), which is the standard normal variance. 
#       Pmeans - the vector of means for use in normals. Should be of length two.  Defaults to c(0,0), which is the standard normal mean.
#       Pmin - the minimum, for use in a uniform distribution.  Defaults to 0.  
#       Pmax - the maximum for use in a uniform distribution.  Defaults to 1.
#       Pmu - the vector of means for use in a multivariate normal.  Should be length two.  Defaults to c(0,0)
#       PSigma - the variance-covariance matrix for use in a multivariate normal.  Should be a positive definite 2 by 2 matrix.  Defaults to cbind(c(1,0),(0,1)). 

#output: an n by 2 matrix of party ideal points.  

#Authors: Myunghoon Kang and Dalston Ward

PartyStarter <- function(Pn=2,Pdist="n",Pvars=c(1,1),Pmeans=c(0,0),Pmin=0,Pmax=1,Pmu=c(0,0),PSigma=cbind(c(1,0),c(0,1))){
  if(Pdist=="n"){
    dim1 <- rnorm(Pn,mean=Pmeans[1],sd=sqrt(Pvars[1]))
    dim2 <- rnorm(Pn,mean=Pmeans[2],sd=sqrt(Pvars[2]))
    parties <- cbind(dim1,dim2)
  }
  if(Pdist=="u"){
    parties <- matrix(runif(Pn*2,min=Pmin,max=Pmax),ncol=2)
  }
  if(Pdist=="m"){
    parties <- mvrnorm(Pn,mu=Pmu,Sigma=PSigma)
  }
  rownames(parties) <- paste("Party", 1:Pn)
  colnames(parties) <- c("dim1","dim2")
  return(parties)
}

#3. write a function that re-locates the parties according to this hueristic

#PartyRelocator - A function that changes parties' ideal points to the mean position of thier voters.

#This function lets parties update thier ideal points to the average position of all affiliated voters.  To do this, it assumes that the Voters input has the following variables in the following order: first, a vector of voters' dim 1 preferences, second, a vector of voters' dim2 preferences, and third, a vector with either ones or twos giving voters party affiliation (1 for party 1 supporters, 2 for party 2 supporters.)   The function then subsets the voters data and uses apply to calculate the column means for each type of voters.  These means are then put into the parties object and returned.    

#input: Voters - a matrix or data frame containing at least three variables: dim1 and dim2, which are numeric vectors of voter ideal points, and affiliation, which takes values of 1 for party 1 and 2 for party 2.  
#       Parties - The current party ideal point matrix.  This matrix is used to ensure that the output matrix has the same number of parties and dimensions as the current parties matrix, and the same row/column names. 

#Output: A matrix with each party's updated ideal points on both dimensions

#Author: Myunghoon Kang and Dalston Ward

PartyRelocator <- function(Voters,Parties){
  Output <- matrix(ncol=ncol(Parties),nrow=nrow(Parties),dimnames=dimnames(Parties))
  Output[1,] <- apply(Voters[Voters[,3]==1,1:2],2,mean)
  Output[2,] <- apply(Voters[Voters[,3]==2,1:2],2,mean)
  return(Output)
}


#the next like ten lines are just some code I've been using to test my functions 
Voters <- VoterDistribution()
Parties <- PartyStarter()

VPdists <- pdist(Voters,Parties)
distances <- matrix(VPdists@dist, ncol=2, byrow=TRUE,dimnames=list(rownames=1:100,colnames=c("Dist from PartyA","Dist from PartyB")))
affiliation <- ifelse(distances[,1] < distances[,2],"PartyA","PartyB")
Voters <- data.frame(dim1=Voters[,1],dim2=Voters[,2],affiliation=affiliation)

Voters <- VoterDistribution()
Parties <- PartyStarter()
Voters <- VoterAffiliate(Voters,Parties)
Parties <- PartyRelocator(Voters,Parties)
# here ends the "testing" code 

#4. Write a "master" function that runs the simulation.  This should involve:

# - Sets up the voters
# - Setus up (at random) the initial positions of the two parties
# - Iterates across the following steps: 
# -- Voters cast votes
# -- Parties re-locate
# - After a specified number of iterations, the simulation stops

#ElectoralSimulations - A function for running a simple electoral simulations

#This function creates simulated electorates and parties with preferences on a 2 dimensional policy space, then has voters choose a party to support, and finally has parties update thier positions based on thier voters.  The last two steps, voting and realignment, are then iterated by the model until the parties adopt the same positions 2 elections in a row, which constitues an electoral equilibrium.  Once this occurs the function breaks, and returning the voter preferences, party affilliation, and party preferences for the final election, as well as the number of elections. Four functions are used internally by the function: VoterDistribution, PartyStarter, VoterAffiliate, and PartyRelocator.  The user has control of the number of simulated elections, as well as the parameters and distributions from which voters are drawn.  The user can also choose whether or not to make a plot, where voters's ideal points and the paths of parties ideal points throughout the iterated "elections" are drawn.  A more specific description of the plot can be found below in the output section. 

#input: nsims - the number of simulated elections to hold.  Defaults to 1.  Values less than 1 cause the function to break return a warning.  
#       visualize - Whether or not to plot the voters and parties as the simulations occur.  Defaults to FALSE.
#       ... - arguments passed to other functions.  These are things like dist, n, mu, and Sigma, which are used in the VoterDistribution and PartyStarter functions (with P or V before the arguements, P for use in the PartyStarter function and V for the VoterDistribution function).  

#output: A list with two elements: the Voters object from the final election, containing voter preferences and party alignments, and the parties object from the final election, containing party ideal points.  Also prints a message informing the user of the number of simulated elections when an equilibrium is reached (if an equilibrium is reached within the user specified number of simulations) or the number of elections simulated without arriving at an equilibrium. 

#If visualize==TRUE, then a plot is also created.  This plot contains a single unfilled circle for each voter, which is colored blue or red depending on the voter's party affiliation.  When a voter switches party affiliation, a new circle of the other color is plotted over the original circle, resulting in a purple circle.  This indiciates a voter who has switched affiliation.  The more "bluish-purple" indicates the voter has switched from the red to the blue side more often, while the more "redish-purple" indicates the voter has switched to the red side from the blue side more.  The locations of the parties are also plotted.  Thier initial positions are plotted with a large filled circle, with the text "party X" written underneath, where X is the party number.  Subsequent party positions are represented by small filled cirlces connected by line segments.  The party position in the last iteration (either because the simluation reached equilibrium or beacuse the total of nsims iterations were ran) is represented by a large filled square, with the text "Final Position" written underneath.    

#Authors: Myunghoon Kang and Dalston Ward 

ElectoralSimulations <- function(nsims=1,visualize=FALSE,...){
  
  #don't give nsims the wrong input.  Please.  
  if(nsims < 1) {
    stop("nsims must be a numeric of at least 1!")
  }
  
  #The next two lines simply create the voter and party distributions
  Voters <- VoterDistribution(...)
  Parties <- PartyStarter(...)
  
  #The first two lines carry out the first "election".  This always happens, regardless of the nsims value. 
  Voters <- VoterAffiliate(Voters,Parties)
  PartiesNew <- PartyRelocator(Voters,Parties)
  
  #the next several lines are only used when visualizing. 
  if(visualize==TRUE){
    Visual(Voters,Parties) #make a base plot using initial voter affiliation and party preferences
    if(!nsims==1){
      #update the party positions on the plot normally if there is more than 1 election 
    points(PartiesNew[1,1],PartiesNew[1,2], col="blue", pch=20)
    points(PartiesNew[2,1],PartiesNew[2,2], col="red", pch=20)
    } else { #where there's only a single election (a single simulation), then this makes the plot have squares to represent the final position 
      points(PartiesNew[1,1],PartiesNew[1,2], col="blue", pch=15)
      points(PartiesNew[2,1],PartiesNew[2,2], col="red", pch=15)
      text(PartiesNew[1,1],PartiesNew[1,2],"Final Position", pos=1) 
      text(PartiesNew[2,1],PartiesNew[2,2],"Final Position", pos=1)
    } #regardlesss of the subsequent number of elections, use segments to connect the party positions
    segments(x0=Parties[1,1],x1=PartiesNew[1,1],y0=Parties[1,2],y1=PartiesNew[1,2],col="blue")
    segments(x0=Parties[2,1],x1=PartiesNew[2,1],y0=Parties[2,2],y1=PartiesNew[2,2],col="red")
  }
  
  #Now, we check if there is more than a single "election".  If so, it executes the required number of elections.  If not, it changes PartiesNew to Parties, and returns the Voters and Parties objects.
  
  if(nsims > 1){
  for(i in 2:nsims){ #iterates according to nsims.  I think this is the appropriate use of a for loop.
    VotersNew <- VoterAffiliate(Voters,Parties) #hold a new election, generating a new voter object
    
    if(visualize==TRUE){
    points(x=Voters[Voters[,3]!=VotersNew[,3],1],y=Voters[Voters[,3]!=VotersNew[,3],2],col=ifelse(Voters[,3]==1, "blue", "red")) #use this voter object to repaint voters who switch parties in the new elections.  These voters will now have purple circles ( because red + blue = purple.)  The more "purple-ish" a voter, the more often she has switched parties! 
    }
    
    Voters <- VotersNew #after the "election results" have been plotted, reassign the VotersNew object to Voters, getting ready for the new "election"
    
    #The parties then update based on the current "election"
    PartiesNew <- PartyRelocator(Voters,Parties)
    
    #checks to see if the the updated and previous party positions are the same.  If they are, it stops everything, and this combination of affiliations and party positions is determined to be an "equilibrium"  
    if(all(mapply(identical,Parties,PartiesNew))){  
      cat("The Parties reached equilibrium positions after", i, "elections.\n") 
      if(visualize==TRUE){
        points(PartiesNew[1,1],PartiesNew[1,2], col="blue", pch=15, cex=1.2)
        points(PartiesNew[2,1],PartiesNew[2,2], col="red", pch=15, cex=1.2)
        segments(x0=Parties[1,1],x1=PartiesNew[1,1],y0=Parties[1,2],y1=PartiesNew[1,2],col="blue")
        segments(x0=Parties[2,1],x1=PartiesNew[2,1],y0=Parties[2,2],y1=PartiesNew[2,2],col="red")
        text(PartiesNew[1,1],PartiesNew[1,2],"Final Position", pos=1) 
        text(PartiesNew[2,1],PartiesNew[2,2],"Final Position", pos=1)
      }
      Parties <- PartiesNew
      break
    }
    
    #after every "election" and "party update", if visualization is on, the parties' new positions are plotted, and connected to thier old position with a line segment.  The plotting character is a small circle, which differentiates it from the starting position (a large circle) and the last position (a square)
    if(visualize==TRUE){
      points(PartiesNew[1,1],PartiesNew[1,2], col="blue", pch=20)
      points(PartiesNew[2,1],PartiesNew[2,2], col="red", pch=20)
      segments(x0=Parties[1,1],x1=PartiesNew[1,1],y0=Parties[1,2],y1=PartiesNew[1,2],col="blue")
      segments(x0=Parties[2,1],x1=PartiesNew[2,1],y0=Parties[2,2],y1=PartiesNew[2,2],col="red")
    }
    
    #these print the "Final Position" stuff for visualization when equilibrium isn't reached by the nsims point. 
    if(i == nsims & visualize==TRUE){
        points(PartiesNew[1,1],PartiesNew[1,2], col="blue", pch=15, cex=1.2)
        points(PartiesNew[2,1],PartiesNew[2,2], col="red", pch=15, cex=1.2)
        segments(x0=Parties[1,1],x1=PartiesNew[1,1],y0=Parties[1,2],y1=PartiesNew[1,2],col="blue")
        segments(x0=Parties[2,1],x1=PartiesNew[2,1],y0=Parties[2,2],y1=PartiesNew[2,2],col="red")
        text(PartiesNew[1,1],PartiesNew[1,2],"Final Position", pos=1) 
        text(PartiesNew[2,1],PartiesNew[2,2],"Final Position", pos=1)
        cat("No equilibrium was reached after",i,"elections \n")
    }
    
    #this resets the parties object, to either be returned or used in the next "election"
    Parties <- PartiesNew 
  } #closes the for loop
  } else { Parties <- PartiesNew } #close the if loop (and add an else statement)
  return(list(Voters=Voters,Parties=Parties))
} #close the function

ElectoralSimulations(20,visualize=TRUE)

#5. You will find it helpful (and fun!!!!!) to have some visualtion of this process, but note that this will slow up the speed of your simulations considerably.  
