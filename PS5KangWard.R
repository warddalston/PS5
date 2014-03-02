#################################################################################
## PS 5 Simulation Activity - Myunghoon Kang and Dalston Ward - February 2014 ###
#################################################################################

#First, clear the work space
rm(list=ls())

#Second, load necessary packages
library(MASS)
library(pdist) #don't forget to install this if necessary! 
library(plyr)
library(doMC)
library(multicore)
library(foreach)

##### Section A: Simulation Set Up

#Voter Distributions - A function for creating distributions of voter preferences

#This function allows the user to a create a distribution of voters with preferences on two dimensions.  The user is allowed to choose one of five distributional set-ups from which preferences are drawn.  These are standard normals, normal distributions with user specified variances, uniform distributions, a multivariate normal with a user specified vector of means and variance-covariance matrix, and finally, up to three multivariate normal distributions, where each voter is equally likely to have her preferences drawn from each distribution. All of the arguments have a "V" in front of them.  This is so as to specify that these arguements impact the voter distribution draw, as opposed to the party distribution draw.  This is important for the ElectoralSimulations function below, but is not important here.  

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

VoterAffiliate <- function(Voters, Parties){
  ll <- pdist(Voters[,1:2],Parties)
  distance <- matrix(ll@dist, ncol=nrow(Parties), byrow=TRUE) #calculates distance between voters and each party
  MinDistance <- apply(distance, 1, min) #pick out the minimum distance 
  affiliation <- distance==MinDistance  #i'th Row: voter i, i'th Columns:Partiy i, If the voter affiliates with a party, then the value is TRUE
  affiliation <- affiliation %*% c(1:nrow(Parties)) #vector indicating affiliations of voters. values are 1,2,3,...,n.
  if(ncol(Voters)==2){
      Voters <- cbind(Voters, affiliation) #affiliation of voters are added to the voter matrix
    } else {Voters[,3] <- affiliation}
    return(Voters)
  } 

Visual <- function(Voters, Parties){
  #this line set palette of colors. 1=blue, 2=red and so forth. NOTE_DW: This sets the colors for the ENTIRE R session.  Thus, it only needs to be run once, and if you want back to original colors, you need to run this: palette("default")
  palette(c("blue", "red", "yellow","forestgreen", "darkorange", "maroon", "purple4", "chocolate4","cyan","deeppink","khaki3", "lawngreen","sandybrown","plum4","wheat3")) 
  #plot the positions of the voters along two dimensions.
  plot(Voters[,1], Voters[,2], col=Voters[,3], xlab="Dimension 1", ylab="Dimension 2", 
       xlim=c(min(min(Voters[,1]),Parties[,1]),max(max(Voters[,1]),Parties[,1])),
       ylim=c(min(min(Voters[,2]),Parties[,2]),max(max(Voters[,2]),Parties[,2])))
  #Points the position for each party
  mapply(points, x=Parties[,1], y=Parties[,2], col=1:nrow(Parties), MoreArgs=list(pch=19, cex=1.2))     
  #labels the party name for each party (party 1, party 2, and so forth)                                              
  mapply(text, x=Parties[,1], y=Parties[,2], labels=paste("Party",1:nrow(Parties)), MoreArgs=list(pos=1))
}
 

#par(mfrow=c(3,3))

#for(i in 1:9){ # try plot the positions of the parties, the positions of the voters and their
 #              # affiliation 9 times.
  #Voters <- VoterDistribution()
  #Parties <- matrix(rnorm(4),ncol=2)
  #Voters <- VoterAffiliate(Voters, Parties)
  #Visual(Voters, Parties)
  #}

#par(mfrow=c(1,1))


#### Section B: Get Things Moving! 

#1. For each iteration t of the model, the parties will locate at the "mean" position of all voters who addiliated with them in period t-1

#2. Write a funciton that chooses the "starting" position of the parties at random

#PartyStarter - A function to generate random ideal points for parties on two dimensions

#This function allows the user to generate preferences for parties on 2 dimensions according to a number of distributions.  The user can specificy the parameters of all the distributions.  There are 3 distributions supported: the normal, the uniform, and the multivariate normal. For the normal distribution option, the user has the ability to choose difference mean and variance parameters for each distribution. The function is very similar to the VoterDistribution function above.  Each arguement has a "P" before it, this is to indicate that these arguments impact the party distribution parameters.  This is not important for this function, but matters for the ElectoralSimulations function, below, in which users can set the parameters of both voter and party distributions.  

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
  for(i in 1:nrow(Parties)){ 
      Output[i,] <- apply(Voters[Voters[,3]==i,1:2,drop=FALSE],2,mean)
  }
  #if a party did not get any votes from voters then the party will be eliminated from simulation by setting their positions a large number.
  #So, in the following plot, the eliminated party will be described as going outside the plot box.
  #Also, their positions will be recorded as 1.00000e+05.
  Output[!complete.cases(Output),] <- 100000000                                               
  return(Output)
}

# here ends the "testing" code 

#4. Write a "master" function that runs the simulation.  This should involve:

# - Sets up the voters
# - Setus up (at random) the initial positions of the two parties
# - Iterates across the following steps: 
# -- Voters cast votes
# -- Parties re-locate
# - After a specified number of iterations, the simulation stops

#ElectoralSimulations - A function for running a simple electoral simulations

#This function creates simulated electorates and parties with preferences on a 2 dimensional policy space, then has voters choose a party to support, and finally has parties update thier positions based on thier voters. The user sets up the party and voter preference distributions using the arguements to the PartyStarter and VoterDistribution functions. The r.seed arguement allows the user to replicate results.  These arguments, which are used to control the the number of parties/voters to draw, the distribution from which their preferences are drawn, and the parameters of the given distribution, are largely the same for both parties and voters.  The only IMPORTANT DIFFERENCE between the party and voters arguments is that the voter arguments begin with "V" and the party arguments begin with "P". Reproducible distirbutions can be created by using the r.seed arguement.  

#The last two steps, voting and realignment, are then iterated by the model until the parties adopt the same positions 2 elections in a row, which constitues an electoral equilibrium or the user defined number of simulated elections have occured.  Once this occurs the function breaks, and returns the voter preferences, party affilliation, and party preferences for the final election, the number of elections, and a vector of party positions throughout the simulation. Four functions are used internally by the function: VoterDistribution, PartyStarter, VoterAffiliate, and PartyRelocator.  The user has control of the number of simulated elections, as well as the parameters and distributions from which voters are drawn.  

#The user can also choose whether or not to make a plot, where voters's ideal points and the paths of parties ideal points throughout the iterated "elections" are drawn.  A more specific description of the plot can be found below in the output section. 

#General input: nsims - the number of simulated elections to hold.  Defaults to 1.  Values less than 1 cause the function to break return a warning.  
#               visualize - Whether or not to plot the voters and parties as the simulations occur.  Defaults to FALSE.
#               r.seed - a random seed. Defaults to be NULL

#Input to set up Voter preference distribution:
#        Vn - the number of voters.  Defaults to 100
#       Vdist - one of four values: "s" for standard normal, "n" for user specified normal, "u" for uniform, "m" for one of the multivariate options. Defaults to "s"
#       Vmeans - A vector of means for use in the "n" option.  Defaults to c(0,0)
#       Vvars - a vector of two variances for use in the "n" option.  Defaults to c(1,1).
#       Vmu - a 2 by d matrix of means for use in the multivariate normal distributions, where each column represents a vector of means for a multivariate normal distributions.  d represents the number of multivariate distributions from which preferences are drawn, it can be 1,2, or 3.  
#       VSigma - a 2 by 2 by d array, where each table is a variance-covariance matrix for a multivariate normal distribution.  d represetns the number of multivariate distributions from which preferences are drawn, it can be 1,2 or 3. 
#       Vmin - the minimum value for a uniform distribution.  Defaults to 0
#       Vmax - the maximum value for a uniform distribution.  Defaults to 1

#Input to set up the Party preference distribution:
#       Pn - the number of parties.  Defaults to 2.
#       Pdist - the distribution used for creating preferences.  "n" for normal, "u" for uniform, and "m" for multivariate normal. Defaults to "n" 
#       Pvars - the vector of variances for use in a normal.  Should be of lenght two.  Defaults to c(1,1), which is the standard normal variance. 
#       Pmeans - the vector of means for use in normals. Should be of length two.  Defaults to c(0,0), which is the standard normal mean.
#       Pmin - the minimum, for use in a uniform distribution.  Defaults to 0.  
#       Pmax - the maximum for use in a uniform distribution.  Defaults to 1.
#       Pmu - the vector of means for use in a multivariate normal.  Should be length two.  Defaults to c(0,0)
#       PSigma - the variance-covariance matrix for use in a multivariate normal.  Should be a positive definite 2 by 2 matrix.  Defaults to cbind(c(1,0),(0,1)). 

#output: A list with the following elements: 
#       PartiesHistory - the vector of positions the parties take throughout the simulation
#       Voters - a matrix of the Voter's preferences and thier party affiliation in the final electoin
#       Parties - a matrix of the Parties' final positions. 

#If visualize==TRUE, then a plot is also created.  This plot contains a single unfilled circle for each voter, which is colored blue or red depending on the voter's party affiliation.  When a voter switches party affiliation, a new circle of the other color is plotted over the original circle, resulting in a purple circle.  This indiciates a voter who has switched affiliation.  The more "bluish-purple" indicates the voter has switched from the red to the blue side more often, while the more "redish-purple" indicates the voter has switched to the red side from the blue side more.  The locations of the parties are also plotted.  Thier initial positions are plotted with a large filled circle, with the text "party X" written underneath, where X is the party number.  Subsequent party positions are represented by small filled cirlces connected by line segments.  The party position in the last iteration (either because the simluation reached equilibrium or beacuse the total of nsims iterations were ran) is represented by a large filled square, with the text "Final Position" written underneath.    

#Authors: Myunghoon Kang and Dalston Ward 

ElectoralSimulations <- function(nsims=1,visualize=FALSE, r.seed=NULL, Vn=100,Vdist="s",Vmeans=c(0,0),Vvars=c(1,1),Vmu=NULL,VSigma=NULL,Vmin=0,Vmax=1,Pn=2,Pdist="n",Pvars=c(1,1),Pmeans=c(0,0),Pmin=0,Pmax=1,Pmu=c(0,0),PSigma=cbind(c(1,0),c(0,1))){

  # set a random seed if a user gives input of a random seed. 
  if(!is.null(r.seed)) set.seed(r.seed)
  
  #don't give nsims the wrong input.  Please.  
  if(!nsims%%1==0 | nsims < 1){
    stop("nsims must be an integer of at least 1!")
  }
  
  #The next two lines simply create the voter and party distributions
  Voters <- VoterDistribution(Vn=Vn,Vdist=Vdist,Vmeans=Vmeans,Vvars=Vvars,Vmu=Vmu,VSigma=VSigma,Vmin=Vmin,Vmax=Vmax)
  Parties <- PartyStarter(Pn=Pn,Pdist=Pdist,Pvars=Pvars,Pmeans=Pmeans,Pmin=Pmin,Pmax=Pmax,Pmu=Pmu,PSigma=PSigma)
  
  #The first two lines carry out the first "election".  This always happens, regardless of the nsims value. 
  Voters <- VoterAffiliate(Voters,Parties)
  PartiesNew <- PartyRelocator(Voters,Parties)
  
  #stores the initial party position(character vector)
  PartiesHistory <- apply(Parties, 1, function(x) paste("(",x[1],", ",x[2],")", sep=""))
  
  #this records the current positions of parties (Character vector) 
  UpdateHistory <- apply(PartiesNew, 1, function(x) paste("(",x[1],", ",x[2],")", sep=""))
  PartiesHistory <- c(PartiesHistory, UpdateHistory)

  #the next several lines are only used when visualizing. 
  if(visualize==TRUE){
    Visual(Voters,Parties) #make a base plot using initial voter affiliation and party preferences
    if(!nsims==1){
      #update the party positions on the plot normally if there is more than 1 election 
      mapply(points, x=PartiesNew[,1], y=PartiesNew[,2], col=1:Pn, MoreArgs=list(pch=20, cex=1))
    } else { #where there's only a single election (a single simulation), then this makes the plot have squares to represent the final position 
      mapply(points, x=PartiesNew[,1], y=PartiesNew[,2], col=1:Pn, MoreArgs=list(pch=15, cex=1.2))
      mapply(text, x=PartiesNew[,1], y=PartiesNew[,2], MoreArgs=list(labels="Final Position", pos=1))
    } #regardlesss of the subsequent number of elections, use segments to connect the party positions
    mapply(segments, x0=Parties[,1], x1=PartiesNew[,1], y0=Parties[,2], y1=PartiesNew[,2], col=1:Pn)
  }
  
  #update this and get ready for the next election. 
  Parties <- PartiesNew
  
  #Now, we check if there is more than a single "election".  If so, it executes the required number of elections.  If not, it returns the Voters, Parties, and PartiesHistories objects.  
  if(nsims > 1){
    for(i in 2:nsims){ #iterates according to nsims.  I think this is the appropriate use of a for loop.
      VotersNew <- VoterAffiliate(Voters,Parties) #hold a new election, generating a new voter object
      
      if(visualize==TRUE){
        
        #plot new points, but only for voters that changed parties in this election!
        points(x=Voters[Voters[,3]!=VotersNew[,3],1],y=Voters[Voters[,3]!=VotersNew[,3],2],col=VotersNew[Voters[,3]!=VotersNew[,3],3])
      }
      
      #reassign the VotersNew object to Voters, getting ready for the new "election"
      Voters <- VotersNew 
      
      #The parties then update based on the current "election"
      PartiesNew <- PartyRelocator(Voters,Parties)
      
      #checks to see if the the updated and previous party positions are the same.  If they are, it stops everything, and this combination of affiliations and party positions is determined to be an "equilibrium"  
      if(all(mapply(identical,Parties,PartiesNew))){  
        cat("The Parties reached equilibrium positions after", i, "elections.\n") 
        if(visualize==TRUE){
          mapply(points, x=PartiesNew[,1], y=PartiesNew[,2], col=1:Pn, MoreArgs=list(pch=15, cex=1.2))
          mapply(segments, x0=Parties[,1], x1=PartiesNew[,1], y0=Parties[,2], y1=PartiesNew[,2], col=1:Pn)
          mapply(text, x=PartiesNew[,1], y=PartiesNew[,2], MoreArgs=list(labels="Final Position", pos=1))
        }
        
        #rename the parties object so it can be returned
        Parties <- PartiesNew
        
        #this records the current positions of parties (Character vector) 
        UpdateHistory <- apply(PartiesNew, 1, function(x) paste("(",x[1],", ",x[2],")", sep=""))
        PartiesHistory <- c(PartiesHistory, UpdateHistory)
        
        #Give the Parties History object informative names before returning them
        names(PartiesHistory) <- paste(names(PartiesHistory),"-", rep(0:i, rep(Pn,i+1)), sep="")
        
        return(list(PartiesHistory=PartiesHistory,Voters=Voters,Parties=Parties))
      }
      
      #after every "election" and "party update", if visualization is on, the parties' new positions are plotted, and connected to thier old position with a line segment.  The plotting character is a small circle, which differentiates it from the starting position (a large circle) and the last position (a square)
      if(visualize==TRUE){
       mapply(points, x=PartiesNew[,1], y=PartiesNew[,2], col=1:Pn, MoreArgs=list(pch=20, cex=1))
       mapply(segments, x0=Parties[,1], x1=PartiesNew[,1], y0=Parties[,2], y1=PartiesNew[,2], col=1:Pn)
      }
      
      #these print the "Final Position" stuff for visualization when equilibrium isn't reached by the nsims point. 
      #Also, the "Fianl Position" will be described as hollow squrare (meaning not equilibrium).
      if(i == nsims){   cat("No equilibrium was reached after",i,"elections \n")
      if(visualize==TRUE){
        mapply(points, x=PartiesNew[,1], y=PartiesNew[,2], col=1:Pn, MoreArgs=list(pch=22, cex=1.2))
        mapply(segments, x0=Parties[,1], x1=PartiesNew[,1], y0=Parties[,2], y1=PartiesNew[,2], col=1:Pn)
        mapply(text, x=PartiesNew[,1], y=PartiesNew[,2], MoreArgs=list(labels="Final Position", pos=1))
      }
      }
      
      #this resets the parties object, to either be returned or used in the next "election"
      Parties <- PartiesNew 
      
      #this records the current positions of parties (Character vector) 
      UpdateHistory <- apply(PartiesNew, 1, function(x) paste("(",x[1],", ",x[2],")", sep=""))
      PartiesHistory <- c(PartiesHistory, UpdateHistory)
      
    } #closes the for loop
    names(PartiesHistory) <- paste(names(PartiesHistory),"-", rep(0:nsims, rep(Pn,nsims+1)), sep="")
  } else {
    names(PartiesHistory) <- paste(names(PartiesHistory),"-", rep(0:nsims, rep(Pn,nsims+1)), sep="")
  } #close the if loop 
  return(list(PartiesHistory=PartiesHistory,Voters=Voters,Parties=Parties))
} #close the function

out <- ElectoralSimulations(100,visualize=TRUE,Pn=15)

#5. You will find it helpful (and fun!!!!!) to have some visualtion of this process, but note that this will slow up the speed of your simulations considerably.  

Run <- expand.grid(Vn=10:500,Vvars1=seq(1,5,.1),Vvars2=1)

registerDoMC(cores=4)


BIG <- alply(.data=Run, .margins=1, .fun=function(x){
  x <- unlist(x)
  print(ElectoralSimulations(100,Vn=x[1],Vvars=c(x[2],x[3]),Vdist="n"))
<<<<<<< HEAD
  },.parallel=TRUE)
=======
  },.parallel=TRUE)

### Section D. Expand your model
#1. Alter your model so that the number of parties is an optional input. How do your results change as a result?

par(mfrow=c(2,3))
for(i in 2:7){
    ElectoralSimulations(100, visualize=TRUE, r.seed=1801, Pn=i)
}    
par(mfrow=c(1,1))

# When there are only two parties, both partie move toward the middel point of voters.  
# However, when there are more than two parties, some parties begin to move away from the middle point of voters.

#2. Alter your model so that voters vote "probabilistically" as some function of the distance between the two parties.
# (That is, allow them to make "wrong" decision if they are nearly indifferent between the parties.) Do the results change?

#plot when voters do not make 'wrong' decision.
par(mfrow=c(2,3))
for(i in 2:4){
    ElectoralSimulations(100, visualize=TRUE, r.seed=1801, Pn=i)
}    

# adding 'probabilistical' feature to voters' affiliation function.
VoterAffiliate <- function(Voters, Parties){
  ll <- pdist(Voters[,1:2],Parties)
  distance <- matrix(ll@dist, ncol=nrow(Parties), byrow=TRUE) #calculates distance between voters and each party
  distance <- round(distance, 1) #voters cannot know the exact distance below 1 decimal point.
  MinDistance <- apply(distance, 1, min) #pick out the minimum distance 
  affiliation <- distance==MinDistance  #i'th Row: voter i, i'th Columns:Partiy i, If the voter affiliates with a party, then the value is TRUE
  both.true <- apply(affiliation, 1, function(x) which(x==TRUE)) #because of voters' imperfect information, voters may
                                                                 #have indifferent preferences over parties. 
  prob.decision <- sapply(both.true, function(x) x <- x[sample(length(x),1)]) # when voters have indifferent party preference,
                                                                              # they randomly choose. 
  for(i in 1:nrow(affiliation)){
      affiliation[i,-prob.decision[i]] <- FALSE
  }
  affiliation <- affiliation %*% c(1:nrow(Parties)) #vector indicating affiliations of voters. values are 1,2,3,...,n.
   if(ncol(Voters)==2){
      Voters <- cbind(Voters, affiliation) #affiliation of voters are added to the voter matrix
    } else {Voters[,3] <- affiliation}
    return(Voters)
} 

# plot when voters make 'wrong' decision.  
for(i in 2:4){
    ElectoralSimulations(100, visualize=TRUE, r.seed=1801, Pn=i)
}   
par(mfrow=c(1,1))

# As the plots show, there are some minor changes caused by voters' 'wrong' decision. However, the final position of
# each party does not change dramatically.

# Laver(2005) ("Policy and dynamics of political competition") explores several additional heuristics parties might use
# to choose their position. Add at least on heuristic to the model (i.e, the party heuristic chosen should be a parameter
# for each party). How does that change the behavior of the model?

# alter PartyRelocator function to reflect party types

PartyRelocator <- function(Voters,Parties,Ptype,Size){
  Output <- matrix(ncol=ncol(Parties),nrow=nrow(Parties),dimnames=dimnames(Parties))
  for(i in 1:nrow(Parties)){
    if(Ptype[i]=="a"){ #if party type is Aggregator, it executes the following
    Output[i,] <- apply(Voters[Voters[,3]==i,1:2,drop=FALSE],2,mean)
    } else{
      if(Ptype[i]=="p"){ #if party type is Predator, it executes the following
        # finds the largest party
        largest.party <- as.numeric(names(sort(Size, decreasing=TRUE)[1]))
        # check whether this party is the largest party
        is.largest <- i == largest.party
        # set direction to the largest party
        move <- c((Parties[largest.party,1]-Parties[i,1]),(Parties[largest.party,2]-Parties[i,2]))
        # transforming the above vector into unit vector
        unit.move <- move/sqrt(sum(move^2))
        
        if(is.largest==TRUE){# if this party is the largest party, don't move.
        Output[i,] <- Parties[i,]
        # if this party is not the largest party, making a unit move toward the largest party
        # Note that the unit of move is set 0.1
      } else {Output[i,] <- Parties[i,]+unit.move*0.1}
    } else {stop("invalid party type")}}
  }
#if a party did not get any votes from voters then the party will be eliminated from simulation by setting their positions a large number.
#So, in the following plot, the eliminated party will be described as going outside the plot box.
#Also, their positions will be recorded as 1.00000e+05.
Output[!complete.cases(Output),] <- 100000000                                               
return(Output)
}

# add argument "Ptype". Input must be a vector (e.g. c("a","p"))
# "a" denotes Aggregator, "p" denotes Predator.
# The first element of the input vector is for party 1, the second is for party 2, and so forth.
ElectoralSimulations <- function(nsims=1,visualize=FALSE, r.seed=NULL, Vn=100,Vdist="s",Vmeans=c(0,0),Vvars=c(1,1),Vmu=NULL,VSigma=NULL,Vmin=0,Vmax=1,Pn=2,Pdist="n",Pvars=c(1,1),Pmeans=c(0,0),Pmin=0,Pmax=1,Pmu=c(0,0),PSigma=cbind(c(1,0),c(0,1)),Ptype=c("a","p")){
  
  # set a random seed if a user gives input of a random seed. 
  if(!is.null(r.seed)) set.seed(r.seed)
  
  #don't give nsims the wrong input.  Please.  
  if(!nsims%%1==0 | nsims < 1){
    stop("nsims must be an integer of at least 1!")
  }
  
  #The next two lines simply create the voter and party distributions
  Voters <- VoterDistribution(Vn=Vn,Vdist=Vdist,Vmeans=Vmeans,Vvars=Vvars,Vmu=Vmu,VSigma=VSigma,Vmin=Vmin,Vmax=Vmax)
  Parties <- PartyStarter(Pn=Pn,Pdist=Pdist,Pvars=Pvars,Pmeans=Pmeans,Pmin=Pmin,Pmax=Pmax,Pmu=Pmu,PSigma=PSigma)
  
  #The first four lines carry out the first "election".  This always happens, regardless of the nsims value. 
  Voters <- VoterAffiliate(Voters,Parties)
  Size <- table(Voters[,3]) # calculate the current party size
  PartiesNew <- PartyRelocator(Voters,Parties,Ptype,Size)
    
  #stores the initial party position(character vector)
  PartiesHistory <- apply(Parties, 1, function(x) paste("(",x[1],", ",x[2],")", sep=""))
  
  #this records the current positions of parties (Character vector) 
  UpdateHistory <- apply(PartiesNew, 1, function(x) paste("(",x[1],", ",x[2],")", sep=""))
  PartiesHistory <- c(PartiesHistory, UpdateHistory)
  
  #the next several lines are only used when visualizing. 
  if(visualize==TRUE){
    Visual(Voters,Parties) #make a base plot using initial voter affiliation and party preferences
    if(!nsims==1){
      #update the party positions on the plot normally if there is more than 1 election 
      mapply(points, x=PartiesNew[,1], y=PartiesNew[,2], col=1:Pn, MoreArgs=list(pch=20, cex=1))
    } else { #where there's only a single election (a single simulation), then this makes the plot have squares to represent the final position 
      mapply(points, x=PartiesNew[,1], y=PartiesNew[,2], col=1:Pn, MoreArgs=list(pch=15, cex=1.2))
      mapply(text, x=PartiesNew[,1], y=PartiesNew[,2], MoreArgs=list(labels="Final Position", pos=1))
    } #regardlesss of the subsequent number of elections, use segments to connect the party positions
    mapply(segments, x0=Parties[,1], x1=PartiesNew[,1], y0=Parties[,2], y1=PartiesNew[,2], col=1:Pn)
  }
  
  #update this and get ready for the next election. 
  Parties <- PartiesNew
  
  #Now, we check if there is more than a single "election".  If so, it executes the required number of elections.  If not, it returns the Voters, Parties, and PartiesHistories objects.  
  if(nsims > 1){
    for(i in 2:nsims){ #iterates according to nsims.  I think this is the appropriate use of a for loop.
      VotersNew <- VoterAffiliate(Voters,Parties) #hold a new election, generating a new voter object
      Size <- table(VotersNew[,3]) #Recalcuate the party size under the new party position.
      if(visualize==TRUE){
        
        #plot new points, but only for voters that changed parties in this election!
        points(x=Voters[Voters[,3]!=VotersNew[,3],1],y=Voters[Voters[,3]!=VotersNew[,3],2],col=VotersNew[Voters[,3]!=VotersNew[,3],3])
      }
      
      #reassign the VotersNew object to Voters, getting ready for the new "election"
      Voters <- VotersNew 
      
      #The parties then update based on the current "election"
      PartiesNew <- PartyRelocator(Voters,Parties,Ptype,Size)
    
      #after every "election" and "party update", if visualization is on, the parties' new positions are plotted, and connected to thier old position with a line segment.  The plotting character is a small circle, which differentiates it from the starting position (a large circle) and the last position (a square)
      if(visualize==TRUE){
        mapply(points, x=PartiesNew[,1], y=PartiesNew[,2], col=1:Pn, MoreArgs=list(pch=20, cex=1))
        mapply(segments, x0=Parties[,1], x1=PartiesNew[,1], y0=Parties[,2], y1=PartiesNew[,2], col=1:Pn)
      }
      
      #these print the "Final Position" stuff for visualization when equilibrium isn't reached by the nsims point. 
      #Also, the "Fianl Position" will be described as hollow squrare (meaning not equilibrium).
      if(i == nsims){   cat("No equilibrium was reached after",i,"elections \n")
                        if(visualize==TRUE){
                          mapply(points, x=PartiesNew[,1], y=PartiesNew[,2], col=1:Pn, MoreArgs=list(pch=15, cex=1.2))
                          mapply(segments, x0=Parties[,1], x1=PartiesNew[,1], y0=Parties[,2], y1=PartiesNew[,2], col=1:Pn)
                          mapply(text, x=PartiesNew[,1], y=PartiesNew[,2], MoreArgs=list(labels="Final Position", pos=1))
                        }
      }
      
      #this resets the parties object, to either be returned or used in the next "election"
      Parties <- PartiesNew 
      
      #this records the current positions of parties (Character vector) 
      UpdateHistory <- apply(PartiesNew, 1, function(x) paste("(",x[1],", ",x[2],")", sep=""))
      PartiesHistory <- c(PartiesHistory, UpdateHistory)
      
    } #closes the for loop
    names(PartiesHistory) <- paste(names(PartiesHistory),"-", rep(0:nsims, rep(Pn,nsims+1)), sep="")
  } else {
    names(PartiesHistory) <- paste(names(PartiesHistory),"-", rep(0:nsims, rep(Pn,nsims+1)), sep="")
  } #close the if loop 
  return(list(PartiesHistory=PartiesHistory,Voters=Voters,Parties=Parties))
} #close the function

par(mfrow=c(1,3))
aggregator <- ElectoralSimulations(300, visual=TRUE, Pn=2, Ptype=c("a","a"), r.seed=1801)
aggregator.predator <- ElectoralSimulations(300, visual=TRUE, Pn=2, Ptype=c("a","p"), r.seed=1801)
predator.predator <- ElectoralSimulations(300, visual=TRUE, Pn=2, Ptype=c("p","p"), r.seed=1801)
par(mfrow=c(1,1))
table(aggregator[[2]][,3])
table(aggregator.predator[[2]][,3])
table(predator.predator[[2]][,3])
# When one party uses "Predator" heuristic and the other party uses "Aggregator" heuristic, the "Predator" party
# gets worse outcome than using "Aggregator" heuristic given other party always uses "Aggregator" heuristic.
# Interestingly, when both parties use "Predator" heuristic, their final position is the same and once their
# final positions meet each other, there is no move anymore. 


<<<<<<< HEAD
>>>>>>> FETCH_HEAD
=======
>>>>>>> FETCH_HEAD
