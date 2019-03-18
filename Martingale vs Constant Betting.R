# m = inital amount of money
# c = amount betted at start
# n = max number of rounds per iteration
# I = number of iterations

#Doubling method
Martingale <-function(m,c,n,I){
	roundStats<-matrix(nrow=I,ncol=2) #Keeps track of round number and money left
	for (i in 1:I) {
		currentMoney <- m #Money available for betting
		currentBet <- c #Doubling method doubles this value after every failed bet
		roundNumber <- 0 
		for(j in 1:n){
			if(currentBet <= currentMoney){ #Stops simulation if not enough money to bet
				roundNumber <- j #Assignment to put it in statistics
				randNum <- rbinom(n=1, size=1, prob = c(0.5, 0.5))
				if(randNum == 1){ #Random number generation, 1 if wheel lands on color bet on
					currentMoney <- currentMoney + 2*c #Add to current money if won bet
				}
				else{ #Reduce bet from currentMoney and double next bet
					currentMoney = currentMoney - currentBet
					currentBet = currentBet*2	
				}
			}
		}
		#add the round number and the money from the previous round to the matrix
		roundStats[i,]<- c(roundNumber,currentMoney)		
 	}
	return(roundStats)
}

#Constant Bet method
ConstantBet <-function(m,c,n,I){
	roundStats<-matrix(nrow=I,ncol=2) #Keeps track of round number and money left
	for (i in 1:I) {
		currentMoney <- m #Money available for betting
		roundNumber <- 0
		for(j in 1:n){
			if(c <= currentMoney){
				roundNumber <- j #Assignment to put it in statistics
				randNum <- rbinom(n=1, size=1, prob = c(0.5, 0.5)) #Random number generation, 1 if wheel lands on color bet on
				if(randNum == 1){currentMoney = currentMoney + 2*c} #Add to current money if won bet
				else{currentMoney = currentMoney - c} #Reduce bet from currentMoney
			}
		}
		#add the round number and the money from the previous round to the matrix
		roundStats[i,]<- c(roundNumber,currentMoney)
 	}
	return(roundStats)
}

#Calling the functions and getting the matricies as return values
constantArray = ConstantBet(650,5,100,200)
martingaleArray = Martingale(650,5,100,200)

#Vector that contains the money left for both methods
constant_method_amount_left <- constantArray[,2]
martingale_method_amount_left <- martingaleArray[,2]

#Matrix to add values with names
constantStats<-matrix(nrow=2,ncol=5)
martingaleStats<-matrix(nrow=2,ncol=5)

#Adding names to line up with values
constantStats[1,] <- c("Mean","Median","Variance","Minimum","Maximum")
martingaleStats[1,] <- c("Mean","Median","Variance","Minimum","Maximum")

#Values for constant bet method
#Mean Meadian Variance Min Max
constantStats[2,] <- c(mean(constant_method_amount_left),median(constant_method_amount_left),var(constant_method_amount_left),min(constant_method_amount_left),max(constant_method_amount_left))

#Histogram for the money left after constant bet method
hist(constant_method_amount_left)

#Values for martingale bet method
#Mean Meadian Variance Min Max
martingaleStats[2,] <- c(mean(martingale_method_amount_left),median(martingale_method_amount_left),var(martingale_method_amount_left),min(martingale_method_amount_left),max(martingale_method_amount_left))

#Histogram for the money left after martingale bet method
hist(martingale_method_amount_left)