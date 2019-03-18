install.packages("ggplot2")
library(ggplot2)

#Epilepsy Dataset
epilepsy <-c(1,0,31,11,5,3,3,3,
2,0,30,11,3,5,3,3,
3,0,25,6,2,4,0,5,
4,0,36,8,4,4,1,4,
5,0,22,66,7,18,9,21,
6,0,29,27,5,2,8,7,
7,0,31,12,6,4,0,2,
8,0,36,52,40,20,23,12,
9,0,37,23,5,6,6,5,
10,0,28,10,14,13,6,0,
11,0,36,52,26,12,6,22,
12,0,24,33,12,6,8,5,
13,0,28,18,4,4,6,2,
14,0,36,42,7,9,12,14,
15,0,26,87,16,24,10,9,
16,0,26,50,11,0,0,5,
17,0,28,18,0,0,3,3,
18,0,31,111,37,29,28,29,
19,0,32,18,3,5,2,5,
20,0,21,20,3,0,6,7,
21,0,29,12,3,4,3,4,
22,0,21,9,3,4,3,4,
23,0,32,17,2,3,3,5,
24,0,25,28,8,12,2,8,
25,0,30,55,18,24,76,25,
26,0,40,9,2,1,2,1,
27,0,19,10,3,1,4,2,
28,0,22,47,13,15,13,12,
29,1,18,76,11,14,9,8,
30,1,32,38,8,7,9,4,
31,1,20,19,0,4,3,0,
32,1,20,10,3,6,1,3,
33,1,18,19,2,6,7,4,
34,1,24,24,4,3,1,3,
35,1,30,31,22,17,19,16,
36,1,35,14,5,4,7,4,
37,1,57,11,2,4,0,4,
38,1,20,67,3,7,7,7,
39,1,22,41,4,18,2,5,
40,1,28,7,2,1,1,0,
41,1,23,22,0,2,4,0,
42,1,40,13,5,4,0,3,
43,1,43,46,11,14,25,15,
44,1,21,36,10,5,3,8,
45,1,35,38,19,7,6,7,
46,1,25,7,1,1,2,4,
47,1,26,36,6,10,8,8,
48,1,25,11,2,1,0,0,
49,1,22,151,102,65,72,63,
50,1,32,22,4,3,2,4,
51,1,25,42,8,6,5,7,
52,1,35,32,1,3,1,5,
53,1,21,56,18,11,28,13,
54,1,41,24,6,3,4,0,
55,1,32,16,3,5,4,3,
56,1,26,22,1,23,19,8,
57,1,21,25,2,3,0,1,
58,1,36,13,0,0,0,0,
59,1,37,12,1,4,3,2)


#Formatting epilepsy dataset -- still wide
epilepsy2<-matrix(epilepsy,nrow=8,ncol=59)
epilepsy2 <- t(epilepsy2)
colnames(epilepsy2) <- c("ID","Treatment","Age",0,1,2,3,4)

#Changing epilepsy dataset to a data.frame
epilepsy3.df <- as.data.frame(epilepsy2)
epilepsy3.df

#Wide -> Long and sorting by Patient ID
epilepsyLong <- reshape(epilepsy3.df, direction = "long", varying = list(names(epilepsy3.df)[4:8]), v.names = "Seizure_Rate", idvar = c("ID","Treatment","Age"), timevar = "Every_Two_Weeks", times = 0:4)
epilepsyLong<-epilepsyLong[order(epilepsyLong$ID),]


#Changing to weekly rates
for (i in 1:nrow(epilepsyLong)) {
	if(epilepsyLong$Every_Two_Weeks[i] == 0){
		epilepsyLong$Seizure_Rate[i] = epilepsyLong$Seizure_Rate[i]/8
	}
	else{
		epilepsyLong$Seizure_Rate[i] = epilepsyLong$Seizure_Rate[i]/2
	}
}

#Removes selected value from dataset
removeOne<-function(dataName,removeNum){
	tempData <- subset(dataName, ID!=removeNum)
	return(tempData)
}

#run linear regession
runRegression<-function(dataName){
	regModel <- lm(Seizure_Rate~Age + Treatment + Every_Two_Weeks,data=dataName)
	return(regModel)
}

#Getting return values from regression to do math
regStats <- data.frame(matrix(NA,nrow=59,ncol=4))
for (i in 1:59){
	tempData <- removeOne(epilepsyLong,i)
	tempReg <- runRegression(tempData)
	print(tempReg$coefficients)
	regStats[i,] <- tempReg$coefficients
}
regStats<-regStats[rep(seq_len(nrow(regStats)), each=5),]

#Calculating predicted values and creating final dataset
predictedValues <- data.frame(matrix(NA,nrow=nrow(regStats),ncol=7))
for (i in 1:nrow(epilepsyLong)){
	#Id;Treatment;Age;Week;Actual
	predictedValues[i,1] <- epilepsyLong[i,1]
	predictedValues[i,2] <- epilepsyLong[i,2]
	predictedValues[i,3] <- epilepsyLong[i,3]
	predictedValues[i,4] <-	epilepsyLong[i,4]
	predictedValues[i,5] <- epilepsyLong[i,5]

	#Getting values to calculate predicted value
	AgeVal <- regStats[i,2]*epilepsyLong[i,3]
	TreatmentVal <- regStats[i,3]*epilepsyLong[i,2]
	WeekVal <- regStats[i,4]*epilepsyLong[i,4]

	#predicted value
	predictedValues[i,6] <- regStats[i,1] + AgeVal + TreatmentVal + WeekVal
	
	#residual^2
	predictedValues[i,7] <- (predictedValues[i,5] - predictedValues[i,6])^2
}
colnames(predictedValues) <- c("ID","Treatment","Age","Week","Actual","Predicted","ResidualSquared")

#Plotting
#Numbered points are ID
#Colors for Age
#x=Predicted;y=Actual
#0=Placebo;1=Treatment
ggplot(predictedValues, aes(Predicted,Actual)) + geom_point(aes(colour=predictedValues$Age)) + geom_text(aes(label=predictedValues$ID),size=2.3) +   scale_colour_gradientn(colours = terrain.colors(10)) +facet_grid(predictedValues$Treatment ~ .)