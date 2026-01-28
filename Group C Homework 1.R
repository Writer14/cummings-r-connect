install.packages(“ISLR”)
library(ISLR)
library(dplyr)

   #Problem 1
apps<-College$Apps
set.seed(1000)
boots<-NULL
for(i in 1:10000){
  meanapp <- apps%>%sample(size = 777,replace=TRUE)%>%mean()
}
  #mean( sample(apps, 777, replace=TRUE))
  boots <- c(boots, meanapp)  

  boots%>%quantile(c(.1,.9))
  mean(boots)
  sd(boots)
head(boots)
  
   #Problem 2
  
    #First Seed (Note: inadvertently sampled 10,000 not 1,000 times.)
set.seed(3200)
boots<-NULL
for(i in 1:10000){
    meanapp <- apps%>%sample(size = 777,replace=TRUE)%>%mean()
    #mean( sample(apps, 777, replace=TRUE))
    boots <- c(boots, meanapp)
}

#Our 80th Quantile Results
boots%>%quantile(c(.1,.9))
  
mean(apps) + 2*c(-sd(boots), sd(boots))
mean(boots)
sd(boots)

set.seed(4350)
Runtime4350<-Sys.time()
boots<-NULL
for(i in 1:5000){
  meanapp <- apps%>%sample(size = 777,replace=TRUE)%>%mean()
  boots <- c(boots, meanapp)  
}
Runtime2_4350<-Sys.time()
Runtime2_4350 - Runtime4350

set.seed(3200)

> mean(boots)

> sd(boots)

    

   #Problem 3
private<-College$Private
dim(private)
summary(College)
tapply(College$Apps,College$Private,mean)
AppData<-data.frame(Apps=College$Apps,School=sample(College$Private))
tapply(AppData$Apps, AppData$School,mean) 
newApp<-AppData$School%>%sample(size=777)
newApp
newData<-data.frame(Apps=AppData$Apps, School= newApp)
newData
tapply(newData$Apps, newData$School, mean)
newMeans <- tapply(newData$Apps, newData$School, mean)
newMeans2<-newData%>%group_by(School)%>%summarise(mean_Apps=mean(Apps))
newPrivateMean <- newMeans[1]
newPublicMean <- newMeans[2]
newPublicMean - newPrivateMean

   #Randomization
set.seed(5600)
randomResults <- NULL


   #Alternate or backup for Problem 3
   #We find the median for each group.

medians =tapply(College$Apps,College$Private,median)
dim(medians)
medians
my_stat=medians[1] - medians[2]
#We check this value for its statistical significance
my_stat


set.seed(690)
random_diff = NULL
#We sample the 777 observations of applications and find a comparative median
for(k in 1:12000){
  stirred = sample(College$Apps,dim(College)[1],replace = FALSE)
  truthdf = data.frame(AppNumber=stirred,Status=College$Private)
  Medians = tapply(truthdf$AppNumber,truth_df$Status,median)
  head(Medians)
  
  #Compare the randomized median to the first
  random_diff<-c(random_diff, Medians[1] - Medians[2])
}
hist(random_diff)

#At 80th quantile = 57.5; at 90th = 33.475
random_diff%>%quantile(c(.1,.9))
random_diff%>%quantile(c(.05,.95))


#H_0: p = 0 Private schools have the same or more
#H_A: p > 0 Public schools hv more apps than private (Better then random chance)
greaterThanOrig <- random_diff > my_stat
sum(greaterThanOrig) / 12000

#The randomization favored the NULL hypothesis.
