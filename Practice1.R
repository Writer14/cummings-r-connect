install.packages("ISLR")
library(ISLR)
library(dplyr)
help(College)
dim(College)
apps <-College$Apps
head(College)
set.seed(1200)
bootsApp <-NULL
for(j in 1:1000){
  meanApp <-apps%>%sample( size = 1000,replace = T)%>%mean()
  bootsApp <- c(bootsApp, meanApp)
  #Less modern code here
  #bootsApp<-c(bootsApp,mean(sample(apps,length(apps),replace = TRUE) ))
}
bootsApp%>%hist(main="Average Applications")
   #hist(bootsApp)

bootsApp%>%quantile(c(.1, .9))
#quantile(bootsApp,c(.010, .900))
mean(bootsApp)
q()

