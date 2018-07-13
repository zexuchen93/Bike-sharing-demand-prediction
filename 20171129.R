library(ggthemes)
library(lubridate)
bike_data<-read.csv("hour.csv")
bike_data<-transform(bike_data,dteday=as.character(bike_data$dteday))
bike_data$dteday<-as.Date(bike_data$dteday)
bike_data$Day_of_Month<-mday(bike_data$dteday)
bike_data<-bike_data[,-1]
#base one day of month
bike_data$Week_of_year<-week(bike_data$dteday)
bike_data<-bike_data[,-c(14,15)]
bike_data<-bike_data[,c(1:13,15:16,14)]
#make unique day 
bike_data$day_of_2year<-0
bike_data$day_of_2year[bike_data
                       $yr==0]<-yday(bike_data$dteday[bike_data$yr==0])
bike_data$day_of_2year[bike_data
                       $yr==1]<-yday(bike_data$dteday[bike_data$yr==1])+365



rownames(bike_data)<-paste(as.character(bike_data$dteday),bike_data$hr)
bike_data<-bike_data[,-1]
bike_data$week_of_month<-bike_data$Week %% 4
bike_data$week_of_month<-ifelse(bike_data$week_of_month==0,4,
                                bike_data$week_of_month)
bike_data<-bike_data[,c(1:14,16,17,15)]
bike_data$yr[bike_data$yr==0]<-2011
bike_data$yr[bike_data$yr==1]<-2012




factor_transfom<-function(x){
  bike_data[,x]<-as.factor(bike_data[,x])
  return(bike_data)
}
#creat list that I want to transform

mylist<-c(c(1:8),c(13:14,16))
for (i in 1:length(mylist) ){
  bike_data=factor_transfom(mylist[i])
}  

bike_data$temp=bike_data$temp*41
bike_data$atemp=bike_data$atemp*50
bike_data$hum=bike_data$hum*100
bike_data$windspeed=bike_data$windspeed*67
colnames(bike_data)[c(9,10,11)]<-c("Temperature","FeelTemperature","Humidity")



#create a list of 80% of the rows in the original dataset we can use for training


set.seed(7)
library(caret)
validationIndex <- createDataPartition(bike_data$cnt, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- bike_data[-validationIndex,]
# use the remaining 80% of data to training and testing the models
trainbike <- bike_data[validationIndex,]





#Univariate plots


x<-trainbike[,c(9,10,11,12)]
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(x)[i])}



#Run algorithms using 10-fold cross-validation
trainControl <- trainControl(method="repeatedcv", number=10)
metric <- "RMSE"

trainbike1<-trainbike[,c(1:12,17)]

ggplot(trainbike1,aes(x=cnt))+geom_histogram()+ylab("Bike Counts")+theme_few()+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(hjust = 0.5))+xlab("Count of bikes")+ggtitle("Histogram of rental bikes")

set.seed(7)
library(MASS)
p_fit1_boc<-boxcox(cnt~.,data=trainbike1)

#get the parameter lambda f
#for year one
lambda1<-
  p_fit1_boc$x
Max_likelihood1<-
  p_fit1_boc$y

lambda1[which.max(Max_likelihood1)]

library(caret)


Trans1<-BoxCoxTrans(trainbike1$cnt)

#After transformation

count_of_bike_after_transform1<-predict(Trans1,trainbike1$cnt)

newbike<-cbind(trainbike1[,c(1:12)],count_of_bike_after_transform1)




my.grid <- expand.grid(.decay = c(0.5, 0.1), .size = c(5, 6, 7))

nnfit2<- train(cnt~season+yr+mnth+hr+holiday+holiday+weekday+weekday+workingday+weathersit+Humidity+windspeed+Temperature_Sense,data=trainbike1,
               method = "nnet", maxit = 1000, tuneGrid = my.grid, trace = F, linout = 1)









