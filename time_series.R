

# this script is for modeling user behaviour
#

setwd("C:/Users/Majid/Google Drive/My Phd/Proposal/dataset/amzn-anon-access-samples/Clean")
getwd()
library(caret)
library("pROC")

install.packages("tidyr")
library(xts)
library(gbm)
#install.packages("randomForest")
library(tidyr)
#install.packages("gbm3")
#install.packages('gridExtra')
library(tseries)
#####################################################################################
train <- read.csv('final_totalDay1.csv', header = TRUE)
head(train)
names(train)
selected_columns <- c(4,5,c(13:18)) #c("PERSON_COMPANY")
train_sub <-  train[, c(13:18)]
names(train_sub)


#
# selecting appropriate columns
selected_columns <- c("PERSON_ID", "resource_assigned", "resources_requested", "request_count", "days", "weight_request", "bandwidth")
#used_columns <- c( "start_date","resources_requested", "request_count", "days", "weight_request", "bandwidth")
used_columns <- c( "start_date","weight_request")

USER_ID <- 1000
train_sub <- subset(train, PERSON_ID == USER_ID)
train_sub <- train_sub[, used_columns]
head.train <- head(train_sub)
train_sub <- unique(train_sub)
###################################################Reshaping Data#################################
train_sub1 <- train_sub
train_sub1$start_date <- as.yearmon(train_sub1$start_date)

train_sub1$start_date <- sapply(train_sub1$start_date, function(x) substr(x, 0, 7)) ## Removing day from date
separate_c <- train_sub1 %>% separate(start_date, c("year", "month"), sep = "\\-") ## spiliting data to year and month
wide_DF <- separate_c %>% spread(month, weight_request) ## reshaping long data to wide data
class(wide_DF)
str(wide_DF)
#############################Converting to Time_Serries###################################################
as.yearmon(train_sub$start_date)
newTseries <- xts(wide_DF[,-1], order.by=as.yearmon(paste0(wide_DF[,1], "-01")))
attr(newTseries)
?attr()
attributes(time.train)
as.ts(time.train)
length(time.train)
newts=as.ts(time.train[250:1189]) #remove year of 2006
time.train <-  xts(train_sub[,-1], order.by=as.Date(train_sub[,1], "%Y-%m-%d"))
time.train1 <-  xts(wide_DF, order.by=as.Date(wide_DF[,1],"%Y"))

?to.weekly
weekly_time.train <- to.weekly(time.train)
monthly_time.train <- to.monthly(time.train)
weekly_time.train$mean<- apply(weekly_time.train[,2:3], 1, mean)
monthly_time.train$mean<- apply(monthly_time.train[,2:3], 1, mean)

str(weekly_time.train)
str(AirPassengers)
plot(monthly_time.train)
plot(monthly_time.train$mean)

?as.Date()
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test <- read.csv('final_test_totalDay1.csv', header = TRUE)
test_sub <- subset(test, PERSON_ID == USER_ID)
test_sub <- test_sub[, used_columns]

train_sub <- train_sub[,-3]
test_sub <- test_sub[,-3]
################################################################
time.test <-  xts(test_sub[,-1], order.by=as.Date(test_sub[,1], "%Y-%m-%d"))

monthly_time.test <- to.monthly(time.test)
monthly_time.test$mean<- apply(monthly_time.test[,2:3], 1, mean)


##########################PAsSING############################
passing <- function(x,y){
  counter <-0
  passT <-0
  
  for (i in 1:length(x))
    if (x[i]>y[i]+(sd(y)/10)) {
      counter <- counter+1
      if (x[i]> (y[i]+sd(y))) {
        passT <- passT+1
      }
      
    }
  return (passT)
}


##########################Normalization#####################################
normalization <- function(x){
  Max <-max(x)
  Min <-min(x)
  for (i in 1:length(x))
    x[i] <- (x[i]-Min)/(Max-Min)
  return(x)
}

#########################################################
#########################################################
##Modifeid ROOT MEAN SQUARED ERROR
acc.fun <- function(len=length(test_sub$weight_request),pre,test_data){
  counter <-0
  sum <-0
  for (i in 1:len)
  {
    if (test_data[i]> (pre[i]-sd(pre))) sum <- sum+(test_data[i]-pre[i])^2
    else counter <- counter+1
  }
  
  return((1- (sqrt( sum  / (len-counter))))*100)
  
}

#########################TIME SERIES#############################################
summary(newts)
class(newts)
plot(newts)
plot((diff(log(newts))))
AirPassengers
acf(diff(log(newts))) #determine of q------> 0
pacf(diff(log(newts)))#determine of p------> 18
#ar i ma (p, d , q)
?arima()
fitmodel <- arima(log(newts),c(2,1,0),seasonal = list(order =c(15,1,0),period = 15))
prediction <- predict(fitmodel, n.ahead=180)
prediction.nor <- 2.718^prediction$pred
ts.plot(newts,prediction.nor,log="y",lty=c(1,8))
plot(monthly_time.test$mean)
lines(prediction.nor)

#################################################################
def.par <- par(no.readonly = TRUE)
par(oma=c(0,0,3,0))
par(mfcol= c(5,2))
par(mar=c(4,5,1.5,1.5)+0.5)
par(cex.axis=.7)
##########################FIRST FEATURE#######################################
acc.all <- 0
accuracy <- 0
pass <- 0
counter <- 1000
while (counter<=10000) {
  USER_ID <- counter
  train_sub <- subset(train, PERSON_ID == USER_ID)
  test_sub <- subset(test, PERSON_ID == USER_ID)
  test_sub <- test_sub[, used_columns]
  train_sub <- train_sub[, used_columns]
  
  
  #########################################################
  #model <- gbm(weight_request ~ ., distribution = "gaussian", data = train_sub, bag.fraction = 0.08, n.minobsinnode = 2)
  #best.iter <- gbm.perf(model,plot.it = FALSE,method="OOB")
  #pr1 <- predict(model, test_sub, best.iter)
  #?gbm.perf()
  #t11 <- glm( weight_request~., data=train_sub)
  #pr11
  
  t1 <- train( weight_request~., data=train_sub, method = "rf" )
  
  pr1 <- predict(t1,newdata=test_sub)
  #?train()
  
  
  #t2 <- train( resources_requested~., data=train_sub, method = "knn" )
  #pr2 <- predict(t2,newdata=test_sub)
  
  #t3 <- train( request_count~., data=train_sub, method = "lm" )
  #pr3 <- predict(t3,newdata=test_sub)
  
  #par(mfrow=c(2,2))
  #grid <- matrix (c(1,1,1,1,1,1,2,2,3,3), nrow=5, ncol=2, byrow = TRUE)
  #layout(grid)
  
  #plot(test_sub$weight_request,type="l",col="red",main=USER_ID,xlab="Day",ylab="Weight of requests",lwd=1.5)
  #lines(pred,col="green",lwd=1,lty=8)
  #lines(pred+sd(pred),col="blue",lwd=.8,lty=2)
  
  
  plot(test_sub$weight_request,type="l",col="red",main=USER_ID,xlab="Day",ylab="Weight of requests",lwd=1.5)
  lines(pr1,col="green",lwd=1,lty=8)
  lines(pr1+sd(pr1),col="blue",lwd=.8,lty=2)
  
  
  
  #plot(test_sub$resources_requested,type="l",col="red",main=USER_ID,xlab= "Day",ylab="Number of requested resources", lwd=1.5)
  #lines(pr2,col="green",lwd=1,lty=8)
  #lines(pr2+sd(pr2),col="blue",lwd=.8,lty=2)
  
  
  #plot(test_sub$request_count,type="l",col="red",main=USER_ID,xlab= "Day",ylab="Count of all access requests", lwd=1.5)
  #lines(pr1,col="green",lwd=1,lty=8)
  #lines(pr1+sd(pr1),col="blue",lwd=.8,lty=2)
  
  ##########################################################
  
  
  attr(pr1, "names") <- NULL
  acc <- acc.fun (length(test_sub$weight_request),pr1, test_sub$weight_request)
  #acc <- acc.fun (length(test_sub$weight_request),normalization(pr1), normalization(test_sub$weight_request))
  
  
  #acc <- sqrt( sum ((test_sub$weight_request- pr1)^2) / length(test_sub$weight_request))
  acc.all=rbind(acc.all,acc)
  #(sum (abs(test_sub$weight_request- pr1)/test_sub$weight_request) / length(test_sub$weight_request))*100
  accuracy1 <- acc.fun (length(test_sub$weight_request),normalization(pr1), normalization(test_sub$weight_request))
  accuracy=rbind(accuracy,accuracy1)
  pass1 <- passing(test_sub$weight_request,pr1)
  pass <- rbind(pass,pass1)
  
  counter=counter+1000
  #auc(test_sub$weight_request,pr1)
  
}
#title ("Users' behavior based their on weight of requests", outer = TRUE)
#warnings()
#par(def.par)

#layout(1)
###########################################SECOND FEATURE##########################################################################################
counter <- 1000
while (counter<=10000) {
  USER_ID <- counter
  train_sub <- subset(train, PERSON_ID == USER_ID)
  test_sub <- subset(test, PERSON_ID == USER_ID)
  test_sub <- test_sub[, used_columns]
  train_sub <- train_sub[, used_columns]
  #train_sub <- train_sub[,-3]
  #test_sub <- test_sub[,-3]
  
  #########################################################
  #model <- gbm(weight_request ~ ., distribution = "gaussian", data = train_sub, bag.fraction = 0.1, n.minobsinnode = 1)
  #best.iter <- gbm.perf(model,plot.it = FALSE,method="OOB")
  #pr1 <- predict(model, test_sub, best.iter)
  
  #t11 <- glm( weight_request~., data=train_sub)
  #pr11
  
  #t1 <- train( weight_request~., data=train_sub, method = "rf" )
  #pr1 <- predict(t1,newdata=test_sub)
  #?train()
  
  
  t2 <- train( resources_requested~., data=train_sub, method = "rf" )
  pr2 <- predict(t2,newdata=test_sub)
  
  #t3 <- train( request_count~., data=train_sub, method = "lm" )
  #pr3 <- predict(t3,newdata=test_sub)
  
  #par(mfrow=c(2,2))
  #grid <- matrix (c(1,1,1,1,1,1,2,2,3,3), nrow=5, ncol=2, byrow = TRUE)
  #layout(grid)
  
  #plot(test_sub$weight_request,type="l",col="red",main=USER_ID,xlab="Day",ylab="Weight of requests",lwd=1.5)
  #lines(pred,col="green",lwd=1,lty=8)
  #lines(pred+sd(pred),col="blue",lwd=.8,lty=2)
  
  
  #plot(test_sub$weight_request,type="l",col="red",main=USER_ID,xlab="Day",ylab="Weight of requests",lwd=1.5)
  #lines(pr1,col="green",lwd=1,lty=8)
  #lines(pr1+sd(pr1),col="blue",lwd=.8,lty=2)
  
  
  
  plot(test_sub$resources_requested,type="l",col="red",main=USER_ID,xlab= "Day",ylab="Number of requested resources", lwd=1.5)
  lines(pr2,col="green",lwd=1,lty=8)
  lines(pr2+sd(pr2),col="blue",lwd=.8,lty=2)
  
  
  #plot(test_sub$request_count,type="l",col="red",main=USER_ID,xlab= "Day",ylab="Count of all access requests", lwd=1.5)
  #lines(pr1,col="green",lwd=1,lty=8)
  #lines(pr1+sd(pr1),col="blue",lwd=.8,lty=2)
  counter=counter+1000
  
}
##############################################3rd FEATURE#########################################################################################
counter <- 1000
while (counter<=10000) {
  USER_ID <- counter
  train_sub <- subset(train, PERSON_ID == USER_ID)
  test_sub <- subset(test, PERSON_ID == USER_ID)
  test_sub <- test_sub[, used_columns]
  train_sub <- train_sub[, used_columns]
  #train_sub <- train_sub[,-3]
  #test_sub <- test_sub[,-3]
  
  #########################################################
  #model <- gbm(weight_request ~ ., distribution = "gaussian", data = train_sub, bag.fraction = 0.1, n.minobsinnode = 1)
  #best.iter <- gbm.perf(model,plot.it = FALSE,method="OOB")
  #pr1 <- predict(model, test_sub, best.iter)
  
  #t11 <- glm( weight_request~., data=train_sub)
  #pr11
  
  #t1 <- train( weight_request~., data=train_sub, method = "rf" )
  #pr1 <- predict(t1,newdata=test_sub)
  #?train()
  
  
  #t2 <- train( resources_requested~., data=train_sub, method = "rf" )
  #pr2 <- predict(t2,newdata=test_sub)
  
  t3 <- train( request_count~., data=train_sub, method = "rf" )
  pr3 <- predict(t3,newdata=test_sub)
  
  #par(mfrow=c(2,2))
  #grid <- matrix (c(1,1,1,1,1,1,2,2,3,3), nrow=5, ncol=2, byrow = TRUE)
  #layout(grid)
  
  #plot(test_sub$weight_request,type="l",col="red",main=USER_ID,xlab="Day",ylab="Weight of requests",lwd=1.5)
  #lines(pred,col="green",lwd=1,lty=8)
  #lines(pred+sd(pred),col="blue",lwd=.8,lty=2)
  
  
  #plot(test_sub$weight_request,type="l",col="red",main=USER_ID,xlab="Day",ylab="Weight of requests",lwd=1.5)
  #lines(pr1,col="green",lwd=1,lty=8)
  #lines(pr1+sd(pr1),col="blue",lwd=.8,lty=2)
  
  
  
  #plot(test_sub$resources_requested,type="l",col="red",main=USER_ID,xlab= "Day",ylab="Number of requested resources", lwd=1.5)
  #lines(pr2,col="green",lwd=1,lty=8)
  #lines(pr2+sd(pr2),col="blue",lwd=.8,lty=2)
  
  
  plot(test_sub$request_count,type="l",col="red",main=USER_ID,xlab= "Day",ylab="Count of all access requests", lwd=1.5)
  lines(pr3,col="green",lwd=1,lty=8)
  lines(pr3+sd(pr3),col="blue",lwd=.8,lty=2)
  counter=counter+1000
  
}

sd(pr1)/length(test_sub$weight_request)


###################TEST OTHER MODELS############################################
model <- gbm(weight_request ~ ., distribution = "gaussian", data = train_sub, bag.fraction = 0.1, n.minobsinnode = 1)
model
plot(model)
#model2 <- randomForest(x = test[,-18], y = test$bandwidth)
#?predict
#?gbm
dim(test_sub)
pred <- predict(model, data = test_sub, n.trees = 100)
pred
best.iter <- gbm.perf(model,method="OOB")
pred <- predict(model, test_sub, best.iter)

length(test_sub$bandwidth)
length(pred)
sqrt( sum ((pred - test_sub$bandwidth)^2) / length(test_sub$bandwidth))

head(test_sub)
test[1,]

#install.packages("ranger")
#library(ranger)
#library(randomForest)
#?randomForest()
rf <- randomForest(weight_request~., data=train_sub)
rf_pre <- predict(rf,test_sub)
#test_sub$weight_request
plot(test_sub$weight_request,type="l",col="red")
lines(rf_pre,col="green")


train_sub     <- train_sub[complete.cases(train_sub), ]

#?train



#?ranger
rang_weight_request <- ranger(weight_request ~ ., data = train_sub)
pred_weight_request <- predict(rang_weight_request, data = test_sub)





sqrt( sum ((pred$predictions - test_sub$weight_request)^2) / length(test_sub$weight_request))
plot(pred_weight_request,type="l",col="red")
lines(test_sub$weight_request,col="green")


rang_request_count <- ranger(request_count ~ ., data = train_sub)
pred_request_count <- predict(rang_request_count, data = test_sub)

plot(test_sub$request_count,type="l",col="red")
lines(pred_request_count$predictions,col="green")

