setwd("C:/Users/Majid/Google Drive/My Phd/Proposal/dataset/amzn-anon-access-samples/Clean")
getwd()

#install.packages("dplyr")
#install.packages("cluster")
#install.packages("shape")
library("cluster")
library("shape")
library("dplyr")


train <- read.csv('clean.csv', header = TRUE)
data <- train[,-1]  
#data <- data[,7782:7793]
str(data)


history <- read.csv('history.csv', header = TRUE)
history$day <- sapply(history$REQUEST_DATE, function(x) substr(x, 0, 10))

#length(unique(history$LOGIN))

users <- intersect(history$LOGIN, data$PERSON_ID)

activeData=subset(data, PERSON_ID %in% users)

activeDataWithoutId=activeData[,c(-7786,-7789)]
length(unique(activeDataWithoutId$PERSON_LOCATION))
#head(activeData[,c(1,2,3,4,6,7,8,9,10)])

head(activeDataWithoutId[,7782:7791])
set.seed(2500)
kmean<-kmeans(activeDataWithoutId[,7782:7791],20 ,iter.max = 50000,nstart = 10)
kmean
clusplot(activeDataWithoutId[,7782:7791],kmean$cluster,line=0,shade = TRUE,color = TRUE,labels = 4 ,plotchar = TRUE,span = TRUE,main = paste('Clusters of the users'))

activeData$cluster <- as.factor(kmean$cluster)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newset <- subset(history, LOGIN %in% 76704)


clusteredactiveData=activeData
subset1 <- subset(history, LOGIN %in% (subset(activeData, cluster==1,PERSON_ID))$PERSON_ID)
subset1$LOGIN=as.integer(1000)
#clusteredset1 <-activeData$cluster==1
clusteredactiveData$PERSON_ID[activeData$cluster==1]=as.integer(1000)
#head(clusteredactiveData)
subset2 <- subset(history, LOGIN %in% (subset(activeData, cluster==2,PERSON_ID))$PERSON_ID)
subset2$LOGIN=as.integer(2000)
clusteredactiveData$PERSON_ID[activeData$cluster==2]=as.integer(2000)

subset3 <- subset(history, LOGIN %in% (subset(activeData, cluster==3,PERSON_ID))$PERSON_ID)
subset3$LOGIN=as.integer(3000)
clusteredactiveData$PERSON_ID[activeData$cluster==3]=as.integer(3000)

subset4 <- subset(history, LOGIN %in% (subset(activeData, cluster==4,PERSON_ID))$PERSON_ID)
subset4$LOGIN=as.integer(4000)
clusteredactiveData$PERSON_ID[activeData$cluster==4]=as.integer(4000)

subset5 <- subset(history, LOGIN %in% (subset(activeData, cluster==5,PERSON_ID))$PERSON_ID)
subset5$LOGIN=as.integer(5000)
clusteredactiveData$PERSON_ID[activeData$cluster==5]=as.integer(5000)

subset6 <- subset(history, LOGIN %in% (subset(activeData, cluster==6,PERSON_ID))$PERSON_ID)
subset6$LOGIN=as.integer(6000)
clusteredactiveData$PERSON_ID[activeData$cluster==6]=as.integer(6000)

subset7 <- subset(history, LOGIN %in% (subset(activeData, cluster==7,PERSON_ID))$PERSON_ID)
subset7$LOGIN=as.integer(7000)
clusteredactiveData$PERSON_ID[activeData$cluster==7]=as.integer(7000)

subset8 <- subset(history, LOGIN %in% (subset(activeData, cluster==8,PERSON_ID))$PERSON_ID)
subset8$LOGIN=as.integer(8000)
clusteredactiveData$PERSON_ID[activeData$cluster==8]=as.integer(8000)

subset9 <- subset(history, LOGIN %in% (subset(activeData, cluster==9,PERSON_ID))$PERSON_ID)
subset9$LOGIN=as.integer(9000)
clusteredactiveData$PERSON_ID[activeData$cluster==9]=as.integer(9000)

subset10 <- subset(history, LOGIN %in% (subset(activeData, cluster==10,PERSON_ID))$PERSON_ID)
subset10$LOGIN=as.integer(10000)
clusteredactiveData$PERSON_ID[activeData$cluster==10]=as.integer(10000)

subset11 <- subset(history, LOGIN %in% (subset(activeData, cluster==11,PERSON_ID))$PERSON_ID)
subset11$LOGIN=as.integer(11000)
clusteredactiveData$PERSON_ID[activeData$cluster==11]=as.integer(11000)

subset12 <- subset(history, LOGIN %in% (subset(activeData, cluster==12,PERSON_ID))$PERSON_ID)
subset12$LOGIN=as.integer(12000)
clusteredactiveData$PERSON_ID[activeData$cluster==12]=as.integer(12000)

subset13 <- subset(history, LOGIN %in% (subset(activeData, cluster==13,PERSON_ID))$PERSON_ID)
subset13$LOGIN=as.integer(13000)
clusteredactiveData$PERSON_ID[activeData$cluster==13]=as.integer(13000)

subset14 <- subset(history, LOGIN %in% (subset(activeData, cluster==14,PERSON_ID))$PERSON_ID)
subset14$LOGIN=as.integer(14000)
clusteredactiveData$PERSON_ID[activeData$cluster==14]=as.integer(14000)

subset15 <- subset(history, LOGIN %in% (subset(activeData, cluster==15,PERSON_ID))$PERSON_ID)
subset15$LOGIN=as.integer(15000)
clusteredactiveData$PERSON_ID[activeData$cluster==15]=as.integer(15000)

subset16 <- subset(history, LOGIN %in% (subset(activeData, cluster==16,PERSON_ID))$PERSON_ID)
subset16$LOGIN=as.integer(16000)
clusteredactiveData$PERSON_ID[activeData$cluster==16]=as.integer(16000)

subset17 <- subset(history, LOGIN %in% (subset(activeData, cluster==17,PERSON_ID))$PERSON_ID)
subset17$LOGIN=as.integer(17000)
clusteredactiveData$PERSON_ID[activeData$cluster==17]=as.integer(17000)

subset18 <- subset(history, LOGIN %in% (subset(activeData, cluster==18,PERSON_ID))$PERSON_ID)
subset18$LOGIN=as.integer(18000)
clusteredactiveData$PERSON_ID[activeData$cluster==18]=as.integer(18000)

subset19 <- subset(history, LOGIN %in% (subset(activeData, cluster==19,PERSON_ID))$PERSON_ID)
subset19$LOGIN=as.integer(19000)
clusteredactiveData$PERSON_ID[activeData$cluster==19]=as.integer(19000)

subset20 <- subset(history, LOGIN %in% (subset(activeData, cluster==20,PERSON_ID))$PERSON_ID)
subset20$LOGIN=as.integer(20000)
clusteredactiveData$PERSON_ID[activeData$cluster==20]=as.integer(20000)

str(clusteredHistory)
str(clusteredactiveData)
clusteredHistory=rbind(subset1,subset2)
clusteredHistory=rbind(clusteredHistory,subset3)
clusteredHistory=rbind(clusteredHistory,subset4)
clusteredHistory=rbind(clusteredHistory,subset5)
clusteredHistory=rbind(clusteredHistory,subset6)
clusteredHistory=rbind(clusteredHistory,subset7)
clusteredHistory=rbind(clusteredHistory,subset8)
clusteredHistory=rbind(clusteredHistory,subset9)
clusteredHistory=rbind(clusteredHistory,subset10)
clusteredHistory=rbind(clusteredHistory,subset11)
clusteredHistory=rbind(clusteredHistory,subset12)
clusteredHistory=rbind(clusteredHistory,subset13)
clusteredHistory=rbind(clusteredHistory,subset14)
clusteredHistory=rbind(clusteredHistory,subset15)
clusteredHistory=rbind(clusteredHistory,subset16)
clusteredHistory=rbind(clusteredHistory,subset17)
clusteredHistory=rbind(clusteredHistory,subset18)
clusteredHistory=rbind(clusteredHistory,subset19)
clusteredHistory=rbind(clusteredHistory,subset20)

dd=clusteredHistory[with(clusteredHistory, order(REQUEST_DATE)), ]

#
# Select data in the given date period
#
Data_Selector_Period <- function(data, history_data, start_date, end_date) {
  
  # Selecting for the specified period
  history_data_period <- subset(history_data, day >= start_date & day <= end_date)
  #max(history_data$day)
  #min(history_data$day)
  #unique_date <- unique(history_data_period$day)
  days <- length (unique(history_data_period$day) )
  #length(unique(history_data_period$LOGIN))
  
  history_data_sub <- history_data_period[, c('LOGIN', 'TARGET_NAME', 'day')]
  #head(history_data_sub)
  colnames(history_data_sub)[1] <- 'PERSON_ID'
  #names(history_data_sub)
  
  attrs <- tail(names(data), n = 12)
  data_sub <- data[, attrs]
  #head(data_sub)
  
  
  # Building attributes for the dataset
  # 1) Creating the attribute: 'Number of assigned resources'
  cols_to_remove <- c("PERSON_BUSINESS_TITLE", "PERSON_BUSINESS_TITLE_DETAIL", 
                      "PERSON_COMPANY", "PERSON_DEPTNAME", "PERSON_ID", "PERSON_JOB_CODE", 
                      "PERSON_JOB_FAMILY", "PERSON_LOCATION", "PERSON_MGR_ID", "PERSON_ROLLUP_1", "PERSON_ROLLUP_2", "PERSON_ROLLUP_3", "cluster")
  
  #temp_data <- data[, !(names(data) %in% cols_to_remove)]
  #tail(names(temp_data))
  #sum(temp_data[nrow(temp_data),])
  num_res <- rowSums(  data[, !(names(data) %in% cols_to_remove)] )
  data_sub$resource_assigned <- num_res
  #head(data_sub)
  
  # 2) Creating the attribute: 'Number of resources requested' by each user
  resources_requested <- history_data_sub %>% group_by(PERSON_ID, TARGET_NAME)  %>% summarise(
    count = n()
  )
  #head(resources_requested, n = 20)
  
  resources_requested_num <- resources_requested %>% group_by(PERSON_ID)  %>% summarise(
    resources_requested = n()
  )
  #head(resources_requested_num, n = 20)
  
  data_sub <- merge(data_sub, resources_requested_num, by='PERSON_ID', all.x = TRUE, sort=TRUE)
  #head(data_sub)
  
  # 3) Creating the attribute: 'Number of requests by the user'
  requested_num <- resources_requested %>% group_by(PERSON_ID)  %>% summarise(
    request_count = sum(count)
  )
  #head(requested_num, n = 20)
  
  data_sub <- merge(data_sub, requested_num, by='PERSON_ID', sort=TRUE) # all.x = F, 
  #head(data_sub)
  #dim(data_sub)
  #length(unique(data_sub$PERSON_ID))
  
  data_sub$days <- days
  data_sub$start_date <- start_date
  
  return(data_sub)
  
}


## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Selecting data for specific start and end date
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




intervals <- data.frame(ID = 1, start = "2005-04-01", end = "2005-05-31")
intervals <- rbind(intervals, data.frame(ID = 2, start = "2005-06-01", end = "2005-08-31"))
intervals <- rbind(intervals, data.frame(ID = 3, start = "2005-09-01", end = "2005-10-31"))
intervals <- rbind(intervals, data.frame(ID = 4, start = "2005-11-01", end = "2005-12-31"))
intervals <- rbind(intervals, data.frame(ID = 5, start = "2006-01-01", end = "2006-02-31"))
intervals <- rbind(intervals, data.frame(ID = 6, start = "2006-03-01", end = "2006-04-31"))
intervals <- rbind(intervals, data.frame(ID = 7, start = "2006-05-01", end = "2006-06-31"))
intervals <- rbind(intervals, data.frame(ID = 8, start = "2006-07-01", end = "2006-08-31"))
intervals <- rbind(intervals, data.frame(ID = 9, start = "2006-09-01", end = "2006-10-15"))
intervals <- rbind(intervals, data.frame(ID = 10, start = "2006-11-01", end = "2006-12-31"))
intervals <- rbind(intervals, data.frame(ID = 11, start = "2007-01-01", end = "2007-02-31"))
intervals <- rbind(intervals, data.frame(ID = 12, start = "2007-03-01", end = "2007-04-31"))
intervals <- rbind(intervals, data.frame(ID = 13, start = "2007-05-01", end = "2007-06-31"))
intervals <- rbind(intervals, data.frame(ID = 14, start = "2007-07-01", end = "2007-08-31"))
intervals <- rbind(intervals, data.frame(ID = 15, start = "2007-09-01", end = "2007-10-31"))
intervals <- rbind(intervals, data.frame(ID = 16, start = "2007-11-01", end = "2007-12-31"))
intervals <- rbind(intervals, data.frame(ID = 17, start = "2008-01-01", end = "2008-01-31"))
intervals <- rbind(intervals, data.frame(ID = 18, start = "2008-02-01", end = "2008-02-31"))
intervals <- rbind(intervals, data.frame(ID = 19, start = "2008-03-01", end = "2008-03-31"))
intervals <- rbind(intervals, data.frame(ID = 20, start = "2008-04-01", end = "2008-04-31"))
intervals <- rbind(intervals, data.frame(ID = 21, start = "2008-05-01", end = "2008-05-31"))
intervals <- rbind(intervals, data.frame(ID = 22, start = "2008-06-01", end = "2008-06-31"))
intervals <- rbind(intervals, data.frame(ID = 23, start = "2008-07-01", end = "2008-07-31"))
intervals <- rbind(intervals, data.frame(ID = 24, start = "2008-08-01", end = "2008-08-31"))
intervals <- rbind(intervals, data.frame(ID = 25, start = "2008-09-01", end = "2008-09-31"))
intervals <- rbind(intervals, data.frame(ID = 26, start = "2008-10-01", end = "2008-10-31"))
intervals <- rbind(intervals, data.frame(ID = 27, start = "2008-11-01", end = "2008-11-31"))
intervals <- rbind(intervals, data.frame(ID = 28, start = "2008-12-01", end = "2008-12-31"))
intervals <- rbind(intervals, data.frame(ID = 29, start = "2009-01-01", end = "2009-01-31"))
intervals <- rbind(intervals, data.frame(ID = 30, start = "2009-02-01", end = "2009-02-15"))
intervals <- rbind(intervals, data.frame(ID = 31, start = "2009-02-16", end = "2009-02-31"))
intervals <- rbind(intervals, data.frame(ID = 32, start = "2009-03-01", end = "2009-03-15"))
intervals <- rbind(intervals, data.frame(ID = 33, start = "2009-03-16", end = "2009-03-31"))
intervals <- rbind(intervals, data.frame(ID = 34, start = "2009-04-01", end = "2009-04-15"))
intervals <- rbind(intervals, data.frame(ID = 35, start = "2009-04-16", end = "2009-04-31"))
intervals <- rbind(intervals, data.frame(ID = 36, start = "2009-05-01", end = "2009-05-15"))
intervals <- rbind(intervals, data.frame(ID = 37, start = "2009-05-16", end = "2009-05-31"))
intervals <- rbind(intervals, data.frame(ID = 38, start = "2009-06-01", end = "2009-06-15"))
intervals <- rbind(intervals, data.frame(ID = 39, start = "2009-06-16", end = "2009-06-31"))
intervals <- rbind(intervals, data.frame(ID = 40, start = "2009-07-01", end = "2009-07-15"))
intervals <- rbind(intervals, data.frame(ID = 41, start = "2009-07-16", end = "2009-07-31"))
intervals <- rbind(intervals, data.frame(ID = 42, start = "2009-08-01", end = "2009-08-15"))
intervals <- rbind(intervals, data.frame(ID = 43, start = "2009-08-16", end = "2009-08-31"))
intervals <- rbind(intervals, data.frame(ID = 44, start = "2009-09-01", end = "2009-09-15"))
intervals <- rbind(intervals, data.frame(ID = 45, start = "2009-09-16", end = "2009-09-31"))
intervals <- rbind(intervals, data.frame(ID = 46, start = "2009-10-01", end = "2009-10-15"))
intervals <- rbind(intervals, data.frame(ID = 47, start = "2009-10-16", end = "2009-10-31"))
intervals <- rbind(intervals, data.frame(ID = 48, start = "2009-11-01", end = "2009-11-15"))
intervals <- rbind(intervals, data.frame(ID = 49, start = "2009-11-16", end = "2009-11-31"))
intervals <- rbind(intervals, data.frame(ID = 50, start = "2009-12-01", end = "2009-12-15"))
intervals <- rbind(intervals, data.frame(ID = 51, start = "2009-12-16", end = "2009-12-31"))
intervals <- rbind(intervals, data.frame(ID = 52, start = "2010-01-01", end = "2010-01-15"))
intervals <- rbind(intervals, data.frame(ID = 53, start = "2010-01-16", end = "2010-01-31"))
intervals <- rbind(intervals, data.frame(ID = 54, start = "2010-02-01", end = "2010-02-15"))
intervals <- rbind(intervals, data.frame(ID = 55, start = "2010-02-16", end = "2010-02-31"))
intervals <- rbind(intervals, data.frame(ID = 56, start = "2010-03-01", end = "2010-03-15"))
intervals <- rbind(intervals, data.frame(ID = 57, start = "2010-03-16", end = "2010-03-31"))
intervals <- rbind(intervals, data.frame(ID = 58, start = "2010-04-01", end = "2010-04-15"))
intervals <- rbind(intervals, data.frame(ID = 59, start = "2010-04-16", end = "2010-04-31"))
intervals <- rbind(intervals, data.frame(ID = 60, start = "2010-05-01", end = "2010-05-31"))
intervals <- rbind(intervals, data.frame(ID = 61, start = "2010-05-16", end = "2010-05-31"))
intervals <- rbind(intervals, data.frame(ID = 62, start = "2010-06-01", end = "2010-06-31"))
intervals <- rbind(intervals, data.frame(ID = 63, start = "2010-06-16", end = "2010-06-31"))
intervals <- rbind(intervals, data.frame(ID = 64, start = "2010-07-01", end = "2010-07-31"))
intervals <- rbind(intervals, data.frame(ID = 65, start = "2010-07-16", end = "2010-07-31"))
intervals <- rbind(intervals, data.frame(ID = 66, start = "2010-08-01", end = "2010-08-15"))
intervals <- rbind(intervals, data.frame(ID = 67, start = "2010-08-16", end = "2010-08-31"))
intervals <- rbind(intervals, data.frame(ID = 68, start = "2010-09-01", end = "2010-09-15"))
intervals <- rbind(intervals, data.frame(ID = 69, start = "2010-09-16", end = "2010-09-31"))
intervals <- rbind(intervals, data.frame(ID = 70, start = "2010-10-01", end = "2010-10-15"))
intervals <- rbind(intervals, data.frame(ID = 71, start = "2010-10-16", end = "2010-10-31"))



# periods <- list()
# users <- list()
# for (i in c(1:nrow(intervals)) ) {
#   tmp <- Data_Selector_Period(data, history_data, as.character(intervals[i,2]), as.character(intervals[i,3]) )
#   periods[[i]] <- append(periods, tmp)
#   users[[i]] <- append(users, unique(tmp$PERSON_ID))
# }
# 
# length(periods)
# tt <- users[[1]]
# for (i in c(2:nrow(intervals)) ) {
#   tt <- intersect(tt, users[[i]])
# }


############################################################################################
sub1 <- subset(activeData, cluster==1)[,1:7781]
colnum1 <- dim(sub1[, colSums(sub1 != 0) > 0])[2]

sub1 <- subset(activeData, cluster==2)[,1:7781]
colnum2 <- dim(sub1[, colSums(sub1 != 0) > 0])[2]

sub1 <- subset(activeData, cluster==3)[,1:7781]
colnum3 <- dim(sub1[, colSums(sub1 != 0) > 0])[2]

sub1 <- subset(activeData, cluster==4)[,1:7781]
colnum4 <- dim(sub1[, colSums(sub1 != 0) > 0])[2]

sub1 <- subset(activeData, cluster==5)[,1:7781]
colnum5 <- dim(sub1[, colSums(sub1 != 0) > 0])[2]

sub1 <- subset(activeData, cluster==6)[,1:7781]
colnum6 <- dim(sub1[, colSums(sub1 != 0) > 0])[2]

sub1 <- subset(activeData, cluster==7)[,1:7781]
colnum7 <- dim(sub1[, colSums(sub1 != 0) > 0])[2]

sub1 <- subset(activeData, cluster==8)[,1:7781]
colnum8 <- dim(sub1[, colSums(sub1 != 0) > 0])[2]

sub1 <- subset(activeData, cluster==9)[,1:7781]
colnum9 <- dim(sub1[, colSums(sub1 != 0) > 0])[2]

sub1 <- subset(activeData, cluster==10)[,1:7781]
colnum10 <- dim(sub1[, colSums(sub1 != 0) > 0])[2]

sub1 <- subset(activeData, cluster==11)[,1:7781]
colnum11 <- dim(sub1[, colSums(sub1 != 0) > 0])[2]

sub1 <- subset(activeData, cluster==12)[,1:7781]
colnum12 <- dim(sub1[, colSums(sub1 != 0) > 0])[2]

sub1 <- subset(activeData, cluster==13)[,1:7781]
colnum13 <- dim(sub1[, colSums(sub1 != 0) > 0])[2]

sub1 <- subset(activeData, cluster==14)[,1:7781]
colnum14 <- dim(sub1[, colSums(sub1 != 0) > 0])[2]

sub1 <- subset(activeData, cluster==15)[,1:7781]
colnum15 <- dim(sub1[, colSums(sub1 != 0) > 0])[2]

sub1 <- subset(activeData, cluster==16)[,1:7781]
colnum16 <- dim(sub1[, colSums(sub1 != 0) > 0])[2]


sub1 <- subset(activeData, cluster==17)[,1:7781]
colnum17 <- dim(sub1[, colSums(sub1 != 0) > 0])[2]

sub1 <- subset(activeData, cluster==18)[,1:7781]
colnum18 <- dim(sub1[, colSums(sub1 != 0) > 0])[2]

sub1 <- subset(activeData, cluster==19)[,1:7781]
colnum19 <- dim(sub1[, colSums(sub1 != 0) > 0])[2]

sub1 <- subset(activeData, cluster==20)[,1:7781]
colnum20 <- dim(sub1[, colSums(sub1 != 0) > 0])[2]

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
a <- 1
b <- 4
k <- 1
#j <- 31
paste (2004+k,a,b+1,sep="-")
totalDay <- NULL
for (k in 1:4){
  for (i in 1:12){
    for(j in 1:30)
    {
      tryCatch({
        
      #if (i==2 && j>28) 
      #{i <- 3
      #j <- 1}
      day=Data_Selector_Period(clusteredactiveData, clusteredHistory, paste(2005+k,if(i<10) paste0(0,i)else i,if(j<10) paste0(0,j) else j,sep="-"), paste (2005+k,if(i<10) paste0(0,i)else i,if(j<10) paste0(0,j) else j,sep="-"))
      totalDay=rbind(totalDay,day[!duplicated(day$PERSON_ID),]) 
      }, error=function(e){cat("ERROR :",paste (2005+k,i,j,sep="-"), "\n")})
    }
  }
}  
day=day[!duplicated(day$PERSON_ID),]

totalDay$resource_assigned[totalDay$cluster==1] <- colnum1

#j <- 31
test_totalDay <- NULL
for (i in 1:8){
 # for(j in 1:30)
  {
    tryCatch({
    #if (i==2 && j==29) 
    #{i <- 3
    #j <- 1}
    day=Data_Selector_Period(clusteredactiveData, clusteredHistory, paste(2010,if(i<10) paste0(0,i)else i,if(j<10) paste0(0,j) else j,sep="-"), paste (2010,if(i<10) paste0(0,i)else i,if(j<10) paste0(0,j) else j,sep="-"))
    test_totalDay=rbind(test_totalDay,day[!duplicated(day$PERSON_ID),]) 
    }, error=function(e){cat("ERROR :",paste (2010,i,j,sep="-"), "\n")})
  }
}




period1 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[1,2]), as.character(intervals[1,3]))
head(period1)
users1 <- unique(period1$PERSON_ID)
period1=period1[!duplicated(period1$PERSON_ID),]


period2 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[2,2]), as.character(intervals[2,3]))
users2 <- unique(period2$PERSON_ID)
period2=period2[!duplicated(period2$PERSON_ID),]

period3 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[3,2]), as.character(intervals[3,3]))
users3 <- unique(period3$PERSON_ID)
period3=period3[!duplicated(period3$PERSON_ID),]

period4 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[4,2]), as.character(intervals[4,3]))
users4 <- unique(period4$PERSON_ID)
period4=period4[!duplicated(period4$PERSON_ID),]

period5 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[5,2]), as.character(intervals[5,3]))
users5 <- unique(period5$PERSON_ID)
period5=period5[!duplicated(period5$PERSON_ID),]

period6 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[6,2]), as.character(intervals[6,3]))
users6 <- unique(period6$PERSON_ID)
period6=period6[!duplicated(period6$PERSON_ID),]

period7 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[7,2]), as.character(intervals[7,3]))
users7 <- unique(period7$PERSON_ID)
period7=period7[!duplicated(period7$PERSON_ID),]

period8 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[8,2]), as.character(intervals[8,3]))
users8 <- unique(period8$PERSON_ID)
period8=period8[!duplicated(period8$PERSON_ID),]

period9 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[9,2]), as.character(intervals[9,3]))
users9 <- unique(period9$PERSON_ID)
period9=period9[!duplicated(period9$PERSON_ID),]

period10 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[10,2]), as.character(intervals[10,3]))
users10 <- unique(period10$PERSON_ID)
period10=period10[!duplicated(period10$PERSON_ID),]

period11 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[11,2]), as.character(intervals[11,3]))
users11 <- unique(period11$PERSON_ID)
period11=period11[!duplicated(period11$PERSON_ID),]

period12 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[12,2]), as.character(intervals[12,3]))
users12 <- unique(period12$PERSON_ID)
period12=period12[!duplicated(period12$PERSON_ID),]

period13 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[13,2]), as.character(intervals[13,3]))
users13 <- unique(period13$PERSON_ID)
period13=period13[!duplicated(period13$PERSON_ID),]

period14 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[14,2]), as.character(intervals[14,3]))
users14 <- unique(period14$PERSON_ID)
period14=period14[!duplicated(period14$PERSON_ID),]

period15 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[15,2]), as.character(intervals[15,3]))
users15 <- unique(period15$PERSON_ID)
period15=period15[!duplicated(period15$PERSON_ID),]

period16 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[16,2]), as.character(intervals[16,3]))
users16 <- unique(period16$PERSON_ID)
period16=period16[!duplicated(period16$PERSON_ID),]

period17 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[17,2]), as.character(intervals[17,3]))
users17 <- unique(period17$PERSON_ID)
period17=period17[!duplicated(period17$PERSON_ID),]

period18 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[18,2]), as.character(intervals[18,3]))
users18 <- unique(period18$PERSON_ID)
period18=period18[!duplicated(period18$PERSON_ID),]

period19 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[19,2]), as.character(intervals[19,3]))
users19 <- unique(period19$PERSON_ID)
period19=period19[!duplicated(period19$PERSON_ID),]

period20 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[20,2]), as.character(intervals[20,3]))
users20 <- unique(period20$PERSON_ID)
period20=period20[!duplicated(period20$PERSON_ID),]

period21 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[21,2]), as.character(intervals[21,3]))
users21 <- unique(period21$PERSON_ID)
period21=period21[!duplicated(period21$PERSON_ID),]

period22 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[22,2]), as.character(intervals[22,3]))
users22 <- unique(period22$PERSON_ID)
period22=period22[!duplicated(period22$PERSON_ID),]

period23 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[23,2]), as.character(intervals[23,3]))
users23 <- unique(period23$PERSON_ID)
period23=period23[!duplicated(period23$PERSON_ID),]

period24 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[24,2]), as.character(intervals[24,3]))
users24 <- unique(period24$PERSON_ID)
period24=period24[!duplicated(period24$PERSON_ID),]

period25 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[25,2]), as.character(intervals[25,3]))
users25 <- unique(period25$PERSON_ID)
period25=period25[!duplicated(period25$PERSON_ID),]

period26 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[26,2]), as.character(intervals[26,3]))
users26 <- unique(period26$PERSON_ID)
period26=period26[!duplicated(period26$PERSON_ID),]

period27 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[27,2]), as.character(intervals[27,3]))
users27 <- unique(period27$PERSON_ID)
period27=period27[!duplicated(period27$PERSON_ID),]

period28 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[28,2]), as.character(intervals[28,3]))
users28 <- unique(period28$PERSON_ID)
period28=period28[!duplicated(period28$PERSON_ID),]

period29 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[29,2]), as.character(intervals[29,3]))
users29 <- unique(period29$PERSON_ID)
period29=period29[!duplicated(period29$PERSON_ID),]

period30 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[30,2]), as.character(intervals[30,3]))
users30 <- unique(period30$PERSON_ID)
period30=period30[!duplicated(period30$PERSON_ID),]

period31 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[31,2]), as.character(intervals[31,3]))
users31 <- unique(period31$PERSON_ID)
period31=period31[!duplicated(period31$PERSON_ID),]

period32 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[32,2]), as.character(intervals[32,3]))
users32 <- unique(period32$PERSON_ID)
period32=period32[!duplicated(period32$PERSON_ID),]

period33 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[33,2]), as.character(intervals[33,3]))
users33 <- unique(period33$PERSON_ID)
period33=period33[!duplicated(period33$PERSON_ID),]

period34 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[34,2]), as.character(intervals[34,3]))
users34 <- unique(period34$PERSON_ID)
period34=period34[!duplicated(period34$PERSON_ID),]

period35 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[35,2]), as.character(intervals[35,3]))
users35 <- unique(period35$PERSON_ID)
period35=period35[!duplicated(period35$PERSON_ID),]

period36 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[36,2]), as.character(intervals[36,3]))
users36 <- unique(period36$PERSON_ID)
period36=period36[!duplicated(period36$PERSON_ID),]

period37 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[37,2]), as.character(intervals[37,3]))
users37 <- unique(period37$PERSON_ID)
period37=period37[!duplicated(period37$PERSON_ID),]

period38 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[38,2]), as.character(intervals[38,3]))
users38 <- unique(period38$PERSON_ID)
period38=period38[!duplicated(period38$PERSON_ID),]

period39 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[39,2]), as.character(intervals[39,3]))
users39 <- unique(period39$PERSON_ID)
period39=period39[!duplicated(period39$PERSON_ID),]

period40 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[40,2]), as.character(intervals[40,3]))
users40 <- unique(period40$PERSON_ID)
period40=period40[!duplicated(period40$PERSON_ID),]

period41 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[41,2]), as.character(intervals[41,3]))
users41 <- unique(period41$PERSON_ID)
period41=period41[!duplicated(period41$PERSON_ID),]

period42 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[42,2]), as.character(intervals[42,3]))
users42 <- unique(period42$PERSON_ID)
period42=period42[!duplicated(period42$PERSON_ID),]

period43 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[43,2]), as.character(intervals[43,3]))
users43 <- unique(period43$PERSON_ID)
period43=period43[!duplicated(period43$PERSON_ID),]

period44 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[44,2]), as.character(intervals[44,3]))
users44 <- unique(period44$PERSON_ID)
period44=period44[!duplicated(period44$PERSON_ID),]

period45 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[45,2]), as.character(intervals[45,3]))
users45 <- unique(period45$PERSON_ID)
period45=period45[!duplicated(period45$PERSON_ID),]

period46 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[46,2]), as.character(intervals[46,3]))
users46 <- unique(period46$PERSON_ID)
period46=period46[!duplicated(period46$PERSON_ID),]

#period47 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[47,2]), as.character(intervals[47,3]))
#users47 <- unique(period47$PERSON_ID)
#period47=period47[!duplicated(period47$PERSON_ID),]

#period48 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[48,2]), as.character(intervals[48,3]))
#users48 <- unique(period48$PERSON_ID)
#period48=period48[!duplicated(period48$PERSON_ID),]

#period49 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[49,2]), as.character(intervals[49,3]))
#users49 <- unique(period49$PERSON_ID)
#period49=period49[!duplicated(period49$PERSON_ID),]

#period50 <- Data_Selector_Period(clusteredactiveData, clusteredHistory, as.character(intervals[50,2]), as.character(intervals[50,3]))
#users50 <- unique(period50$PERSON_ID)
#period50=period50[!duplicated(period50$PERSON_ID),]

# Selecting only common users
#data_common_users <- subset(data, PERSON_ID %in% tt)
#history_common_users <- subset(history_data, LOGIN %in% tt)
#length( unique(history_common_users$LOGIN))

length(which(period1$PERSON_ID %in% tt))
head(period1)


#period19$Rate1 <- period19$request_count / period19$resources_requested
#period19$Rate2 <- period19$days / period19$request_count

train <- rbind(period1, period2, period3, period4, period5, period6, period7, period8, period9, period10,period11, period12 , period13,period14, period15, period16, period17, period18, period19,period20, period21, period22, period23, period24, period25, period26, period27, period28, period29,period30)
test <- rbind(period31, period32, period33, period34, period35, period36, period37, period38, period39, period40,period41, period42 , period43,period44, period45, period46)

totalDay$resource_assigned[totalDay$cluster==1] <- colnum1
totalDay$resource_assigned[totalDay$cluster==2] <- colnum2
totalDay$resource_assigned[totalDay$cluster==3] <- colnum3
totalDay$resource_assigned[totalDay$cluster==4] <- colnum4
totalDay$resource_assigned[totalDay$cluster==5] <- colnum5
totalDay$resource_assigned[totalDay$cluster==6] <- colnum6
totalDay$resource_assigned[totalDay$cluster==7] <- colnum7
totalDay$resource_assigned[totalDay$cluster==8] <- colnum8
totalDay$resource_assigned[totalDay$cluster==9] <- colnum9
totalDay$resource_assigned[totalDay$cluster==10] <- colnum10
totalDay$resource_assigned[totalDay$cluster==11] <- colnum11
totalDay$resource_assigned[totalDay$cluster==12] <- colnum12
totalDay$resource_assigned[totalDay$cluster==13] <- colnum13
totalDay$resource_assigned[totalDay$cluster==14] <- colnum14
totalDay$resource_assigned[totalDay$cluster==15] <- colnum15
totalDay$resource_assigned[totalDay$cluster==16] <- colnum16
totalDay$resource_assigned[totalDay$cluster==17] <- colnum17
totalDay$resource_assigned[totalDay$cluster==18] <- colnum18
totalDay$resource_assigned[totalDay$cluster==19] <- colnum19
totalDay$resource_assigned[totalDay$cluster==20] <- colnum20



test_totalDay$resource_assigned[test_totalDay$cluster==1] <- colnum1
test_totalDay$resource_assigned[test_totalDay$cluster==2] <- colnum2
test_totalDay$resource_assigned[test_totalDay$cluster==3] <- colnum3
test_totalDay$resource_assigned[test_totalDay$cluster==4] <- colnum4
test_totalDay$resource_assigned[test_totalDay$cluster==5] <- colnum5
test_totalDay$resource_assigned[test_totalDay$cluster==6] <- colnum6
test_totalDay$resource_assigned[test_totalDay$cluster==7] <- colnum7
test_totalDay$resource_assigned[test_totalDay$cluster==8] <- colnum8
test_totalDay$resource_assigned[test_totalDay$cluster==9] <- colnum9
test_totalDay$resource_assigned[test_totalDay$cluster==10] <- colnum10
test_totalDay$resource_assigned[test_totalDay$cluster==11] <- colnum11
test_totalDay$resource_assigned[test_totalDay$cluster==12] <- colnum12
test_totalDay$resource_assigned[test_totalDay$cluster==13] <- colnum13
test_totalDay$resource_assigned[test_totalDay$cluster==14] <- colnum14
test_totalDay$resource_assigned[test_totalDay$cluster==15] <- colnum15
test_totalDay$resource_assigned[test_totalDay$cluster==16] <- colnum16
test_totalDay$resource_assigned[test_totalDay$cluster==17] <- colnum17
test_totalDay$resource_assigned[test_totalDay$cluster==18] <- colnum18
test_totalDay$resource_assigned[test_totalDay$cluster==19] <- colnum19
test_totalDay$resource_assigned[test_totalDay$cluster==20] <- colnum20



totalDay$weight_request <- totalDay$request_count / totalDay$resources_requested
totalDay$bandwidth <- totalDay$weight_request * totalDay$resource_assigned

test_totalDay$weight_request <- test_totalDay$request_count / test_totalDay$resources_requested
test_totalDay$bandwidth <- test_totalDay$weight_request * test_totalDay$resource_assigned

write.csv(totalDay, file = 'final_totalDay1.csv', row.names = F, quote = F)
write.csv(test_totalDay, file = 'final_test_totalDay2.csv', row.names = F, quote = F)

