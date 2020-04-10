library(lubridate)
library(tidyverse)
library(ggmap)
library(MLmetrics)
library(randomForest)
library(pROC)
library(tree)
library(grid)
library(libcoin)
library(mvtnorm)
library(partykit)
library(rpart)
library(lattice)
library(caret)
library(e1071)


# Text file with the API key
register_google(key = "AIzaSyCgMGcj1t8FAeqs3SYCNh1UJZNoXuo73kw" )
source("DataAnalyticsFunctions.R")

bike1<-read.csv("metro-bike-share-trips-2019-q1.csv")
bike2<-read.csv("metro-bike-share-trips-2019-q2.csv")
bike<-rbind(bike1,bike2)
bike$start_station<-as.factor(bike$start_station)
bike$end_station<-as.factor(bike$end_station)
bike$month<-month(bike$start_time)
bike$date<-date(bike$start_time)
bike$weekday<-wday(bike$start_time)
bike$hr<-hour(bike$start_time)
bike$day<-day(bike$start_time)
#Some basic exploratory data analysis and visualization

#Explore hour and weekdays
hour_trip<-bike%>%
  group_by(hr)%>%
  summarise(ct=n())

ggplot(hour_trip, aes(x=hr,y=ct,fill=ct))+
  geom_bar(stat = "identity")

weekday_trip<-bike%>%
  group_by(weekday,hr)%>%
  summarise(ct=n())

ggplot(weekday_trip, aes(x=hr,y=ct,fill=ct))+
  geom_bar(stat = "identity")+
  facet_wrap(~weekday)



bike_label <- bike %>% mutate(weekday =ifelse(bike$weekday== 1,'1.Sunday',
                                              ifelse(bike$weekday== 2,'2.Monday',
                                                     ifelse(bike$weekday== 3,'3.Tuesday',
                                                            ifelse(bike$weekday== 4,'4.Wednesday',
                                                                   ifelse(bike$weekday== 5,'5.Thursday', 
                                                                          ifelse(bike$weekday== 6,'6.Friday','7.Saturday')))))))
weekday_trip<-bike_label%>%
  group_by(weekday,hr)%>%
  summarise(count=n())

ggplot(weekday_trip, aes(x=hr,y=count,fill=count))+
  geom_bar(stat = "identity")+
  facet_wrap(~weekday)+ 
  scale_fill_gradient(low="blue", high="yellow")+
  xlab('hour')+
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA))

##visualization


#############################


table(bike$passholder_type)/nrow(bike) #The percentage of different pass
table(bike$trip_route_category)/nrow(bike) #85% are one-way trip
table(bike$bike_type)/nrow(bike) #78% are standard bike trip

#We only focus on Annual pass, monthly pass, day pass and walk-up since the rest is
#too insignifcant to worry about

#We group them by date and station, so we get a record of every bike station for each day
#This include the percentage of one-way trip, the percentage of standard bike trip (Non-electronic)
#And percentage of monthly pass and walk-up for every station for every day.

# For start station information
bike_group<-bike%>%
  filter(bike$passholder_type %in% c("Annual Pass","Monthly Pass","One Day Pass","Walk-up"))%>%
  group_by(date,start_station)%>%
  summarise(avg_duration=mean(duration),count=n(),one_way=(table(trip_route_category)/n())[[1]],
            standard=(table(bike_type)/n())[[3]],
            monthly=(table(passholder_type)/n())[[3]],
            walk_up=(table(passholder_type)/n())[[6]],
  )


# For destination station information
bike_group_2<-bike%>%
  filter(bike$passholder_type %in% c("Annual Pass","Monthly Pass","One Day Pass","Walk-up"))%>%
  group_by(date,end_station)%>%
  summarise(avg_duration=mean(duration),count=n(),one_way=(table(trip_route_category)/n())[[1]],
            standard=(table(bike_type)/n())[[3]],
            monthly=(table(passholder_type)/n())[[3]],
            walk_up=(table(passholder_type)/n())[[6]])



# Join them on date and station, and now start_station just means station
m1<-full_join(bike_group,bike_group_2,by=c("date","start_station"="end_station"),all = TRUE)

m1$isendnull<-ifelse(is.na(m1$count.y),1,0)
m1$isstartnull<-ifelse(is.na(m1$count.x),1,0)
m1[is.na(m1)]<-0

m1$flow<-m1$count.y-m1$count.x

attach(m1)
m2<-m1[order(start_station,date),]
detach(m1)

#########Only get the stations that are in both periods
C <- intersect(unique(bike1$start_station), unique(bike1$end_station))
D <- intersect(unique(bike2$start_station), unique(bike2$end_station))

E <- intersect(unique(C), unique(D))

m2<-m2%>%
  filter(start_station %in% E)


### For every station, move the flow one unit up and delete the last day for every station since it 
### does not have information for next day.
m2$next_day_flow<-c(m2$flow[-1],0)

m2$lastday<-c(duplicated(m2$start_station)[-1],FALSE)
final_data<-m2%>%
  filter(lastday==TRUE)
final_data$weekday<-wday(final_data$date)
final_data$netflow<-ifelse(final_data$next_day_flow>=0,1,0)
### Get Rid of the replication check
final_data<-final_data[,-19]
qualified_station<-unique(final_data$start_station)[table(final_data$start_station)>170]
final_data<-final_data%>%
  filter(start_station %in% qualified_station)

##################### BoxPlot ####################
final_data1 <- final_data %>% mutate(weekday =ifelse(weekday== 1,'1.Sunday',
                                                     ifelse(weekday== 2,'2.Monday',
                                                            ifelse(weekday== 3,'3.Tuesday',
                                                                   ifelse(weekday== 4,'4.Wednesday',
                                                                          ifelse(weekday== 5,'5.Thursday', 
                                                                                 ifelse(weekday== 6,'6.Friday','7.Saturday')))))))
### Box plot####

ggplot(final_data1,aes(y = walk_up.x,x= weekday,fill=weekday))+
  geom_boxplot()+
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA),
        axis.text.x = element_text(angle=60, hjust=1))+
  xlab('weekday')+
  ylab('walk up rate')



ggplot(final_data1,aes(y = monthly.x,x= weekday,fill=weekday))+
  geom_boxplot()+
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA),
        axis.text.x = element_text(angle=60, hjust=1))+
  xlab('weekday')+
  ylab('monthly pass rate')


################# Logistics Regression ##################
station_info<-data.frame(station=qualified_station,Accuracy=NA,OOS_Accuracy=NA,Inflow_percentage=NA,AUC=NA)

for(i in 2:52){
  station_id<-qualified_station[i]
  filtered_data<-final_data%>%
    filter(start_station==station_id)
  filtered_data<-filtered_data[,-2]
  station_info$Inflow_percentage[i]<-(table(filtered_data$netflow)/nrow(filtered_data))[[2]]
  testing_data<-filtered_data[which(month(filtered_data$date)==6),]
  filtered_data<-filtered_data[which(month(filtered_data$date)!=6),]
  if(station_info$Inflow_percentage[i]<1){
    logistics<-glm(netflow~.-flow-next_day_flow-date,data = filtered_data,family = "binomial")
    pred_test<-predict(logistics,filtered_data[,-19],type = "response")
    pred2<-as.numeric(pred_test>0.5)
    pred2<-factor(pred2)
    levels(pred2)<-c(0,1)
    station_info$Accuracy[i]<-Accuracy(pred2,filtered_data$netflow)
    pred_test<-predict(logistics,testing_data[,-19],type = "response")
    pred2<-as.numeric(pred_test>0.5)
    pred2<-factor(pred2)
    levels(pred2)<-c(0,1)
    station_info$OOS_Accuracy[i]<-Accuracy(pred2,testing_data$netflow)}
  else {
    station_info$Accuracy<-1
  }
  roc_obj <- roc(as.numeric(testing_data$netflow), as.numeric(pred2))
  station_info$AUC[i]<-auc(roc_obj)
}
##Run a classification Tree

station_info1<-data.frame(station=qualified_station,Tree_In_Sample_Accuracy=NA,Tree_OOS_Accuracy=NA,Inflow_Percentage=NA)
for(i in 2:52){
  station_id<-qualified_station[i]
  filtered_data<-final_data%>%
    filter(start_station==station_id)
  filtered_data<-filtered_data[,-2]
  station_info1$Inflow_Percentage[i]<-(table(filtered_data$netflow)/nrow(filtered_data))[[2]]
  testing_data<-filtered_data[which(month(filtered_data$date)==6),]
  filtered_data<-filtered_data[which(month(filtered_data$date)!=6),]
  outflowtree2 <- tree(netflow~.-flow-next_day_flow-date, data = filtered_data)
  pred_test1<-predict(outflowtree2,filtered_data[,-19])
  pred3<-as.numeric(pred_test1>0.5)
  pred3<-factor(pred3)
  levels(pred3)<-c(0,1)
  station_info1$Tree_In_Sample_Accuracy[i]<-Accuracy(pred3,filtered_data$netflow)
  pred_test1<-predict(outflowtree2,testing_data[,-19])
  pred3<-as.numeric(pred_test1>0.5)
  pred3<-factor(pred3)
  levels(pred3)<-c(0,1)
  station_info1$Tree_OOS_Accuracy[i]<-Accuracy(pred3,testing_data$netflow)
  roc_obj <- roc(as.numeric(testing_data$netflow), as.numeric(pred3))
  station_info1$AUC[i]<-auc(roc_obj)
}



####Random Forest model


station_info2<-data.frame(station=qualified_station,RF_Accuracy=NA,RF_Out_Sample_Accuracy=NA,Inflow_Percentage=NA)

for(i in 2:52){
  station_id<-qualified_station[i]
  filtered_data<-final_data%>%
    filter(start_station==station_id)
  filtered_data<-filtered_data[,-2]
  station_info2$Inflow_Percentage[i]<-(table(filtered_data$netflow)/nrow(filtered_data))[[2]]
  testing_data<-filtered_data[which(month(filtered_data$date)==6),]
  filtered_data<-filtered_data[which(month(filtered_data$date)!=6),]
  RF1 <- randomForest(netflow~.-flow-next_day_flow-date, data = filtered_data, family="binominal")
  pred_test2<-predict(RF1,filtered_data[,-19],type = "response")
  pred4<-as.numeric(pred_test2>0.5)
  pred4<-factor(pred4)
  levels(pred4)<-c(0,1)
  station_info2$RF_Accuracy[i]<-Accuracy(pred4,filtered_data$netflow)
  pred_testing<-predict(RF1,testing_data[,-19],type = "response")
  pred5<-as.numeric(pred_testing>0.5)
  pred5<-factor(pred5)
  levels(pred5)<-c(0,1)
  station_info2$RF_Out_Sample_Accuracy[i]<-Accuracy(pred5,testing_data$netflow)
  roc_obj <- roc(as.numeric(testing_data$netflow), as.numeric(pred5))
  station_info2$AUC[i]<-auc(roc_obj)
}

station_complete_info<-inner_join(station_info,station_info1,by="station")
station_complete_info<-inner_join(station_complete_info,station_info2,by="station")
station_complete_info<-na.omit(station_complete_info)
summary(station_complete_info)
colMeans(station_complete_info[,-1])

################################
OOS <- station_complete_info$OOS_Accuracy
OOS<-data_frame(OOS)
OOS<-OOS%>%
  mutate(station_complete_info$Tree_OOS_Accuracy)%>%
  mutate(station_complete_info$RF_Out_Sample_Accuracy)
names(OOS) <- c("Logistic","Tree","Random Forest")
boxplot(OOS)+title("OOS Accuracy")

In_Sample <- station_complete_info$Accuracy
In_Sample<-data_frame(In_Sample)
In_Sample<-In_Sample%>%
  mutate(station_complete_info$Tree_In_Sample_Accuracy)%>%
  mutate(station_complete_info$RF_Accuracy)
names(In_Sample)<- c("Logistic","Tree","Random Forest")
boxplot(In_Sample)+ title("In Sample Accuracy")
summary(In_Sample)

AUC<- station_complete_info$AUC.x
AUC<-data_frame(AUC)
AUC<-AUC%>%
  mutate(station_complete_info$AUC.y)%>%
  mutate(station_complete_info$AUC)
names(AUC) <- c("Logistic","Tree","Random Forest")
boxplot(AUC)+title("AUC")


#####################Cluster Analysis
grouped_station<-final_data%>%
  group_by(start_station)%>%
  summarise(avg_start=mean(count.x),avg_end=mean(count.y),
            trip_activity=mean(count.x+count.y),
            avg_start_duration=sum(count.x*avg_duration.x)/sum(count.x),
            avg_end_duration=sum(count.y*avg_duration.y)/sum(count.y),
            avg_start_flow=mean(flow),
            avg_one_way.y=sum(count.y*one_way.y)/sum(count.y),
            avg_walk_up.y=sum(count.y*walk_up.y)/sum(count.y),
            avg_standard.y=sum(count.y*standard.y)/sum(count.y),
            avg_one_way.x=sum(count.x*one_way.x)/sum(count.x),
            avg_walk_up.x=sum(count.x*walk_up.x)/sum(count.x),
            avg_standard.x=sum(count.x*standard.x)/sum(count.x)
)


grouped_station$start_station <- as.factor(grouped_station$start_station)
kmeans_station <- kmeans(grouped_station,4,nstart=52)

kmeans_station
grouped_station$cluster <-factor(kmeans_station$cluster)


#################Get the coordinates of station ########################

bike_plot<-bike%>%
  filter(start_station%in% qualified_station)%>%
  filter(end_station %in% qualified_station)
bike_plot<-bike_plot[,c(5,6,7)]
bike_plot<-unique(bike_plot)
station_plot<-inner_join(grouped_station,bike_plot)
summary(station_plot$cluster)

qmap("Los Angeles")
get_googlemap("Los Angeles",maptype = "roadmap",zoom = 13)%>%ggmap()+
  geom_point(aes(x=start_lon,y=start_lat,color=cluster,size=trip_activity),data = na.omit(station_plot))+
  scale_color_manual(breaks=c(1,2,3),values=c("red","blue", "green"))

##################################################################



station_cluster_info<-grouped_station[,c(1,14)]

final_data_with_cluster<-left_join(final_data,station_cluster_info)
final_data_with_cluster$netflow<-factor(final_data_with_cluster$netflow)
cluster1_station<-subset(final_data_with_cluster,final_data_with_cluster$cluster==1)
cluster2_station<-subset(final_data_with_cluster,final_data_with_cluster$cluster==2)
cluster3_station<-subset(final_data_with_cluster,final_data_with_cluster$cluster==3)

cluster_info<-data.frame(cluster=1:3,Logistics=NA,classification_tree=NA,Random_forest=NA,
                         LR_AUC=NA,CT_AUC=NA,RF_AUC=NA)

for (i in 1:3){
  filtered_data<-final_data_with_cluster%>%
    filter(cluster==i)
  filtered_data<-filtered_data[,-21]
  ## Divide the testing and training sets
  testing_data<-filtered_data[which(month(filtered_data$date)==6),]
  filtered_data<-filtered_data[which(month(filtered_data$date)!=6),]
  #Logistics Regression
  logistics<-glm(netflow~.-flow-next_day_flow-date,data = filtered_data,family = "binomial")
  pred_test<-predict(logistics,testing_data[,-20],type = "response")
  pred2<-as.numeric(pred_test>0.5)
  pred2<-factor(pred2)
  levels(pred2)<-c(0,1)
  cluster_info$Logistics[i]<-Accuracy(pred2,testing_data$netflow)
  roc_obj <- roc(as.numeric(testing_data$netflow), as.numeric(pred2))
  cluster_info$LR_AUC[i]<-auc(roc_obj)
  #Classification Tree
  outflowtree <- rpart(netflow~.-flow-next_day_flow-date, data = filtered_data)
  pred_test1<-predict(outflowtree,testing_data[,-20],type = "class")
  levels(pred_test1)<-c(0,1)
  cluster_info$classification_tree[i]<-Accuracy(pred_test1,testing_data$netflow)
  roc_obj <- roc(as.numeric(testing_data$netflow), as.numeric(pred_test1))
  cluster_info$CT_AUC[i]<-auc(roc_obj)
  #Random Forest
  RF1 <- randomForest(netflow~.-flow-next_day_flow-date, data = filtered_data)
  pred_testing<-predict(RF1,testing_data[,-20],type = "class")
  pred5<-pred_testing
  levels(pred5)<-c(0,1)
  cluster_info$Random_forest[i]<-Accuracy(pred5,testing_data$netflow)
  roc_obj <- roc(as.numeric(testing_data$netflow), as.numeric(pred5))
  cluster_info$RF_AUC[i]<-auc(roc_obj)
}


train_set<-cluster3_station[which(month(cluster3_station$date)!=6),]
test_set<-cluster3_station[which(month(cluster3_station$date)==6),]
rpart3<-rpart(netflow~.-flow-next_day_flow-date,data=train_set)
summary(rpart3)
plot(rpart3)
text(rpart3)
pred3<-predict(rpart3,test_set[-20],type = "class")
confusionMatrix(pred3,test_set$netflow)




