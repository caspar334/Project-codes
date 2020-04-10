loan<-read.csv("loanfull.csv")
head(loan)
summary(loan)


library(tidyverse)
library(stringr)

#Create categorical variable
default_word<-c("Does not meet the credit policy. Status:Charged Off","Charged Off","Default")
fully_p<-c("Fully Paid","Does not meet the credit policy. Status:Fully Paid","Issued")
loan$status<-as.factor(ifelse(loan$loan_status %in% default_word, "Default",ifelse(loan$loan_status=="Current","current",ifelse(loan$loan_status %in% fully_p,"Fully Paid","Late"))))



attach(loan)

loan_new<-data.frame(loan_amnt,total_pymnt,int_rate,emp_length,grade,annual_inc,
                     home_ownership,status,term,installment,purpose,initial_list_status)
detach(loan)
loan_new<-na.omit(loan_new)
loan_new<-loan_new[which(loan_new$emp_length!='n/a'),]
loan_new$term<-as.integer(str_extract(loan_new$term,"[[:digit:]]+"))
loan_new$emp_length<-as.integer(str_extract(loan_new$emp_length,"[[:digit:]]+"))
loan_train<-loan_new[which(loan_new$status%in% c("Fully Paid","Default")),]
loan_train<-loan_train[which(loan_train$home_ownership%in%c("MORTGAGE","RENT","OWN","")),]

#Make 
loan_train$purpose<-as.factor(ifelse(loan_train$purpose=="credit_card","Credit Card",
                                     ifelse(loan_train$purpose=="debt_consolidation","Debt Consolidation",
                                            ifelse(loan_train$purpose=="small_business","Small Business","Others"))))
loan_train$status<-ifelse(loan_train$status=="Default",1,0)
loan_train$status<-as.factor(loan_train$status)
loan_train$loss<-ifelse(loan_train$status==0,loan_train$total_pymnt-loan_train$loan_amnt,loan_train$term*loan_train$installment-loan_train$total_pymnt)
loan_train$term<-as.factor(loan_train$term)
set.seed(22)
nm<-sample(1:nrow(loan_train),190000)
loan_training<-loan_train[nm,]
loan_test<-loan_train[-nm,]

ncol(loan_training)
ncol(loan_test)


#Logistics Regression
glm0<-glm(status~log(annual_inc)*loan_amnt+log(annual_inc)+int_rate+grade+loan_amnt+term+purpose,data=loan_training,family=binomial(link='logit'))
grep("initial_list_status", colnames(loan_training))
summary(glm0)
#Get the prediction 
pred_test<-predict(glm0,loan_test[,-c(2,8,13)],type = "response")
head(loan_test[,-c(2,8,13)])
#If you change pred2, you will get different confusion matrix
pred2<-as.numeric(pred_test>1.0)
pred2<-factor(pred2)
levels(pred2) <- c("Paid_Full","Default")
table(pred2,loan_test$status)

pred3<-as.numeric(pred_test>0.8)
loan_testing2<-data.frame(loan_test,pred3)
head(loan_testing2)
l1<-loan_testing2[which(loan_testing2$pred3==0 & loan_testing2$status==1),]
l2<-loan_testing2[which(loan_testing2$pred3==1 & loan_testing2$status==0),]
l3<-loan_testing2[which(loan_testing2$pred3==0,loan_testing2$status==0),]
summary(l1)
summary(l2)
summary(loan_testing2)
summary(l3$status)

threshold <- c()
PD_AP <- c()
PP_AD <- c()
PP_AP <- c()

for (i in 0:10){
  i2 <- 0.1*i
  threshold <- c(threshold,i2)
  pred3<-as.numeric(pred_test>i2)
  loan_testing2<-data.frame(loan_test,pred3)
  l1<-loan_testing2[which(loan_testing2$pred3==0 & loan_testing2$status==1),]
  l2<-loan_testing2[which(loan_testing2$pred3==1 & loan_testing2$status==0),]
  l3<-loan_testing2[which(loan_testing2$pred3==0,loan_testing2$status==0),]
  
  pdap <-l1 %>%
        summarise(loss_from_default = sum(loss))
  ppad <-l2 %>%
        summarise(loss_from_losing_cust = sum(loss))
  ppap <-l3 %>%
         summarise(profit = sum(loss))
  

  PD_AP <- c(PD_AP,pdap[1,1])
  PP_AD <- c(PP_AD,ppad[1,1])
  PP_AP <- c(PP_AP,ppap[1,1])
}

PD_AP

threshold <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
TT_ER <- PD_AP + PP_AD
Margin <- PP_AP - TT_ER
df <- data.frame(threshold,PD_AP,PP_AD)
plot(threshold,PP_AD, type="b", col="green", lwd=2, xlab="Threshold", ylab="Loss",ylim = c(0,170000000))
lines(threshold, PD_AP,type = 'b', col="blue", lwd=2)
lines(threshold, TT_ER, type = 'b',col='red',lwd =2)
legend(0.0, 150000000, legend=c("loss from default", "loss from losing customers","total loss"),
       col=c("green", "blue","red"), lty=1:2, cex=0.8)

plot(threshold,PP_AP, type="b", col="blue", lwd=2, xlab="Threshold", ylab="USD",ylim = c(-50000000,250000000))
lines(threshold, TT_ER,type = 'b', col="red", lwd=2)
lines(threshold, Margin, type ='b', col='green',lwd=2)
legend(0.0, 250000000, legend=c("Total Loss", "Total Revenue","Margin"),
       col=c("red", "blue","green"), lty=1:2, cex=0.8)

plot(threshold,Margin, type="b", col="black", lwd=2,main = 'Margin' ,xlab="Threshold", ylab="USD")


ppad<-l1 %>%
      summarise(loss_from_default = sum(loss))
l2 %>%
  summarise(loss_from_losing_cust = sum(loss))
l3 %>%
  summarise(profit = sum(loss))

int_calc

table <- data.frame(threshold,PD_AP,PP_AD,PP_AP)
