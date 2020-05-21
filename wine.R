library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(grf)




data=read.csv('test_data_1904.csv')
summary(data)
dt = data.table(data)
dagg = dt[,.(open = mean(open), click=mean(click), purch = mean(purch),seOpen = sd(open)/sqrt(.N), seClick=sd(click)/sqrt(.N), sePurch = sd(purch)/sqrt(.N),.N),by = .(group)]
dagg


dodge = position_dodge(width=1); ##to form constant dimensions
ggplot(aes(x=group,y=purch,ymax=purch+sePurch,ymin=purch-sePurch),data=dagg)+
  geom_bar(position=dodge,stat="identity",col=2:3,fill=2:3) + 
  geom_errorbar(position=dodge)
labs(x="Group",y="Purchases")

#To evaluate the average causal effect on purchases we use lm as follows. 

summary(lm(purch~group,data=data))
summary(lm(purch~group+chard+sav_blanc+syrah+cab+past_purch+last_purch+visits,data=data)) #adding baseline variables as controls
summary(lm(purch~group+chard+sav_blanc+syrah+cab+last_purch+visits,data=data)) #adding controls
summary(lm(purch~group+chard+sav_blanc+syrah+cab+last_purch+visits,data=data)) #adding controls separating emails

hist(data$last_purch, 
     xlab="Days Since Last Purchase", ylab="Customers", 
     main="Histogram of Days Since Last Purchase")
data$recentPurch = (data$last_purch < 60)
dt = data.table(data)

## Slicing and dicing: recent buyers versus aged customers

dagg = dt[,.(open = mean(open), click=mean(click), purch = mean(purch),seOpen = sd(open)/sqrt(.N), seClick=sd(click)/sqrt(.N), sePurch = sd(purch)/sqrt(.N),.N),by = .(group,recentPurch)]
dagg

dodge = position_dodge(width=1); ##to form constant dimensions
ggplot(aes(fill=group,y=purch,x=recentPurch,ymax=purch+sePurch,ymin=purch-sePurch),data=dagg)+
  geom_bar(position=dodge,stat="identity") + 
  geom_errorbar(position=dodge)
labs(x="Group",y="Purchases")
summary(lm(purch~0+recentPurch + group:recentPurch,data=data))
summary(lm(purch~group*recentPurch,data=data))
## Slicing and dicing: big past_pay customers versus small ones
hist(data$past_purch, 
     xlab="Days Since Last Purchase", ylab="Customers", 
     main="Histogram of Days Since Last Purchase",breaks=c(0,100,200,300,
                                                           400,500,600,
                                                           700,800,900,
                                                           1000,15000),xlim=range(0,1000))
data$BigCustomer = (data$past_purch > 100)
dt = data.table(data)
dagg = dt[,.(open = mean(open), click=mean(click), purch = mean(purch),seOpen = sd(open)/sqrt(.N), seClick=sd(click)/sqrt(.N), sePurch = sd(purch)/sqrt(.N),.N),by = .(group,BigCustomer)]
dagg

## Is email more effective for recent buyers? 

dodge = position_dodge(width=1); ##to form constant dimensions
ggplot(aes(fill=group,y=purch,x=BigCustomer,ymax=purch+sePurch,ymin=purch-sePurch),data=dagg)+
  geom_bar(position=dodge,stat="identity") + 
  geom_errorbar(position=dodge)
labs(x="Group",y="Purchases")
summary(lm(purch~group*BigCustomer,data=data))
summary(lm(purch~0+BigCustomer + group:BigCustomer,data=data))
#Re-analyze the opens, clicks and purchases for people who have bought syrah in the past. 

data$anySyrah = (data$syrah > 0); dt = data.table(data);
dagg = dt[,.(open = mean(open), click=mean(click), purch = mean(purch),seOpen = sd(open)/sqrt(.N), seClick=sd(click)/sqrt(.N), sePurch = sd(purch)/sqrt(.N),.N),by = .(group,anySyrah)]

ggplot(aes(fill=group,y=purch,x=anySyrah,ymax=purch+sePurch,ymin=purch-sePurch),data=dagg)+
  geom_bar(position=dodge,stat="identity") + 
  geom_errorbar(position=dodge)
labs(x="Group",y="Purchases")
summary(lm(purch~group*anySyrah,data=data)) 
summary(lm(purch~0+anySyrah + group:anySyrah,data=data))
#Re-analyze the opens, clicks and purchases for people who have bought cab in the past. 
data$anyCab = (data$cab > 0); dt = data.table(data);
dagg = dt[,.(open = mean(open), click=mean(click), purch = mean(purch),seOpen = sd(open)/sqrt(.N), seClick=sd(click)/sqrt(.N), sePurch = sd(purch)/sqrt(.N),.N),by = .(group,anyCab)]

ggplot(aes(fill=group,y=purch,x=anyCab,ymax=purch+sePurch,ymin=purch-sePurch),data=dagg)+
  geom_bar(position=dodge,stat="identity") + 
  geom_errorbar(position=dodge)
labs(x="Group",y="Purchases")
summary(lm(purch~group*anyCab,data=data))
summary(lm(purch~0+anyCab + group:anyCab,data=data))
#Re-analyze the opens, clicks and purchases for people who have bought chard in the past.
data$anyChard = (data$cab > 0); dt = data.table(data);
dagg = dt[,.(open = mean(open), click=mean(click), purch = mean(purch),seOpen = sd(open)/sqrt(.N), seClick=sd(click)/sqrt(.N), sePurch = sd(purch)/sqrt(.N),.N),by = .(group,anyChard)]

ggplot(aes(fill=group,y=purch,x=anyChard,ymax=purch+sePurch,ymin=purch-sePurch),data=dagg)+
  geom_bar(position=dodge,stat="identity") + 
  geom_errorbar(position=dodge)
labs(x="Group",y="Purchases")
summary(lm(purch~group*anyChard,data=data))
summary(lm(purch~0+anyChard + group:anyChard,data=data))
#Re-analyze the opens, clicks and purchases for people who have bought sav_blanc in the past.
data$anySav = (data$sav_blanc > 0); dt = data.table(data);
dagg = dt[,.(open = mean(open), click=mean(click), purch = mean(purch),seOpen = sd(open)/sqrt(.N), seClick=sd(click)/sqrt(.N), sePurch = sd(purch)/sqrt(.N),.N),by = .(group,anySav)]

ggplot(aes(fill=group,y=purch,x=anySav,ymax=purch+sePurch,ymin=purch-sePurch),data=dagg)+
  geom_bar(position=dodge,stat="identity") + 
  geom_errorbar(position=dodge)
labs(x="Group",y="Purchases")
summary(lm(purch~group*anySav,data=data))
summary(lm(purch~0+anySav + group:anySav,data=data))
## re-analyze the opens, clicks and purchases for people who visits frequency.


hist(data$visits, 
     xlab="Days Since Last Purchase", ylab="Customers", 
     main="Histogram of Days Since Last Purchase",breaks=c(0,1,2,3,4,5,6,7,8,9,10,15,20,25,30,40,50,60,70),xlim=range(0,20))
data$FrequentVisit = (data$visits > 5)
dt = data.table(data)
dagg = dt[,.(open = mean(open), click=mean(click), purch = mean(purch),seOpen = sd(open)/sqrt(.N), seClick=sd(click)/sqrt(.N), sePurch = sd(purch)/sqrt(.N),.N),by = .(group,FrequentVisit)]
dagg
ggplot(aes(fill=group,y=purch,x=FrequentVisit,ymax=purch+sePurch,ymin=purch-sePurch),data=dagg)+
  geom_bar(position=dodge,stat="identity") + 
  geom_errorbar(position=dodge)
labs(x="Group",y="Purchases")
summary(lm(purch~group*FrequentVisit,data=data))
summary(lm(purch~0+FrequentVisit + group:FrequentVisit,data=data))
## Measuring causal effects with regression: Conditional causal effects






## Incrementality: Lift aka Uplift model for purchase amount {.smaller}
# An uplift model is a regression model that incorporates many baseline variables. 

m <- lm(purch ~ group*(last_purch < 60) + group*(visits > 5) +
          group*(chard > 0) +group*(sav_blanc>0) + group*(syrah>0) + group*(cab>0), 
        data=data)
summary(m)$coef



## Causal forest for wine retailer experiment
set.seed(1)
cf_size <- 10000; #nrow(data) 
cf_set = sample(nrow(data),cf_size)
treat <- as.numeric(as.factor(data$group[cf_set]))
response <- data$purch[cf_set]
baseline <- data[cf_set, c("last_purch", "visits", "chard", "sav_blanc", "syrah", "cab")]

cf <- causal_forest(baseline, response, treat)

print(cf)


## Predicted uplift for all customers in test

hist(predict(cf)$predictions, 
     main="Histogram of Purchase Lift", 
     xlab="Purchase Lift for Email", ylab="Customers")

## Uplift versus past purchase amount

trans_gray <- rgb(0.1, 0.1, 0.1, alpha=0.1)
plot(data$past_purch[1:cf_size], predict(cf)$predictions, 
     cex=0.5, col=trans_gray,
     xlab="Past Purchase Amount ($)", ylab="Predicted Treatment Effect ($)")



new_data<-data.frame(group=rep(c('email','ctrl'),times=78312),user_id=NA,
                     chard=NA,sav_blanc=NA,syrah=NA,cab=NA,past_purch=NA,
                     last_purch=NA,visits=NA)
for(i in 1:78312){
  for(j in 1:2){
    new_data$user_id[(i-1)*2+j]<-data$user_id[i]
    new_data$chard[(i-1)*2+j]<-data$chard[i]
    new_data$sav_blanc[(i-1)*2+j]<-data$sav_blanc[i]
    new_data$syrah[(i-1)*2+j]<-data$syrah[i]
    new_data$cab[(i-1)*2+j]<-data$cab[i]
    new_data$past_purch[(i-1)*2+j]<-data$past_purch[i]
    new_data$last_purch[(i-1)*2+j]<-data$last_purch[i]
    new_data$visits[(i-1)*2+j]<-data$visits[i]
  }
}

new_data$prediction<-predict(m,new_data[,c(1,3:6,8,9)])
new_data$lift<-NA
for(i in 1:78312){
  new_data$lift[2*i-1]<-new_data$prediction[2*i-1]-new_data$prediction[2*i]
}

new_data2<-new_data[!is.na(new_data$lift),]
data$lift_lm<-new_data2$lift



new_cust<-data.frame(data$chard,data$sav_blanc,data$syrah,data$cab,data$last_purch,
                     data$visits)
colnames(new_cust)<-c('chard','sav_blanc','syrah','cab','last_purch','visits')
new_cust$predction_cf <- predict(cf,new_cust)

names(new_cust)[7]<-'cf'
data$lift_cf<-new_cust$cf
data$profit<-data$lift_cf*0.3-0.1
data$profit_lm<-data$lift_lm*0.3-0.1
data$target_lm<-data$profit_lm>0
data$target_cf<-data$profit>0
non_target<-data[data$target_cf==0,]
target<-data[data$target_cf==1,]
length(target$user_id)/length(data$user_id)
summary(target)
summary(non_target)


