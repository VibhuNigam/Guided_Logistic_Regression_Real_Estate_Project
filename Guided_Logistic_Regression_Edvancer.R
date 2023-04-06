library(visdat)
library(tidyr)
library(dplyr)

setwd("D:\\Edvancer\\R\\Data (2)")
## ----
rg_train=read.csv("rg_train.csv",stringsAsFactors = FALSE)
rg_test=read.csv("rg_test.csv",stringsAsFactors = FALSE)

glimpse(rg_train)


vis_dat(rg_train)

CreateDummies=function(data,var,freq_cutoff=0                                                                                                      ){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

rg_test$Revenue.Grid=NA

rg_test$data='test'
rg_train$data='train'

rg= rbind(rg_test,rg_train)

vis_dat(rg)


write.csv(colnames(rg),"RG_DATA_COLUMNS.csv")

table(rg$children)

## ------------------------------------------------------------------------
#rg = rg %>%
# mutate(children=ifelse(children=="Zero",0,substr(children,1,1)),
#        children=as.numeric(children))

child=rg$children
child[child=="Zero"]=0
child[child=="4+"]=4
child=as.numeric(child)
rg$children=child


table(rg$age_band)

rg=rg%>%
  mutate(a1=as.numeric(substr(age_band,1,2)),
         a2=as.numeric(substr(age_band,4,5)),
         age=ifelse(substr(age_band,1,2)=='71',71, ifelse(age_band=='Unknown',NA,0.5*(a1+a2)))
         )%>%
  select(-a1,-a2,-age_band)

table(rg$age)

# steps done for the variable family

table(rg$family_income)

family=rg$family_income

family=gsub("[<,>=]","",family)

family=data.frame(income=family)

family_df=separate(family,col ="income" ,into = c("Inc1","Inc2"))

family_df$Inc1=as.numeric(family_df$Inc1)
family_df$Inc2=as.numeric(family_df$Inc2)

family_df$newcol=ifelse(is.na(family_df$Inc1),family_df$Inc2,ifelse(is.na(family_df$Inc2),family_df$Inc1,0.5*(family_df$Inc1+family_df$Inc2)))

#written in a single statement

#family_df=family %in% 
#  separate(col ="income" ,into = c("Inc1","Inc2"))%in%
#  mutate(as.numeric(Inc1),as.numeric(Inc2),
#         ifelse(is.na(Inc1),Inc2,ifelse(is.na(Inc2),Inc1,0.5*(Inc1+Inc2))))

rg$family_income=family_df$newcol

glimpse(rg)

vis_dat(rg)

lapply(rg, function(x) length (unique(x)))
#it provides us the list of all the components of rg and provide a unique count
# for all the columns in the rg. Here, creating a function(x) for every colums unique value.
#it is just done to determine how many unique values are in post_code and post_area

rg$post_code=NULL
rg$post_area=NULL



#handle categorical columns by creating dummies and impute values.

names(rg) [sapply(rg, function(x) is.character(x))]
#here we are checking for all the character columns for rg using sapply

cat_cols= names(rg) [sapply(rg, function(x) is.character(x))]
#saving all the character values columns name in cat_cols

#or we can also create a vector for the same

#cat_cols=c("status","occupation","occupation_partner","home_status","self_employed",
#          "self_employed_partner","TVarea","gender","region")

#creating a for loop for dummy variable 

for(cat in cat_cols){
  if(cat!= "data"){
  rg=CreateDummies(rg,cat,50)
  }
}

vis_dat(rg)

glimpse(rg)


sum(sapply(rg, function(x) is.character(x)))
#this statement help us to know number of character columns in the dataset rg

table(rg$Revenue.Grid)

rg$Revenue.Grid=as.numeric((rg$Revenue.Grid==1))
#this statement here is used to convert the values into 0s and 1s

table(rg$Revenue.Grid)

sort(unlist(lapply(rg, function(x) sum(is.na(x)))))
#this statement answers question about how many NAs we have in the datasets in this case
#they are age, family_income and Revenue.Grid
#we will treat NAs in family_income and age and not Revenue.Grid because we have created 
#those value in rg_test


for loop for imputing values 

for (col in names(rg)) {
 if(sum(is.na(rg[,col]))>0 & !(col %in% c('data', 'Revenue.Grid'))) {
    rg[is.na(rg[,col]),col]=mean(rg[rg$data=='train',col],na.rm=T)
  }
  
}

vis_dat(rg)

#this is the best solutions for conditions where it is large number of columns in which we have to impute values.

#in this case we can simply do imputing one by one 

mean_age=mean(rg$age, na.rm=T)
rg[is.na(rg$age), "age"]=mean_age

fam_mean=mean(rg$family_income, na.rm=T)
rg[is.na(rg$family_income), "family_income"]=fam_mean

vis_dat(rg)

#Training the model
##

#splitting data into train and test

rg_train=rg %>% filter(data=='train') %>% select(-data)

rg_test=rg %>% filter(data=='test') %>% select(-data,-Revenue.Grid)

#split train data into train and test

set.seed(2)
s=sample(1:nrow(rg_train), 0.8*nrow(rg_train))
rg_train1=rg_train[s,] ## Train Data
rg_train2=rg_train[-s,] ## Train Test Data

#We can start to build the model
#We will remove the high VIF values as the first step of building model
##Then we will determine P Value - Step function to basically 


library(car)

for_vif=lm(Revenue.Grid~.-REF_NO,data=rg_train1)

x=as.data.frame(vif(for_vif))

## from here we'll remove vars with high vif one by one, code below is arrived
## at after multiple iterations

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity
           -Investment.in.Derivative-Investment.in.Equity
           -region_SouthEast-TVarea_Central-occupation_Professional
           -region_Scotland
           -Portfolio.Balance,
           data=rg_train1,)

#fitting logistic regression model

log_fit=glm(Revenue.Grid~.-REF_NO-Investment.in.Commudity
            -Investment.in.Derivative-Investment.in.Equity
            -region_SouthEast-TVarea_Central-occupation_Professional
            -region_Scotland
            -Portfolio.Balance,data=rg_train1,family = "binomial")

# here family binomial tells the system that it is logistic regression
#because logistic regression classify thing into 0s and 1s (True or False)


summary(log_fit)

log_fit=step(log_fit)
#it will take 5 to 6 minutes. With the help of this we can get a formula
#derived after multiple iterations done by it.

formula(log_fit) #this will get us the formula derived by the above iterations

log_fit=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer + 
              Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
              Personal.Loan + Investment.Tax.Saving.Bond + 
              Home.Loan + Online.Purchase.Amount + self_employed_partner_No + 
              TVarea_ScottishTV ,data=rg_train1,family='binomial')

summary(log_fit)

# from here we can drop vars one by one which had higher p-value
# code given below is result of multiple iterations

#### performance of score model on validation data

library(pROC) #performance of ROC model library.

#by using this we can determine the ROC/AUC values.
#it is the alternative of R^2 values in linear regression.
#since, it is logistic regression we do not determine R^2 values.
#hence, we calculate AUC values on ROC curve.

val.score=predict(log_fit, newdata= rg_train2,type='response')

#this will give us the probability score of the predicted values. #we need this!!
#if we remove the type='response' it will give the log head values.


#val.score[val.score>=0.5]=1
#val.score[val.score<0.5]=0

#planting auc curve

#here, rg_train2$Revenue.Grid is 20% actual values 
#whereas val.score are the predicted values of rg_train2
auc(roc(rg_train2$Revenue.Grid, val.score))

#since the AUC is 0.96 it is very robust, because it is very much close to 1

# code given below is result of multiple iterations

log.fit.final=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer + 
                    Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
                    Personal.Loan + Investment.Tax.Saving.Bond + 
                    Home.Loan + Online.Purchase.Amount + self_employed_partner_No + 
                    TVarea_ScottishTV ,data=rg_train,family='binomial')

summary(log.fit.final)

# now if we needed to submit probability scores for the test data we can do at this point

test.prob.score= predict(log.fit.final,newdata = rg_test,type='response')

# If you want to convert to Hard Classes and store in dataframe 
# my_cutoff=0.5
# rg_test$revenue_grid=as.numeric(test.prob.score>my_cutoff)

write.csv(test.prob.score,"Guided_Real_estate_LR_V1.csv",row.names = F)

# however if we need to submit hard classes, we'll need to determine cutoff score


train.score=predict(log.fit.final,newdata = rg_train,type='response')

real=rg_train$Revenue.Grid

cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99999,Sn=99999,Sp=99999,KS=9999,F5=9999,F.1=9999,M=9999)

for(cutoff in cutoffs){
  
  ## Conversion into hard calsses
  predicted=as.numeric(train.score>cutoff)
  
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(100*FP+TP)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

cutoff_data=cutoff_data[-1,]


#### visualise how these measures move across cutoffs
library(ggplot2)
ggplot(cutoff_data,aes(x=cutoff,y=F5))+geom_line()

library(tidyr)

cutoff_long=cutoff_data %>% 
  gather(Measure,Value,Sn:M)

ggplot(cutoff_long,aes(x=cutoff,y=Value,color=Measure))+geom_line()


my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff

# now that we have our cutoff we can convert score to hard classes

test.predicted=as.numeric(test.prob.score>my_cutoff)
write.csv(test.predicted,"proper_submission_file_name.csv",row.names = F)


#restart from 2:32:12