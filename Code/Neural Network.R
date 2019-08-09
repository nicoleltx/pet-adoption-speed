setwd('/Users/guiran/Google Drive/Spring2019/758T/Project')
library("tidyverse")

###############################Adoption speed is 0 - 4

df <- read_csv("Data-final.csv")

df$Name <- NULL
df$PetID <- NULL
df$RescuerID <- NULL
df$Breed1 <- NULL
df$Breed2 <- NULL
df$State <- NULL
df$Description <- NULL
df$`adoptionspeed0,1` <- NULL

df <- df[complete.cases(df),]
#df$word_count <- as.numeric(df$word_count)
#aveLen <- sum(df$word_count,na.rm = TRUE)/length(df$word_count)
#df$word_count <- ifelse(df$word_count>aveLen,1,0)

library("fastDummies")
df <- dummy_cols(df, select_columns = c("AdoptionSpeed"), remove_first_dummy = FALSE)


df <- data.frame(df[,1:16],df$mixbreed,df$word_count,df$AdoptionSpeed,df[,41:45])

set.seed(12345)

train <- sample(nrow(df),0.7*nrow(df))
df_train <- df[train,]
df_test <- df[-train,]

library("neuralnet")

set.seed(123)
nn <- neuralnet(AdoptionSpeed_0+AdoptionSpeed_1+AdoptionSpeed_2+AdoptionSpeed_3+AdoptionSpeed_4~.-df.AdoptionSpeed,data=df_train,err.fct = "ce",
                rep = 2,hidden=c(4),threshold = 0.2,linear.output = FALSE)

#a
plot(nn,rep="best")

#b

cutoff <- 0.5
#Train
predprobnn <- neuralnet::compute(nn, df_train[1:18])$net.result
idx <- apply(predprobnn,1,which.max)
predicted <- c('AdoptionSpeed_0','AdoptionSpeed_1','AdoptionSpeed_2','AdoptionSpeed_3','AdoptionSpeed_4')[idx]
#predicted <- ifelse(predprobnn > cutoff, 1, 0)
(confusion_train_n <- table(df_train$df.AdoptionSpeed, predicted))
(acc = (confusion_train_n[1,1]+confusion_train_n[2,2]+confusion_train_n[3,3])/sum(confusion_train_n))

#Test

predprobnn <- neuralnet::compute(nn, df_test[1:18])$net.result
idx <- apply(predprobnn,1,which.max)
predicted <- c('AdoptionSpeed_0','AdoptionSpeed_1','AdoptionSpeed_2','AdoptionSpeed_3','AdoptionSpeed_4')[idx]
#predicted <- ifelse(predprobnn > cutoff, 1, 0)
(confusion_train_n <- table(df_test$df.AdoptionSpeed, predicted))
(acc = (confusion_train_n[1,1]+confusion_train_n[2,2]+confusion_train_n[3,3])/sum(confusion_train_n))
########################################################

########################################################Adoption speed is 0 - 1
df <- read_csv("Data-final.csv")

df$Name <- NULL
df$PetID <- NULL
df$RescuerID <- NULL

df <- df[complete.cases(df),]
df$word_count <- as.numeric(df$word_count)
aveLen <- sum(df$word_count,na.rm = TRUE)/length(df$word_count)
df$word_count <- ifelse(df$word_count>aveLen,1,0)
df$AdoptionSpeed <- ifelse(df$AdoptionSpeed == 4,0,1)
#type, gender, maturity size, furlength, vaccinate, dewormed, sterilized, health
# Mixbreed

df <- subset(df, select = c(Type,Gender,MaturitySize,FurLength, word_count,Vaccinated, Dewormed,
                            Sterilized, Health,mixbreed,most_popular,
                            PC1,PC2,PC3,PC4,PC5,ave_sentiment,Crime_index,
                            living_space,Median_income,AdoptionSpeed))


set.seed(12345)

train <- sample(nrow(df),0.7*nrow(df))
df_train <- df[train,]
df_test <- df[-train,]

library("neuralnet")

set.seed(123)
nn <- neuralnet(AdoptionSpeed~.,data=df_train, err.fct = "ce",
                rep = 2,hidden=c(4),threshold = 0.05,linear.output = FALSE)

 #a
plot(nn,rep="best")

#b

cutoff <- 0.5
#Train
predprobnn <- neuralnet::compute(nn, df_train[1:17])$net.result
predicted <- ifelse(predprobnn > cutoff, 1, 0)
(confusion_train_n <- table(df_train$AdoptionSpeed, predicted))
(acc = (confusion_train_n[1,1]+confusion_train_n[2,2])/sum(confusion_train_n))

#Test

predprobnn <- neuralnet::compute(nn, df_test[1:17])$net.result
predicted <- ifelse(predprobnn > cutoff, 1, 0)
(confusion_test <- table(df_test$AdoptionSpeed, predicted))
(acc = (confusion_test[1,1]+confusion_test[2,2])/sum(confusion_test))

############################
#a

for (i in 0:4){
  set.seed(13)
  nn <- neuralnet(AdoptionSpeed~.,data=df_train, err.fct = "ce",
                  rep = 2,hidden=c(i),threshold = 0.05,linear.output = FALSE)
  cutoff <- 0.5
  #Train
  predprobnn <- neuralnet::compute(nn, df_train[1:17])$net.result
  predicted <- ifelse(predprobnn > cutoff, 1, 0)
  (confusion_train_n <- table(df_train$AdoptionSpeed, predicted))
  (acc = (confusion_train_n[1,1]+confusion_train_n[2,2])/sum(confusion_train_n))
  print(acc)
  #Test
  
  predprobnn <- neuralnet::compute(nn, df_test[1:17])$net.result
  predicted <- ifelse(predprobnn > cutoff, 1, 0)
  (confusion_test <- table(df_test$AdoptionSpeed, predicted))
  (acc = (confusion_test[1,1]+confusion_test[2,2])/sum(confusion_test))
  print(acc)
}
for (i in 5:7){
  set.seed(13)
  nn <- neuralnet(AdoptionSpeed~.,data=df_train, err.fct = "ce",
                  rep = 2,hidden=c(i),threshold = 0.1,linear.output = FALSE)
  cutoff <- 0.5
  #Train
  predprobnn <- neuralnet::compute(nn, df_train[1:17])$net.result
  predicted <- ifelse(predprobnn > cutoff, 1, 0)
  (confusion_train_n <- table(df_train$AdoptionSpeed, predicted))
  (acc = (confusion_train_n[1,1]+confusion_train_n[2,2])/sum(confusion_train_n))
 
  #Test
  
  predprobnn <- neuralnet::compute(nn, df_test[1:17])$net.result
  predicted <- ifelse(predprobnn > cutoff, 1, 0)
  (confusion_test <- table(df_test$AdoptionSpeed, predicted))
  (acc = (confusion_test[1,1]+confusion_test[2,2])/sum(confusion_test))
  print(acc)
}

#################
nn <- neuralnet(AdoptionSpeed~.,data=df_train, err.fct = "ce",
                rep = 2,hidden=c(4,2),threshold = 0.1,linear.output = FALSE)
cutoff <- 0.5
#Train
predprobnn <- neuralnet::compute(nn, df_train[1:17])$net.result
predicted <- ifelse(predprobnn > cutoff, 1, 0)
(confusion_train_n <- table(df_train$AdoptionSpeed, predicted))
(acc = (confusion_train_n[1,1]+confusion_train_n[2,2])/sum(confusion_train_n))
print(acc)
#Test

predprobnn <- neuralnet::compute(nn, df_test[1:17])$net.result
predicted <- ifelse(predprobnn > cutoff, 1, 0)
(confusion_test <- table(df_test$AdoptionSpeed, predicted))
(acc = (confusion_test[1,1]+confusion_test[2,2])/sum(confusion_test))
print(acc)
#b
for(i in 1:4){
  set.seed(13)
  cutoff <- 0.5
  nn <- neuralnet(AdoptionSpeed~.,data=df_train, err.fct = "ce",
                  rep = 2,hidden=c(4,i),threshold = 0.1,linear.output = FALSE)
  cutoff <- 0.5
  #Train
  predprobnn <- neuralnet::compute(nn, df_train[1:17])$net.result
  predicted <- ifelse(predprobnn > cutoff, 1, 0)
  (confusion_train_n <- table(df_train$AdoptionSpeed, predicted))
  (acc = (confusion_train_n[1,1]+confusion_train_n[2,2])/sum(confusion_train_n))
  print(acc)
  #Test
  
  predprobnn <- neuralnet::compute(nn, df_test[1:17])$net.result
  predicted <- ifelse(predprobnn > cutoff, 1, 0)
  (confusion_test <- table(df_test$AdoptionSpeed, predicted))
  (acc = (confusion_test[1,1]+confusion_test[2,2])/sum(confusion_test))
  print(acc)
}

nn <- neuralnet(AdoptionSpeed~.,data=df_train, err.fct = "ce",
                rep = 2,hidden=c(5,2),threshold = 0.1,linear.output = FALSE)
cutoff <- 0.5
#Train
predprobnn <- neuralnet::compute(nn, df_train[1:17])$net.result
predicted <- ifelse(predprobnn > cutoff, 1, 0)
(confusion_train_n <- table(df_train$AdoptionSpeed, predicted))
(acc = (confusion_train_n[1,1]+confusion_train_n[2,2])/sum(confusion_train_n))
print(acc)
#Test

predprobnn <- neuralnet::compute(nn, df_test[1:17])$net.result
predicted <- ifelse(predprobnn > cutoff, 1, 0)
(confusion_test <- table(df_test$AdoptionSpeed, predicted))
(acc = (confusion_test[1,1]+confusion_test[2,2])/sum(confusion_test))
print(acc)

plot(nn,rep="best")



#end


