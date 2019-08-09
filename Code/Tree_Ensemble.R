library(tree)
library(ISLR)
library(randomForest)
library(gbm)
library(xgboost)
library(rpart)
library(adabag)
library(nnet)
############################
#original data-0,1,2,3,4
#Read data
df <- read.csv("~/Desktop/Spring 2019/758T/Project/Data-final.csv")
#calculate description length
for (i in 1:14990){
  df$DesLen[i] <- nchar(as.character(df$Description[i]))
}
#Prepare data
df$Name <- NULL
df$RescuerID <- NULL
df$PetID <- NULL
df$Breed1 <- NULL
df$Breed2 <- NULL
df$adoptionspeed0.1 <- NULL
df$Description <- NULL
df$State <- NULL
#original data
df[18:40] <- NULL

#
factor.cols <- c('Type','Gender','Color1','Color2','Color3','MaturitySize','FurLength','Vaccinated','Dewormed','Sterilized','Health','Quantity','AdoptionSpeed')                                                    
df[factor.cols] <- lapply(df[factor.cols], factor)
str(df)
#

# Split the data
set.seed(12345)
inTrain <- sample(nrow(df), 0.7*nrow(df))
train <- data.frame(df[inTrain,])
test <- data.frame(df[-inTrain,])
#
############################
#full tree, train data-testing rpart
tree.train <- rpart(train$AdoptionSpeed~., data = train)
#full tree predict test
tree.test.pred <- predict(tree.train, test, type = 'class')
#table
table.tree <-table(test$AdoptionSpeed,tree.test.pred)
#accuracy
(acc = sum(diag(table.tree))/sum(table.tree))
#prune
prune.tree <- prune(tree.train, cp=0.01)
prune.test.pred <- predict(prune.tree, test, type = 'class')

#table
table.tree <-table(test$AdoptionSpeed,prune.test.pred)
#accuracy
(acc = sum(diag(table.tree))/sum(table.tree))
####################
#test each variables significant
mnom <- multinom(df$AdoptionSpeed~df$DesLen, data = df)#tried each variable here
z <- summary(mnom)$coefficients/summary(mnom)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

####################
plot.rpart.obj <- function(rpart.obj, font.size = 0.2) {
  ## plot decision tree
  plot(rpart.obj,
       uniform   = T,    
       branch    = 1,    
       compress  = F,   
       nspace    = 0.1,
       margin    = 0.1, 
       minbranch = 0.3)  
  
  ## Add text
  text(x      = rpart.obj,   
       splits = T,           
       all    = T,           
       use.n  = T,           
       cex    = font.size)   
}
plot.rpart.obj(tree.train,1)

plot.rpart.obj(prune.tree,1)

####################
#Bagging
set.seed(123)
bag <- randomForest(train$AdoptionSpeed~., data = train, mtry=20, importance=TRUE)
bag
pred.bag <- predict(bag, newdata = test)
bag.test <- df[-inTrain, 'adopt']
bag.test <- df[-inTrain, 'adopt']
#table
table.tree <-table(test$AdoptionSpeed,pred.bag)
#accuracy
(acc = sum(diag(table.tree))/sum(table.tree))
#result
importance(bag)
varImpPlot(bag)

#random forest(m=4)
set.seed(123)
rf <- randomForest(train$AdoptionSpeed~., data = train, mtry=4, importance=TRUE)
rf
pred.rf <- predict(rf, newdata = test)
rf.test <- df[-inTrain, 'adopt']
#table
table.tree <-table(test$AdoptionSpeed,pred.rf)
#accuracy
(acc = sum(diag(table.tree))/sum(table.tree))
#result
importance(rf)
varImpPlot(rf)

############################
#Read data
df <- read.csv("~/Desktop/Spring 2019/758T/Project/Data-final.csv")
#calculate description length
for (i in 1:14990){
  df$DesLen[i] <- nchar(as.character(df$Description[i]))
}
#Prepare data
df$Name <- NULL
df$RescuerID <- NULL
df$PetID <- NULL
df$Breed1 <- NULL
df$Breed2 <- NULL
df$adoptionspeed0.1 <- NULL
df$Description <- NULL
df$State <- NULL
#original data
df[18:40] <- NULL
str(df)
df$AdoptionSpeed <- as.factor(df$AdoptionSpeed)
# Split the data
set.seed(12345)
inTrain <- sample(nrow(df), 0.7*nrow(df))
train <- data.frame(df[inTrain,])
test <- data.frame(df[-inTrain,])
#
str(test)
set.seed(123)
boost <- boosting(train$AdoptionSpeed~train$Type.,train,boos = TRUE)
boost
summary(boost)
plot(boost)
pred.boost <- predict(boost, newdata = test, n.trees = 5000, type = 'response')
predicted <- ifelse(pred.boost>=0.5, 1,0)
boost.test2 <- df[-inTrain, 'adopt']
(c = table(boost.test2,predicted))
(acc = (c[1,1]+c[2,2])/sum(c))



############################
#original data-0,1
#Read data
df <- read.csv("~/Desktop/Spring 2019/758T/Project/Data-final.csv")
#calculate description length
for (i in 1:14990){
  df$DesLen[i] <- nchar(as.character(df$Description[i]))
}
#Prepare data
df$Name <- NULL
df$RescuerID <- NULL
df$PetID <- NULL
df$Breed1 <- NULL
df$Breed2 <- NULL
df$adoptionspeed0.1 <- NULL
df$Description <- NULL
df$State <- NULL
#original data
df[18:40] <- NULL
#add Adopt
df$adopt <- ifelse(df$AdoptionSpeed<4,1,0)
df$AdoptionSpeed <- NULL

#
factor.cols <- c('Type','Gender','Color1','Color2','Color3','MaturitySize','FurLength','Vaccinated','Dewormed','Sterilized','Health','Quantity','adopt')                                                    
df[factor.cols] <- lapply(df[factor.cols], factor)
str(df)
#

# Split the data
set.seed(12345)
inTrain <- sample(nrow(df), 0.7*nrow(df))
train <- data.frame(df[inTrain,])
test <- data.frame(df[-inTrain,])
#

############################

df$adopt <- as.factor(df$adopt)
train$adopt <- as.factor(train$adopt)
test$adopt <- as.factor(test$adopt)
str(df)
#full tree, train data
tree.train <- tree(train$adopt~., data = train)
#full tree predict test
tree.test.pred <- predict(tree.train, test, type = 'class')
#result
plot(tree.train)
text(tree.train,pretty = 0)
#Exhibit 1
table(tree.test.pred, test$adopt)
#prune tree
set.seed(123)
cv.train <- cv.tree(tree.train, FUN= prune.misclass,K=10)
plot(cv.train$size, cv.train$dev, type = 'b')#find best nodes
prune.train <- prune.misclass(tree.train, best = 2)#apply best nodes(dont have one)
#prune tree predict test
prune.test.pred <- predict(prune.train, test, type = 'class')
#Exhibit 2
plot(prune.train)
text(prune.train, pretty = 0)
#result
table(prune.test.pred, test$adopt)
summary(tree.train)
summary(prune.train)

#Bagging
set.seed(123)
bag <- randomForest(train$adopt~., data = train, mtry=20, importance=TRUE)
bag
pred.bag <- predict(bag, newdata = test)
bag.test <- df[-inTrain, 'adopt']
bag.test <- df[-inTrain, 'adopt']
#table
(c = table(bag.test,pred.bag))
#accuracy
(acc = (c[1,1]+c[2,2])/sum(c))
#result
importance(bag)
varImpPlot(bag)

#random forest(m=4)
set.seed(123)
rf <- randomForest(train$adopt~., data = train, mtry=4, importance=TRUE)
rf
pred.rf <- predict(rf, newdata = test)
rf.test <- df[-inTrain, 'adopt']
#table
(c = table(rf.test,pred.rf))
#accuracy
(acc = (c[1,1]+c[2,2])/sum(c))
#result
importance(rf)
varImpPlot(rf)
############################
#boosting 
train$adopt <- as.numeric(train$adopt)-1
test$adopt <- as.numeric(test$adopt)-1
df$adopt <- as.numeric(df$adopt)-1

str(test)
set.seed(123)
boost <- gbm(train$adopt~., data = train, distribution = 'bernoulli', n.trees = 5000, interaction.depth = 4)
summary(boost)
plot(boost)
pred.boost <- predict(boost, newdata = test, n.trees = 5000, type = 'response')
predicted <- ifelse(pred.boost>=0.5, 1,0)
boost.test2 <- df[-inTrain, 'adopt']
(c = table(boost.test2,predicted))
(acc = (c[1,1]+c[2,2])/sum(c))

############################

#original data+state
#Read data
df <- read.csv("~/Desktop/Spring 2019/758T/Project/Data-final.csv")
#calculate description length
for (i in 1:14990){
  df$DesLen[i] <- nchar(as.character(df$Description[i]))
}
#Prepare data
df$Name <- NULL
df$RescuerID <- NULL
df$PetID <- NULL
df$Breed1 <- NULL
df$Breed2 <- NULL
df$adoptionspeed0.1 <- NULL
df$Description <- NULL
df$State <- NULL
#original data
df[32:40] <- NULL
df[18:29] <- NULL
#add Adopt
df$adopt <- ifelse(df$AdoptionSpeed<4,1,0)
df$AdoptionSpeed <- NULL

#
factor.cols <- c('Type','Gender','Color1','Color2','Color3','MaturitySize','FurLength','Vaccinated','Dewormed','Sterilized','Health','Quantity','adopt')                                                    
df[factor.cols] <- lapply(df[factor.cols], factor)
str(df)
#

# Split the data
set.seed(12345)
inTrain <- sample(nrow(df), 0.7*nrow(df))
train <- data.frame(df[inTrain,])
test <- data.frame(df[-inTrain,])
#

############################
#full tree, train data
tree.train <- tree(train$adopt~., data = train)
#full tree predict test
tree.test.pred <- predict(tree.train, test, type = 'class')
#result
plot(tree.train)
text(tree.train,pretty = 0)
#Exhibit 1
table(tree.test.pred, test$adopt)
#prune tree
set.seed(123)
cv.train <- cv.tree(tree.train, FUN= prune.misclass,K=10)
plot(cv.train$size, cv.train$dev, type = 'b')#find best nodes
prune.train <- prune.misclass(tree.train, best = 2)#apply best nodes(dont have one)
#prune tree predict test
prune.test.pred <- predict(prune.train, test, type = 'class')
#Exhibit 2
plot(prune.train)
text(prune.train, pretty = 0)
#result
table(prune.test.pred, test$adopt)
summary(tree.train)
summary(prune.train)

#Bagging
set.seed(123)
bag <- randomForest(train$adopt~., data = train, mtry=20, importance=TRUE)
bag
pred.bag <- predict(bag, newdata = test)
bag.test <- df[-inTrain, 'adopt']
bag.test <- df[-inTrain, 'adopt']
#table
(c = table(bag.test,pred.bag))
#accuracy
(acc = (c[1,1]+c[2,2])/sum(c))
#result
importance(bag)
varImpPlot(bag)

#random forest(m=4)
set.seed(123)
rf <- randomForest(train$adopt~., data = train, mtry=4, importance=TRUE)
rf
pred.rf <- predict(rf, newdata = test)
rf.test <- df[-inTrain, 'adopt']
#table
(c = table(rf.test,pred.rf))
#accuracy
(acc = (c[1,1]+c[2,2])/sum(c))
#result
importance(rf)
varImpPlot(rf)
############################
#boosting 
train$adopt <- as.numeric(train$adopt)-1
test$adopt <- as.numeric(test$adopt)-1
df$adopt <- as.numeric(df$adopt)-1

str(test)
set.seed(123)
boost <- gbm(train$adopt~., data = train, distribution = 'bernoulli', n.trees = 5000, interaction.depth = 4)
summary(boost)
plot(boost)
pred.boost <- predict(boost, newdata = test, n.trees = 5000, type = 'response')
predicted <- ifelse(pred.boost>=0.5, 1,0)
boost.test2 <- df[-inTrain, 'adopt']
(c = table(boost.test2,predicted))
(acc = (c[1,1]+c[2,2])/sum(c))

#original data+state
#Read data
df <- read.csv("~/Desktop/Spring 2019/758T/Project/Data-final.csv")
#calculate description length
for (i in 1:14990){
  df$DesLen[i] <- nchar(as.character(df$Description[i]))
}
#Prepare data
df$Name <- NULL
df$RescuerID <- NULL
df$PetID <- NULL
df$Breed1 <- NULL
df$Breed2 <- NULL
df$adoptionspeed0.1 <- NULL
df$Description <- NULL
df$State <- NULL
#original data
df[32:40] <- NULL
df[18:29] <- NULL
#add Adopt
df$adopt <- ifelse(df$AdoptionSpeed<4,1,0)
df$AdoptionSpeed <- NULL

#
factor.cols <- c('Type','Gender','Color1','Color2','Color3','MaturitySize','FurLength','Vaccinated','Dewormed','Sterilized','Health','Quantity','adopt')                                                    
df[factor.cols] <- lapply(df[factor.cols], factor)
str(df)
#

# Split the data
set.seed(12345)
inTrain <- sample(nrow(df), 0.7*nrow(df))
train <- data.frame(df[inTrain,])
test <- data.frame(df[-inTrain,])
#

############################

#original data+state+most popular
#Read data
df <- read.csv("~/Desktop/Spring 2019/758T/Project/Data-final.csv")
#calculate description length
for (i in 1:14990){
  df$DesLen[i] <- nchar(as.character(df$Description[i]))
}
#Prepare data
df$Name <- NULL
df$RescuerID <- NULL
df$PetID <- NULL
df$Breed1 <- NULL
df$Breed2 <- NULL
df$adoptionspeed0.1 <- NULL
df$Description <- NULL
df$State <- NULL
#original data
df[34:40] <- NULL
df[18:29] <- NULL
#add Adopt
df$adopt <- ifelse(df$AdoptionSpeed<4,1,0)
df$AdoptionSpeed <- NULL

#
factor.cols <- c('Type','Gender','Color1','Color2','Color3','MaturitySize','FurLength','Vaccinated','Dewormed','Sterilized','Health','Quantity','adopt','most.popular')                                                    
df[factor.cols] <- lapply(df[factor.cols], factor)
str(df)
#

# Split the data
set.seed(12345)
inTrain <- sample(nrow(df), 0.7*nrow(df))
train <- data.frame(df[inTrain,])
test <- data.frame(df[-inTrain,])
#
############################
#full tree, train data
tree.train <- tree(train$adopt~., data = train)
#full tree predict test
tree.test.pred <- predict(tree.train, test, type = 'class')
#result
plot(tree.train)
text(tree.train,pretty = 0)
#Exhibit 1
table(tree.test.pred, test$adopt)
#prune tree
set.seed(123)
cv.train <- cv.tree(tree.train, FUN= prune.misclass,K=10)
plot(cv.train$size, cv.train$dev, type = 'b')#find best nodes
prune.train <- prune.misclass(tree.train, best = 2)#apply best nodes(dont have one)
#prune tree predict test
prune.test.pred <- predict(prune.train, test, type = 'class')
#Exhibit 2
plot(prune.train)
text(prune.train, pretty = 0)
#result
table(prune.test.pred, test$adopt)
summary(tree.train)
summary(prune.train)

#Bagging
set.seed(123)
bag <- randomForest(train$adopt~., data = train, mtry=20, importance=TRUE)
bag
pred.bag <- predict(bag, newdata = test)
bag.test <- df[-inTrain, 'adopt']
bag.test <- df[-inTrain, 'adopt']
#table
(c = table(bag.test,pred.bag))
#accuracy
(acc = (c[1,1]+c[2,2])/sum(c))
#result
importance(bag)
varImpPlot(bag)

#random forest(m=5)
set.seed(123)
rf <- randomForest(train$adopt~., data = train, mtry=5, importance=TRUE)
rf
pred.rf <- predict(rf, newdata = test)
rf.test <- df[-inTrain, 'adopt']
#table
(c = table(rf.test,pred.rf))
#accuracy
(acc = (c[1,1]+c[2,2])/sum(c))
#result
importance(rf)
varImpPlot(rf)
############################
#boosting 
train$adopt <- as.numeric(train$adopt)-1
test$adopt <- as.numeric(test$adopt)-1
df$adopt <- as.numeric(df$adopt)-1

str(test)
set.seed(123)
boost <- gbm(train$adopt~., data = train, distribution =  'bernoulli', n.trees = 5000, interaction.depth = 4)
summary(boost)
plot(boost)
pred.boost <- predict(boost, newdata = test, n.trees = 5000, type = 'response')
predicted <- ifelse(pred.boost>=0.5, 1,0)
boost.test2 <- df[-inTrain, 'adopt']
(c = table(boost.test2,predicted))
(acc = (c[1,1]+c[2,2])/sum(c))

############################

#original data+state+most popular+sentiment
#Read data
df <- read.csv("~/Desktop/Spring 2019/758T/Project/Data-final.csv")
#calculate description length
for (i in 1:14990){
  df$DesLen[i] <- nchar(as.character(df$Description[i]))
}
#Prepare data
df$Name <- NULL
df$RescuerID <- NULL
df$PetID <- NULL
df$Breed1 <- NULL
df$Breed2 <- NULL
df$adoptionspeed0.1 <- NULL
df$Description <- NULL
df$State <- NULL
#original data
df[34:38] <- NULL
df[18:29] <- NULL
#add Adopt
df$adopt <- ifelse(df$AdoptionSpeed<4,1,0)
df$AdoptionSpeed <- NULL

#
factor.cols <- c('Type','Gender','Color1','Color2','Color3','MaturitySize','FurLength','Vaccinated','Dewormed','Sterilized','Health','Quantity','adopt','most.popular')                                                    
df[factor.cols] <- lapply(df[factor.cols], factor)
str(df)
#

# Split the data
set.seed(12345)
inTrain <- sample(nrow(df), 0.7*nrow(df))
train <- data.frame(df[inTrain,])
test <- data.frame(df[-inTrain,])
#
############################
#full tree, train data
tree.train <- tree(train$adopt~., data = train)
#full tree predict test
tree.test.pred <- predict(tree.train, test, type = 'class')
#result
plot(tree.train)
text(tree.train,pretty = 0)
#Exhibit 1
table(tree.test.pred, test$adopt)
#prune tree
set.seed(123)
cv.train <- cv.tree(tree.train, FUN= prune.misclass,K=10)
plot(cv.train$size, cv.train$dev, type = 'b')#find best nodes
prune.train <- prune.misclass(tree.train, best = 2)#apply best nodes(dont have one)
#prune tree predict test
prune.test.pred <- predict(prune.train, test, type = 'class')
#Exhibit 2
plot(prune.train)
text(prune.train, pretty = 0)
#result
table(prune.test.pred, test$adopt)
summary(tree.train)
summary(prune.train)

#Bagging
set.seed(123)
bag <- randomForest(train$adopt~., data = train, mtry=20, importance=TRUE)
bag
pred.bag <- predict(bag, newdata = test)
bag.test <- df[-inTrain, 'adopt']
bag.test <- df[-inTrain, 'adopt']
#table
(c = table(bag.test,pred.bag))
#accuracy
(acc = (c[1,1]+c[2,2])/sum(c))
#result
importance(bag)
varImpPlot(bag)

#random forest(m=5)
set.seed(123)
rf <- randomForest(train$adopt~., data = train, mtry=5, importance=TRUE)
rf
pred.rf <- predict(rf, newdata = test)
rf.test <- df[-inTrain, 'adopt']
#table
(c = table(rf.test,pred.rf))
#accuracy
(acc = (c[1,1]+c[2,2])/sum(c))
#result
importance(rf)
varImpPlot(rf)
############################
#boosting 
train$adopt <- as.numeric(train$adopt)-1
test$adopt <- as.numeric(test$adopt)-1
df$adopt <- as.numeric(df$adopt)-1

str(test)
set.seed(123)
boost <- gbm(train$adopt~., data = train, distribution = 'bernoulli', n.trees = 5000, interaction.depth = 4)
summary(boost)
plot(boost)
pred.boost <- predict(boost, newdata = test, n.trees = 5000, type = 'response')
predicted <- ifelse(pred.boost>=0.5, 1,0)
boost.test2 <- df[-inTrain, 'adopt']
(c = table(boost.test2,predicted))
(acc = (c[1,1]+c[2,2])/sum(c))

############################
#original data+state+most popular+sentiment+pet features
#Read data
df <- read.csv("~/Desktop/Spring 2019/758T/Project/Data-final.csv")
#calculate description length
for (i in 1:14990){
  df$DesLen[i] <- nchar(as.character(df$Description[i]))
}
#Prepare data
df$Name <- NULL
df$RescuerID <- NULL
df$PetID <- NULL
df$Breed1 <- NULL
df$Breed2 <- NULL
df$adoptionspeed0.1 <- NULL
df$Description <- NULL
df$State <- NULL
#original data
df[34:38] <- NULL
#add Adopt
df$adopt <- ifelse(df$AdoptionSpeed<4,1,0)
df$AdoptionSpeed <- NULL

#
factor.cols <- c('Type','Gender','Color1','Color2','Color3','MaturitySize','FurLength','Vaccinated','Dewormed','Sterilized','Health','Quantity','adopt','most.popular')                                                    
df[factor.cols] <- lapply(df[factor.cols], factor)
str(df)
#

# Split the data
set.seed(12345)
inTrain <- sample(nrow(df), 0.7*nrow(df))
train <- data.frame(df[inTrain,])
test <- data.frame(df[-inTrain,])
#
############################
#full tree, train data
tree.train <- tree(train$adopt~., data = train)
#full tree predict test
tree.test.pred <- predict(tree.train, test, type = 'class')
#result
plot(tree.train)
text(tree.train,pretty = 0)
#Exhibit 1
table(tree.test.pred, test$adopt)
#prune tree
set.seed(123)
cv.train <- cv.tree(tree.train, FUN= prune.misclass,K=10)
plot(cv.train$size, cv.train$dev, type = 'b')#find best nodes
prune.train <- prune.misclass(tree.train, best = 2)#apply best nodes(dont have one)
#prune tree predict test
prune.test.pred <- predict(prune.train, test, type = 'class')
#Exhibit 2
plot(prune.train)
text(prune.train, pretty = 0)
#result
table(prune.test.pred, test$adopt)
summary(tree.train)
summary(prune.train)

#Bagging
set.seed(123)
bag <- randomForest(train$adopt~., data = train, mtry=20, importance=TRUE)
bag
pred.bag <- predict(bag, newdata = test)
bag.test <- df[-inTrain, 'adopt']
bag.test <- df[-inTrain, 'adopt']
#table
(c = table(bag.test,pred.bag))
#accuracy
(acc = (c[1,1]+c[2,2])/sum(c))
#result
importance(bag)
varImpPlot(bag)

#random forest(m=6)
set.seed(123)
rf <- randomForest(train$adopt~., data = train, mtry=6, importance=TRUE)
rf
pred.rf <- predict(rf, newdata = test)
rf.test <- df[-inTrain, 'adopt']
#table
(c = table(rf.test,pred.rf))
#accuracy
(acc = (c[1,1]+c[2,2])/sum(c))
#result
importance(rf)
varImpPlot(rf)
############################
#boosting 
train$adopt <- as.numeric(train$adopt)-1
test$adopt <- as.numeric(test$adopt)-1
df$adopt <- as.numeric(df$adopt)-1

str(test)
set.seed(123)
boost <- gbm(train$adopt~., data = train, distribution = 'bernoulli', n.trees = 5000, interaction.depth = 4)
summary(boost)
plot(boost)
pred.boost <- predict(boost, newdata = test, n.trees = 5000, type = 'response')
predicted <- ifelse(pred.boost>=0.5, 1,0)
boost.test2 <- df[-inTrain, 'adopt']
(c = table(boost.test2,predicted))
(acc = (c[1,1]+c[2,2])/sum(c))

############################
#original data+state+most popular+sentiment+PCA
#Read data
df <- read.csv("~/Desktop/Spring 2019/758T/Project/Data-final.csv")
#calculate description length
for (i in 1:14990){
  df$DesLen[i] <- nchar(as.character(df$Description[i]))
}
#Prepare data
df$Name <- NULL
df$RescuerID <- NULL
df$PetID <- NULL
df$Breed1 <- NULL
df$Breed2 <- NULL
df$adoptionspeed0.1 <- NULL
df$Description <- NULL
df$State <- NULL
#original data
df[18:29] <- NULL
#add Adopt
df$adopt <- ifelse(df$AdoptionSpeed<4,1,0)
df$AdoptionSpeed <- NULL

#
factor.cols <- c('Type','Gender','Color1','Color2','Color3','MaturitySize','FurLength','Vaccinated','Dewormed','Sterilized','Health','Quantity','adopt','most.popular')                                                    
df[factor.cols] <- lapply(df[factor.cols], factor)
str(df)
#

# Split the data
set.seed(12345)
inTrain <- sample(nrow(df), 0.7*nrow(df))
train <- data.frame(df[inTrain,])
test <- data.frame(df[-inTrain,])
#
############################
#full tree, train data
tree.train <- tree(train$adopt~., data = train)
#full tree predict test
tree.test.pred <- predict(tree.train, test, type = 'class')
#result
plot(tree.train)
text(tree.train,pretty = 0)
#Exhibit 1
table(tree.test.pred, test$adopt)
#prune tree
set.seed(123)
cv.train <- cv.tree(tree.train, FUN= prune.misclass,K=10)
plot(cv.train$size, cv.train$dev, type = 'b')#find best nodes
prune.train <- prune.misclass(tree.train, best = 2)#apply best nodes(dont have one)
#prune tree predict test
prune.test.pred <- predict(prune.train, test, type = 'class')
#Exhibit 2
plot(prune.train)
text(prune.train, pretty = 0)
#result
table(prune.test.pred, test$adopt)
summary(tree.train)
summary(prune.train)

#Bagging
set.seed(123)
bag <- randomForest(train$adopt~., data = train, mtry=20, importance=TRUE)
bag
pred.bag <- predict(bag, newdata = test)
bag.test <- df[-inTrain, 'adopt']
bag.test <- df[-inTrain, 'adopt']
#table
(c = table(bag.test,pred.bag))
#accuracy
(acc = (c[1,1]+c[2,2])/sum(c))
#result
importance(bag)
varImpPlot(bag)

#random forest(m=5)
set.seed(123)
rf <- randomForest(train$adopt~., data = train, mtry=5, importance=TRUE)
rf
pred.rf <- predict(rf, newdata = test)
rf.test <- df[-inTrain, 'adopt']
#table
(c = table(rf.test,pred.rf))
#accuracy
(acc = (c[1,1]+c[2,2])/sum(c))
#result
importance(rf)
varImpPlot(rf)
############################
#boosting 
train$adopt <- as.numeric(train$adopt)-1
test$adopt <- as.numeric(test$adopt)-1
df$adopt <- as.numeric(df$adopt)-1

str(test)
set.seed(123)
boost <- gbm(train$adopt~., data = train, distribution = 'bernoulli', n.trees = 5000, interaction.depth = 4)
summary(boost)
plot(boost)
pred.boost <- predict(boost, newdata = test, n.trees = 5000, type = 'response')
predicted <- ifelse(pred.boost>=0.5, 1,0)
boost.test2 <- df[-inTrain, 'adopt']
(c = table(boost.test2,predicted))
(acc = (c[1,1]+c[2,2])/sum(c))

