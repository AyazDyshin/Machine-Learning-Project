library(ISLR)
str(Caravan)
library(rpart)
library(ROCR)
library(randomForest)
library(glmnet)
library("writexl")
data <- Caravan
data1 <- Caravan

set.seed(123)
s = sample(5822)

data.train1 = data[s[1:4822], ]                 # training examples 
data.test1  = data[s[4823:5822], ]  

data.train<-data.train1
data.test<-data.test1

data.train$Purchase <- ifelse(data.train$Purchase == "Yes", 1, 0)
data.test$Purchase <- ifelse(data.test$Purchase == "Yes", 1, 0)

totalNumberYes <- nrow(data[data$Purchase == "Yes",])
total<- nrow(data)
precision.1 <- signif(totalNumberYes/total * 100,digits=3)

vec.firstTable <- vector()
for (i in 1:10){
   tempVec <- data[data$MOSHOOFD == i, ]
   temp.num.elems <- nrow(tempVec)
   temp.num.ofYes <- nrow(tempVec[tempVec$Purchase == "Yes",])
   temp.num.ofNo <-nrow(tempVec[tempVec$Purchase == "No",])
   temp.temp <- c(temp.num.elems,signif(temp.num.ofYes/temp.num.elems * 100,digits=3))
  vec.firstTable <- rbind(vec.firstTable,temp.temp)
}
param <- paste0("L1.",1:10)
row.names(vec.firstTable)<-param
colnames(vec.firstTable)<-c("number in group","percentage of purchased")


vec.SecondTable <- vector()
for (i in 1:41){
   tempVec <- data[data$MOSTYPE == i, ]
   temp.num.elems <- nrow(tempVec)
   temp.num.ofYes <- nrow(tempVec[tempVec$Purchase == "Yes",])
   temp.num.ofNo <-nrow(tempVec[tempVec$Purchase == "No",])
   temp.temp <- c(temp.num.elems,signif(temp.num.ofYes/temp.num.elems * 100,digits=3))
   vec.SecondTable <- rbind(vec.SecondTable,temp.temp)
}
param <- paste0("L0.",1:41)
row.names(vec.SecondTable)<-param
colnames(vec.SecondTable)<-c("number in group","percentage of purchased")
tempVeci <- data[data$MOSTYPE == 15, ]
testim <- nrow(tempVec[tempVeci$Purchase == "Yes",])

## Question 1b 
vec.ThirdTable <- vector()
for (j in 1:41){

tempMostype <- data [data$MOSTYPE == j, ]

vec.inner <- vector ()
for (i in 1:10){
   tempVec <- tempMostype [tempMostype$MOSHOOFD == i, ]
   temp.num.elems <- nrow(tempVec)
   vec.inner <- append(vec.inner,temp.num.elems)
}
vec.ThirdTable <- rbind(vec.ThirdTable, vec.inner)
}
ourdf <- data.frame(pipo)

write_xlsx(ourdf, "C:\\Users\\Shadthepro\\Downloads\\here\\pttp.xlsx")
## Question 2 preparing data
prepare_cv_folds = function(n,k){
   fold.size = n %/% k
   set.seed(12); s = sample(n)    
   f.idx = list()
   for(i in 1:k){
      f.idx[[i]] = s[(1 + (i-1)*fold.size):(i*fold.size)]
      # cat("Fold_", i, "\t", (1 + (i-1)*fold.size), ":", (i*fold.size), " = ", length(f.idx[[i]]), "\n", sep="")
   }
   return(f.idx)
}
trainYes <- data.train[data.train$Purchase == 1,]
trainNo <- data.train[data.train$Purchase == 0,]
trainYes1 <- data.train1[data.train1$Purchase == "Yes",]
trainNo1 <- data.train1[data.train1$Purchase == "No",]
for (i in 1:4){
   trainYes[nrow(trainYes)+1,]<-NA
}

for (i in 1:4){
   trainNo[nrow(trainNo)+1,]<-NA
}

for (i in 1:4){
   trainYes1[nrow(trainYes1)+1,]<-NA
}

for (i in 1:4){
   trainNo1[nrow(trainNo1)+1,]<-NA
}
folds.test.Yes.id = prepare_cv_folds(290, 10)
folds.test.No.id = prepare_cv_folds(4540,10)



auc.cv = numeric(10)
dfofCI.dt <- data.frame()
means.dt = numeric (20)
sds.dt = numeric (20)


# question 2 a decision tree

cutoff.seq.dt = seq(0.001, 0.02, by=0.001)
for (i in 1:20){
   for (k in 1:10){
      ## preparing data temp train:
      tempTrain1 <- trainYes1[- folds.test.Yes.id[[k]],]
      tempTrain2 <- trainNo1[-folds.test.No.id[[k]],]
      tempTrain.u <- rbind (tempTrain1,tempTrain2)
      tempTrain <- tempTrain.u[rowSums(is.na(tempTrain.u)) !=ncol(tempTrain.u),]
      ## temp test:
      tempTest1 <- trainYes1[folds.test.Yes.id[[k]],]
      tempTest2 <- trainNo1[folds.test.No.id[[k]],]
      tempTest.u <- rbind (tempTest1,tempTest2)
      tempTest <- tempTest.u[rowSums(is.na(tempTest.u)) !=ncol(tempTest.u),]
      
      tempTrain$Purchase = as.factor(tempTrain$Purchase)
      
      currTree = rpart(Purchase ~., tempTrain,cp=cutoff.seq.dt[i])
      
      currPredict <- predict(currTree, tempTest, type = "class")
      y.pred.tree.2 <- as.numeric(currPredict)
      currPrediction <- prediction(y.pred.tree.2, as.numeric(tempTest$Purchase))
      auc_ROCR <- performance(currPrediction, measure = "auc",fpr.stop=0.2)
      auc_ROCR <- auc_ROCR@y.values[[1]]
      auc.cv[k] = auc_ROCR
   }
   means.dt[i]= mean (auc.cv)
   sds.dt[i] = sd(auc.cv)
   cf = t.test(auc.cv)
   dfofCI.dt = rbind(dfofCI.dt,cf$conf.int)
}
plot(cutoff.seq.dt,means.dt,col="red",type="l",xlab="cp values",ylab="mean of AUC",main="Decision Tree")
for (i in 1:20){
   cutoff.seq.dt[i]<-signif(cutoff.seq.dt[i],4)
}
tableDT <- rbind(cutoff.seq.dt,means.dt)
tableDT <- rbind(tableDT,sds.dt)
tableDT1 <-t(dfofCI.dt)
tableDT <-rbind(tableDT,tableDT1)
row.names(tableDT)<-c("CP value","mean of AUC","Std. div","CI s","CI e")
lmao <-data.frame(tableDT)
write_xlsx(lmao, "C:\\Users\\Shadthepro\\Downloads\\here\\ppt.xlsx")


## question 2 b Random forest
trainYes1 <- data.train1[data.train1$Purchase == "Yes",]
trainNo1 <- data.train1[data.train1$Purchase == "No",]
for (i in 1:4){
   trainYes1[nrow(trainYes1)+1,]<-NA
}

for (i in 1:4){
   trainNo1[nrow(trainNo1)+1,]<-NA
}

dfofCI.rf <- data.frame()
means.rf = numeric (20)
sds.rf = numeric (20)
cutoff.seq.rf = seq(300,800,by=100)



for (i in 1:6){
   for (k in 1:10){
      ## preparing data temp train:
      tempTrain1 <- trainYes1[- folds.test.Yes.id[[k]],]
      tempTrain2 <- trainNo1[-folds.test.No.id[[k]],]
      tempTrain.u <- rbind (tempTrain1,tempTrain2)
      tempTrain <- tempTrain.u[rowSums(is.na(tempTrain.u)) !=ncol(tempTrain.u),]
      ## temp test:
      tempTest1 <- trainYes1[folds.test.Yes.id[[k]],]
      tempTest2 <- trainNo1[folds.test.No.id[[k]],]
      tempTest.u <- rbind (tempTest1,tempTest2)
      tempTest <- tempTest.u[rowSums(is.na(tempTest.u)) !=ncol(tempTest.u),]
      
      currTree = randomForest(Purchase ~ ., tempTrain, ntree = cutoff.seq.rf[i])
      currPredict <- predict(currTree, tempTest, type = "class")
      y.pred.tree.2 <- as.numeric(currPredict)
      currPrediction <- prediction(y.pred.tree.2, as.numeric(tempTest$Purchase))
      auc_ROCR <- performance(currPrediction, measure = "auc",fpr.stop=0.2)
      auc_ROCR <- auc_ROCR@y.values[[1]]
      auc.cv[k] = auc_ROCR
   }
   means.rf[i]= mean (auc.cv)
   sds.rf[i] = sd(auc.cv)
   cf = t.test(auc.cv)
   dfofCI.rf = rbind(dfofCI.rf,cf$conf.int)
}
means.rff <- means.rf[1:6]
plot(cutoff.seq.rf,means.rff,col="red",type="l",xlab="ntree",ylab="mean of AUC",main="Random forest ntree")
tableDT.rf <- rbind(cutoff.seq.rf,means.rff)
tableDT.rf <- rbind(tableDT.rf,sds.rf[1:6])
tableDT1.rf <-t(dfofCI.rf)
tableDT.rf <-rbind(tableDT.rf,tableDT1.rf)
row.names(tableDT.rf)<-c("CP value","mean of AUC","Std. div","CI s","CI e")
lmaoo <- data.frame(tableDT.rf)
write_xlsx(lmaoo, "C:\\Users\\Shadthepro\\Downloads\\here\\pptt.xlsx")

## random forest mtry 
dfofCI.rf.mtry <- data.frame()
means.rf.mtry = numeric (20)
sds.rf.mtry = numeric (20)
cutoff.seq.rf.mtry = seq(10,12,by=1)

for (i in 1:3){
   for (k in 1:10){
      ## preparing data temp train:
      tempTrain1 <- trainYes1[- folds.test.Yes.id[[k]],]
      tempTrain2 <- trainNo1[-folds.test.No.id[[k]],]
      tempTrain.u <- rbind (tempTrain1,tempTrain2)
      tempTrain <- tempTrain.u[rowSums(is.na(tempTrain.u)) !=ncol(tempTrain.u),]
      ## temp test:
      tempTest1 <- trainYes1[folds.test.Yes.id[[k]],]
      tempTest2 <- trainNo1[folds.test.No.id[[k]],]
      tempTest.u <- rbind (tempTest1,tempTest2)
      tempTest <- tempTest.u[rowSums(is.na(tempTest.u)) !=ncol(tempTest.u),]
      
      currTree = randomForest(Purchase ~ ., tempTrain, ntree = 600, mtry = cutoff.seq.rf.mtry[i])
      currPredict <- predict(currTree, tempTest, type = "class")
      y.pred.tree.2 <- as.numeric(currPredict)
      currPrediction <- prediction(y.pred.tree.2, as.numeric(tempTest$Purchase))
      auc_ROCR <- performance(currPrediction, measure = "auc",fpr.stop=0.2)
      auc_ROCR <- auc_ROCR@y.values[[1]]
      auc.cv[k] = auc_ROCR
   }
   means.rf.mtry[i]= mean (auc.cv)
   sds.rf.mtry[i] = sd(auc.cv)
   cf = t.test(auc.cv)
   dfofCI.rf.mtry = rbind(dfofCI.rf,cf$conf.int)
}

plot(cutoff.seq.rf.mtry,means.rf.mtry[1:3],col="red",type="l",xlab="mtry",ylab="mean of AUC",main="Random forest mtry")



## question 2 c
## test 



a1.matrix<-model.matrix(data.train1$Purchase~., data=data.train1)[,-1]
a1.test.matrix<-data.matrix(data.test1[1:85])
currTut = glmnet(a1.matrix, data.train1$Purchase,alpha=0.1,lambda=0.013, family = binomial)
test.predict <- predict(currTut, newx=a1.test.matrix,type="response")
test.pred <- prediction(test.predict,data.test1$Purchase)
auc_ROCR.a1 <- performance(test.pred, measure = "auc",fpr.stop=0.2)
perf.dt = performance(test.pred,"tpr","fpr",fpr.stop=0.2)
plot(perf.dt)

## alpha = 1
seqAlpha = seq(0,1,by=0.1)
AUCL = numeric(10)
bestlambda = numeric(10)




a1.matrix<-model.matrix(data.train1$Purchase~., data=data.train1)[,-1]
a1.model<-glmnet(a1.matrix, data.train1$Purchase, alpha=0, family=binomial)

set.seed(123)

a1.model.CV <- cv.glmnet(a1.matrix, data.train1$Purchase, alpha=0,k=10, family=binomial("logit"))


lambda_min.a1 <- a1.model.CV$lambda.min
bestlambda[i]= lambda_min.a1

a1.test.matrix<-data.matrix(data.test1[1:85])
a1.predict<-predict(a1.model.CV, s=lambda_min.a1,newx=a1.test.matrix,type="response")

a1.pred <- prediction(a1.predict,data.test1$Purchase)

auc_ROCR <- performance(a1.pred, measure = "auc",fpr.stop=0.2)


## alpha 0 
a0.matrix<-model.matrix(data.train1$Purchase~., data=data.train1)[,-1]
a0.model<-glmnet(a0.matrix, data.train1$Purchase, alpha=0, family=binomial)
plot(a0.model, xvar="lambda")

set.seed(123)

a0.model.CV <- cv.glmnet(a0.matrix, data.train1$Purchase, alpha=0,k=10, family=binomial("logit"))
plot(a0.model.CV)

lambda_min.a0 <- a0.model.CV$lambda.min

a0.test.matrix<-data.matrix(data.test1[1:85])
a0.predict<-predict(a0.model.CV, s=lambda_min.a0,newx=a0.test.matrix,type="response")

a0.pred <- prediction(a0.predict,data.test1$Purchase)

auc_ROCR.a0 <- performance(a0.pred, measure = "auc",fpr.stop=0.2)
auc_ROCR.a00 <- auc_ROCR.a0@y.values[[1]]

## question 2 d 
## DT with optimal parameters 
currTree.dt = rpart(Purchase ~., data.train1,cp=0.001)
currPredict.dt <- predict(currTree.dt, data.test1, type = "class")
y.pred.tree.2.dt <- as.numeric(currPredict.dt)
currPrediction.dt <- prediction(y.pred.tree.2.dt, as.numeric(data.test1$Purchase))
perf.dt = performance(currPrediction.dt,"tpr","fpr",fpr.stop=0.2)
plot(perf.dt)



## RF 

currTree.rf = randomForest(Purchase ~ .,data.train1, ntree = 600, mtry = 11)
currPredict.rf <- predict(currTree.rf, data.test1, type = "class")
y.pred.tree.2.rf <- as.numeric(currPredict.rf)
currPrediction.rf <- prediction(y.pred.tree.2.rf, as.numeric(data.test1$Purchase))
perf.rf = performance(currPrediction.rf,"tpr","fpr",fpr.stop=0.2)
plot(perf.rf)



## glm

x.g <- model.matrix(Purchase ~ ., data = data.train)
y.g <- data.matrix(data.train$Purchase)

x.t <- model.matrix(Purchase ~ ., data = data.test)
y.t <- data.matrix(data.train$Purchase)


lasso <- glmnet(x.g, y.g,
                alpha = 1,
                family = "gaussian",
                lambda = grid)

pred.lasso.g <- predict(lasso, newx = x.t, s = 0)
newPred.g <- prediction(pred.lasso.g,data.test$Purchase)
perf.g = performance(newPred.g,"tpr","fpr",fpr.stop=0.2)
plot(perf.g,  lwd = 2)

## question 3
importance(currTree.rf)
## question 4

blindTest <- read.csv("https://ufal.mff.cuni.cz/~holub/2021/docs/caravan.test.1000.csv", sep="\t", header=FALSE)
dim(blindTest)

blindTest.matrix<-data.matrix(blindTest)
wholeData <- Caravan

a1.matrix<-model.matrix(wholeData$Purchase~., data=wholeData)[,-1]

a1.model.CV <- cv.glmnet(a1.matrix, wholeData$Purchase, alpha=0.1,k=10, family=binomial("logit"))

lambda.min <- a1.model.CV$lambda.min

fff<-predict(a1.model.CV, s=lambda.min,newx=blindTest.matrix,type="response")

gg <- fff
tempoo<-fff[order(fff,decreasing = TRUE)][100]
gg[gg>=tempoo]<-1
gg[gg<tempoo]<-0
## Create file
write.table(gg,file="Titprediction.txt",row.names = FALSE)



   
  