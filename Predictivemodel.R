##Load the necessary libraries and data
library(caret)
library(randomForest)
library(e1071)

Trn <- read.csv("Mlearn/pml-training.csv", na.strings = c("", NA))
Tst <- read.csv("Mlearn/pml-testing.csv", na.strings = c("",NA))

##Analyze the data, look for NA's
dim(Trn)
natrn <- sapply(Trn, function(y) sum(length(which(is.na(y)))))
natst <- sapply(Tst, function(y) sum(length(which(is.na(y)))))
natrn <- data.frame(natrn)
natst <- data.frame(natst)


##several columns have more than 97% NA's, we will remove these columns
##for easier manipulation, as well as, columns 1:7 which are irrevelant 

Trn<- Trn[, which(natrn==0)]
Trn<- Trn[, -c(1:7)]

Tst<- Tst[, which(natst==0)]
Tst<- Tst[, -c(1:7)]
dim(Trn)
dim(Tst)

##The number of relevant variables has been reduced from 160 to 53. nearZeroVar was ran and not needed
##partitiion the data into training/test sets for model building

inTrain <- createDataPartition(y=Trn$classe, p=.6, list=F)
Train <- Trn[inTrain,]
Test <- Trn[-inTrain,]

##Build models see which is best

set.seed(98765)

##randomForest method

fitrf<- randomForest(classe~., data=Train, ntree=500, trcontrol=trainControl(method="cv"), importance=T)
fitrf 
##OOB error=.61%

predictrf <- predict(fitrf, Test)
confusionMatrix(predictrf, Test$classe)

# par(mfrow=c(4,2))
# par(mar=rep(2,4))
plot(fitrf, log="y", main="error rate vs trees")
legend("right", colnames(fitrf$err.rate),fill=1:6)

varImpPlot(fitrf,type = 2)


##model reran with top 25 variables

# rerun <- randomForest(classe~., data=Train, ntree=500, importance=T, mdim2nd=23, imp=1)
# 
# predictionss<- predict(rerun, Test)
# confusionMatrix(predictionss, Test$classe)
# 
# plot(rerun, log="y")
# legend("right", colnames(rerun$err.rate), fill=1:6)






answer <- predict(fitrf, Tst)
answer

mean(predict(fitrf, Test) == Test$classe)


pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(answer)








