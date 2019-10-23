
iris_rand <- iris[sample(150,150),] 

irisclass <- iris_rand[,5]
irisvalues <- iris_rand[,-5]

# training set
irisclasstrain <- irisclass[1:100]
irisvaluestrain <- irisvalues[1:100,]

#test set
irisclasstest <- irisclass[100:150]
irisvaluestest <- irisvalues[100:150,]

#install.packages("rpart")
library(rpart)
fit <- rpart(irisclasstrain~., method="class", data= irisvaluestrain) 

plot(fit, uniform=TRUE, main="Decision Tree for iris data")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

treepred <- predict(fit,irisvaluestest, type = 'class')
nbtestcases <-length(irisclasstest)
nbcorrect <- sum(treepred == irisclasstest) 
accuracy <- nbcorrect/nbtestcases
print(accuracy)

table_mat <- table(irisclasstest, treepred)
print(table_mat)

pfit <- prune(fit, cp=0.1)
pfit2 <- prune(fit, cp=0.5)
plot(pfit, uniform=TRUE, main='pruned decision tree for iris')
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
plot(pfit2, uniform=TRUE, main='pruned decision tree2 for iris')
text(pfit2, use.n=TRUE, all=TRUE, cex=.8)

var1 <- irisvalues[,2]
var2 <- irisvalues[,3]

plot(var2, var1, col=treepred)
#library(class)

knn3pred <- knn(irisvaluestrain, irisvaluestest, irisclasstrain, k=3)
knn5pred <- knn(irisvaluestrain, irisvaluestest, irisclasstrain, k=5)

n <- length(irisclasstest) # nb test cases
ncorrect <- sum(knn3pred==irisclasstest) #nb correct
ncorr <- sum(knn5pred == irisclasstest)
predictedaccurcy <- ncorrect/n
predacc <- ncorr/n
print(predacc)
print(accuracy)


