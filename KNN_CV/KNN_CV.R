#### KNN with Cross Validation
library(class)
library(ggplot2)
#importing dataset
dataset = read.csv("sd_training_data.csv")

# cleaning
dataset = dataset[,-1]

ks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)

# Perform 10 fold cross validation
miss_scores = data.frame()
for (j in 1:30) {
  folds <- cut(seq(1,nrow(dataset)),breaks=10,labels=FALSE)
  for (i in 1:10){
    testIndexes = which(folds==i,arr.ind=TRUE)
    testData = dataset[testIndexes, ]
    trainData = dataset[-testIndexes, ]
    
    X_train = trainData[,2:52]
    Y_train = trainData[,1]
    
    X_val = testData[,2:52]
    Y_val = testData[,1]
    
    knn.pred = knn(train = X_train,
                   test = X_val,
                   cl = Y_train,
                   k = j,
                   prob = TRUE)
    miss[i] = mean(knn.pred != Y_val)
  }
  fold_miss[j] = mean(miss)
}
miss_scores <- cbind(fold_miss)

miss_scores <- cbind(miss_scores, ks)
miss_scores = as.data.frame(miss_scores)


ggplot(miss_scores, aes(x = ks)) + 
  geom_line(aes(y = fold_miss, color="red")) +
  ggtitle("KNN Misscalssification Scores") +
  ylab("Missclassifictaion Rate") +
  xlab("Number for K")

