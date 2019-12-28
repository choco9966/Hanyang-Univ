install.packages("neuralnet")
install.packages("dummies")
install.packages("party")
install.packages("ROCR")
install.packages("e1071")
install.packages("klaR")
install.packages("caret")
install.packages("class")

library(neuralnet)
library(dummies)
library(party)
library(ROCR)
library(e1071)
library(klaR)
library(caret)
library(class)

Formula_full <- Y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x16+x17+x18+x19+x20+x21+x22

train <- read.csv('nomisstrain.csv')
validation <- read.csv('nomissvalidation.csv')
test <- read.csv('nomisstest.csv')

Y <- train$Y
dummy1 <- dummy(train$x1)
dummy2 <- dummy(train$x2)
dummy3 <- dummy(train$x3)
dummy4 <- dummy(train$x4)
dummy5 <- dummy(train$x5)
dummy6 <- dummy(train$x6)
dummy7 <- dummy(train$x7)
dummy8 <- dummy(train$x8)
dummy9 <- dummy(train$x9)
dummy10 <- dummy(train$x10)
dummy11 <- dummy(train$x11)
dummy12 <- dummy(train$x12)
dummy13 <- dummy(train$x13)
dummy14 <- dummy(train$x14)
dummy16 <- dummy(train$x16)
dummy17 <- dummy(train$x17)
dummy18 <- dummy(train$x18)
dummy19 <- dummy(train$x19)
dummy20 <- dummy(train$x20)
dummy21 <- dummy(train$x21)
dummy22 <- dummy(train$x22)
train_dummy <-cbind(Y, dummy1, dummy2, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8, dummy9, dummy10, dummy11, dummy12, dummy13, dummy14, dummy16, dummy17, dummy18, dummy19, dummy20, dummy21, dummy22)
train_dummy <- as.data.frame(train_dummy)

Y <- validation$Y
dummy1 <- dummy(validation$x1)
dummy2 <- dummy(validation$x2)
dummy3 <- dummy(validation$x3)
dummy4 <- dummy(validation$x4)
dummy5 <- dummy(validation$x5)
dummy6 <- dummy(validation$x6)
dummy7 <- dummy(validation$x7)
dummy8 <- dummy(validation$x8)
dummy9 <- dummy(validation$x9)
dummy10 <- dummy(validation$x10)
dummy11 <- dummy(validation$x11)
dummy12 <- dummy(validation$x12)
dummy13 <- dummy(validation$x13)
dummy14 <- dummy(validation$x14)
dummy16 <- dummy(validation$x16)
dummy17 <- dummy(validation$x17)
dummy18 <- dummy(validation$x18)
dummy19 <- dummy(validation$x19)
dummy20 <- dummy(validation$x20)
dummy21 <- dummy(validation$x21)
dummy22 <- dummy(validation$x22)
validation_dummy <-cbind(Y, dummy1, dummy2, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8, dummy9, dummy10, dummy11, dummy12, dummy13, dummy14, dummy16, dummy17, dummy18, dummy19, dummy20, dummy21, dummy22)
validation_dummy <- as.data.frame(validation_dummy)

Y <- test$Y
dummy1 <- dummy(test$x1)
dummy2 <- dummy(test$x2)
dummy3 <- dummy(test$x3)
dummy4 <- dummy(test$x4)
dummy5 <- dummy(test$x5)
dummy6 <- dummy(test$x6)
dummy7 <- dummy(test$x7)
dummy8 <- dummy(test$x8)
dummy9 <- dummy(test$x9)
dummy10 <- dummy(test$x10)
dummy11 <- dummy(test$x11)
dummy12 <- dummy(test$x12)
dummy13 <- dummy(test$x13)
dummy14 <- dummy(test$x14)
dummy16 <- dummy(test$x16)
dummy17 <- dummy(test$x17)
dummy18 <- dummy(test$x18)
dummy19 <- dummy(test$x19)
dummy20 <- dummy(test$x20)
dummy21 <- dummy(test$x21)
dummy22 <- dummy(test$x22)
test_dummy <-cbind(Y, dummy1, dummy2, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8, dummy9, dummy10, dummy11, dummy12, dummy13, dummy14, dummy16, dummy17, dummy18, dummy19, dummy20, dummy21, dummy22)
test_dummy <- as.data.frame(test_dummy)

train$x1 <- factor(train$x1)
train$x2 <- factor(train$x2)
train$x3 <- factor(train$x3)
train$x4 <- factor(train$x4)
train$x5 <- factor(train$x5)
train$x6 <- factor(train$x6)
train$x7 <- factor(train$x7)
train$x8 <- factor(train$x8)
train$x9 <- factor(train$x9)
train$x10 <- factor(train$x10)
train$x11<- factor(train$x11)
train$x12 <- factor(train$x12)
train$x13 <- factor(train$x13)
train$x14 <- factor(train$x14)
train$x16 <- factor(train$x16)
train$x17 <- factor(train$x17)
train$x18 <- factor(train$x18)
train$x19 <- factor(train$x19)
train$x20 <- factor(train$x20)
train$x21 <- factor(train$x21)
train$x22 <- factor(train$x22)

validation$x1 <- factor(validation$x1)
validation$x2 <- factor(validation$x2)
validation$x3 <- factor(validation$x3)
validation$x4 <- factor(validation$x4)
validation$x5 <- factor(validation$x5)
validation$x6 <- factor(validation$x6)
validation$x7 <- factor(validation$x7)
validation$x8 <- factor(validation$x8)
validation$x9 <- factor(validation$x9)
validation$x10 <- factor(validation$x10)
validation$x11<- factor(validation$x11)
validation$x12 <- factor(validation$x12)
validation$x13 <- factor(validation$x13)
validation$x14 <- factor(validation$x14)
validation$x16 <- factor(validation$x16)
validation$x17 <- factor(validation$x17)
validation$x18 <- factor(validation$x18)
validation$x19 <- factor(validation$x19)
validation$x20 <- factor(validation$x20)
validation$x21 <- factor(validation$x21)
validation$x22 <- factor(validation$x22)

test$x1 <- factor(test$x1)
test$x2 <- factor(test$x2)
test$x3 <- factor(test$x3)
test$x4 <- factor(test$x4)
test$x5 <- factor(test$x5)
test$x6 <- factor(test$x6)
test$x7 <- factor(test$x7)
test$x8 <- factor(test$x8)
test$x9 <- factor(test$x9)
test$x10 <- factor(test$x10)
test$x11<- factor(test$x11)
test$x12 <- factor(test$x12)
test$x13 <- factor(test$x13)
test$x14 <- factor(test$x14)
test$x16 <- factor(test$x16)
test$x17 <- factor(test$x17)
test$x18 <- factor(test$x18)
test$x19 <- factor(test$x19)
test$x20 <- factor(test$x20)
test$x21 <- factor(test$x21)
test$x22 <- factor(test$x22)

##decision Tree

train_ctree <- ctree(Formula_full, data=train)
testPred <- predict(train_ctree, newdata=validation[,-1])
y_predict <- ifelse(testPred > 0.587 , 1 , 0 )
table_tree <- table(y_predict , validation[,1])
sum( diag(table_tree) )/sum( table_tree ) #accuracy
sum( table_tree[2,2] / sum( table_tree[2,1] + table_tree[2,2] ) ) #sensitivity
sum( table_tree[1,1] / sum( table_tree[1,1] + table_tree[1,2] )  ) #specificity

##logistic regresstion

model<- glm(Y~., family="binomial" (link='logit'), data=train)
y_predict <- predict(model, newdata = validation[,-1], type= 'response')
y_predict <- ifelse(y_predict > 0.695,1,0)
table_logistic <- table(y_predict , validation[,1])
sum( diag(table_logistic) )/sum( table_logistic ) #accuracy
sum( table_logistic[2,2] / sum( table_logistic[2,1] + table_logistic[2,2] ) ) #sensitivity
sum( table_logistic[1,1] / sum( table_logistic[1,1] + table_logistic[1,2] )  ) #specificity

##svm

tune.svm(Y~. , data=train , gamma=2^(-5:5) , cost=2^(-4:4))
svm.model <- svm(Y ~ . , data = train , cost=16, gamma =0.0625)
y_predict <- predict(svm.model, validation[,-1])
y_predict <- ifelse(svm.pred > 0.587 , 1, 0 )
table_svm <- table(y_predict , validation[,1])
sum( diag(table_svm) )/sum( table_svm ) #accuracy
sum( table_svm[2,2] / sum( table_svm[2,1] + table_svm[2,2] ) ) #sensitivity
sum( table_svm[1,1] / sum( table_svm[1,1] + table_svm[1,2] )  ) #specificity

##naive bayesian

nb.model <- train(train_dummy, as.factor(train$Y),
                  'nb',trControl=trainControl(method='cv',number=10))
y_predict <- predict(nb.model, validation_dummy)
y_predict <- as.numeric(as.matrix(y_predict))
table_nb <- table(y_predict , validation[,1])
sum( diag(table_nb) )/sum( table_nb ) #accuracy
sum( table_nb[2,2] / sum( table_nb[2,1] + table_nb[2,2] ) ) #sensitivity
sum( table_nb[1,1] / sum( table_nb[1,1] + table_nb[1,2] )  ) #specificity

##neural Network

dummy_full <- Y ~ x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x21 +
  x22 + x23 + x24 + x25 + x26 + x31 + x32 + x33 + x34 + x35 + x36 +
  x37 + x40 + x41 + x51 + x52 + x53 + x54 + x55 + x56 + x57 + x58 +
  x59 + x61 + x62 + x63 + x64 + x65 + x66 + x71 + x72 + x73 + x74 +
  x81 + x82 + x83 + x84 + x85 + x86 + x87 + x88 + x89 + x810 + x91 +
  x92 + x101 + x102 + x104 + x106 + x107 + x111 + x112 + x113 + x114 + x121 + 
  x122 + x123 + x124 + x131 + x132 + x133 + x134 + x135 + x136 + x137 + x138 +
  x139 + x141 + x142 + x143 + x144 + x145 + x146 + x147 + x148 + x149 + x161 +
  x162 + x163 + x164 + x171 + x172 + x173 + x182 + x183 + x184 + x185 + x186 +
  x191 + x193 + x201 + x202 + x211 + x212 + x221 + x222 + x223 + x224 + x225 +
  x226 + x227 + x228 + x229 + x2210 + x2211 + x2212

nn.model <- vector(mode="list")
nn.pred <- vector(mode="list")
nn.table <- vector(mode="list")

accuracy <- vector()
sensitivity <- vector()
specificity <- vector()

for(i in 1:10){
  
  nn.model[[i]] <- neuralnet(dummy_full, data = train_dummy, hidden=i, threshold=0.01, stepmax = 1e+07)
  nn.pred[[i]] <- compute(nn.model[[i]], validation_dummy[,-1])
  nn.pred[[i]] <- nn.pred[[i]]$net.result
  
  for(j in 1:nrow(validation)) {
    if(nn.pred[[i]][j] > 0.5)
      nn.pred[[i]][j] <- 1
    else
      nn.pred[[i]][j] <- 0
  }
  
  nn.table[[i]] <- table(validation[,1], nn.pred[[i]])
  accuracy[i] <- sum( diag(nn.table[[i]]) )/sum( nn.table[[i]] ) #accuracy
  sensitivity[i] <- sum( nn.table[[i]][2,2] / sum( nn.table[[i]][2,1] + nn.table[[i]][2,2] ) ) #sensitivity
  specificity[i] <- sum( nn.table[[i]][1,1] / sum( nn.table[[i]][1,1] + nn.table[[i]][1,2] ) ) #specificity
}

nn.pred_val_sum <- rep(0 , 1624)

for(i in 1:10){
  
  for(j in 1:nrow(validation)) {
    nn.pred_val_sum[j] <- nn.pred_val_sum[j] + nn.pred[[i]][j]
  }
  
}

for(i in 1:nrow(validation)) {
  if(nn.pred_val_sum[i] > 5)
    nn.pred_val_sum[i] <- 1
  else
    nn.pred_val_sum[i] <- 0
}
y_predict <- nn.pred_val_sum
table_nn <- table(y_predict , validation[,1])
sum( diag(table_nn) )/sum( table_nn ) #accuracy
sum( table_nn[2,2] / sum( table_nn[2,1] + table_nn[2,2] ) ) #sensitivity
sum( table_nn[1,1] / sum( table_nn[1,1] + table_nn[1,2] )  ) #specificity

##knn k=3

y_predict <- knn(train_dummy[,-1], validation_dummy[,-1], train_dummy[,1], k = 3, prob=FALSE)
y_predict <- as.numeric(as.matrix(knn.pred))
table_nn3 <- table(y_predict, validation[,1])
sum( diag(table_nn3) )/sum( table_nn3 ) #accuracy
sum( table_nn3[2,2] / sum( table_nn3[2,1] + table_nn3[2,2] ) ) #sensitivity
sum( table_nn3[1,1] / sum( table_nn3[1,1] + table_nn3[1,2] )  ) #specificity

##knn k=4

y_predict <- knn(train_dummy[,-1], validation_dummy[,-1], train_dummy[,1], k = 4, prob=FALSE)
y_predict <- as.numeric(as.matrix(knn.pred))
table_nn4 <- table(y_predict, validation[,1])
sum( diag(table_nn4) )/sum( table_nn4 ) #accuracy
sum( table_nn4[2,2] / sum( table_nn4[2,1] + table_nn4[2,2] ) ) #sensitivity
sum( table_nn4[1,1] / sum( table_nn4[1,1] + table_nn4[1,2] )  ) #specificity

##knn k=5

x62 <- rep(0,1500)
x62 <- as.integer(x62)
x72 <- rep(0,1500)
x72 <- as.integer(x72)
x85 <- rep(0,1500)
x85 <- as.integer(x85)

test_dummy <- cbind(test_dummy[,1:34], x62, test_dummy[,35:39], x72 ,test_dummy[40:45], x85 , test_dummy[46:114])

y_predict_test_knn4 <- knn(train_dummy[,-1], test_dummy[,-1], train_dummy[,1], k = 4, prob=FALSE)
y_predict_test_knn4 <- as.data.frame(y_predict)

write.table(y_predict_test_knn4, "C:/Users/Hyunwoo Jin/Documents/3학년 1학기/데마 팀플/Last data/result.txt",
            sep = ",",
            row.names=FALSE,
            quote=FALSE,
            append=TRUE)
