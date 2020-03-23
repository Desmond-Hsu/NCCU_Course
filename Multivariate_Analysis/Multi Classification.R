# Packages ----------------------------------------------------------------####
library(readr)
library(ggplot2)
library(ggcorrplot)
library(MVN)
library(MASS) #lda,qda
library(klaR)
library(rpart) #decision tree
library(rpart.plot) #plotting tree
library(caret) #cv
library(class) #knn
library(glmnet) #logistic discrimination
library(e1071) #SVM, naiveBayes
library(randomForest)
library(adabag)
# Preprocessing -----------------------------------------------------------####
Credit <- read.csv("D:/Desmond/Lesson/1072/Multivariate Analysis/midterm/data/Credit.csv")
data <- Credit[,c("Income","Limit","Rating","Cards","Age","Education","Ethnicity","Balance")]
data$Ethnicity <- as.factor(data$Ethnicity)
data_x <- data[-7]
sd_data_x <- scale(data_x)
sd_data_x <- data.frame(sd_data_x)
test <- read.table("D:/Desmond/Lesson/1072/Multivariate Analysis/midterm/data/test.txt",h=T)
test$Gender <- as.numeric(test$Gender)
test$Student <- as.numeric(test$Student)
test$Married <- as.numeric(test$Married)
# Correlation   -----------------------------------------------------------####
data_plotcor <- data
data_plotcor$Ethnicity <- as.numeric(data_plotcor$Ethnicity)
corr <- round(cor(data_plotcor), 3)
ggcorrplot(corr, hc.order = TRUE,lab = TRUE, colors = c("#6D9EC1", "white", "#E46726"))
ggcorrplot(corr, hc.order = TRUE, type = "upper",lab = TRUE)
res <- cor(data_plotcor)
round(res, 2)


# Multivariate Normal Test ------------------------------------------------####
marida_result <- mvn(data_x, mvnTest = "mardia")
marida_result$multivariateNormality
hz_result <- mvn(data_x, mvnTest = "hz")
hz_result$multivariateNormality
royston_result <- mvn(data_x, mvnTest = "royston")
royston_result$multivariateNormality
# df_result <- mvn(data, mvnTest = "dh")
# df_result$multivariateNormality
# energy_result <- mvn(data, mvnTest = "energy")
# energy_result$multivariateNormality
mvn(Credit , mvnTest = "royston", univariatePlot = "qqplot")
mvn(data_x , mvnTest = "royston", univariatePlot = "histogram")


# EDA ---------------------------------------------------------------------####
ggplot(data,aes(x='Ethnicity'))+geom_histogram(stat = 'count')


# Decision Tree -----------------------------------------------------------####
set.seed(87)
rpart_control <- rpart.control(minisplit=10,minbucket=4,xval=10)
tree_model <- rpart(Ethnicity~.,data,method = 'class',control = rpart_control)
plot(tree_model)
text(tree_model)
printcp(tree_model)
plotcp(tree_model)
summary(tree_model)
prp(tree_model,faclen=0,fallen.leaves=TRUE,shadow.col="gray",extra=2)#plot
prune_tree_model <- prune(tree_model,cp=0.02)
printcp(prune_tree_model)
#cv
control <- trainControl(method="cv", number=10)
tree_control_model <- train(Ethnicity~.,data,method="rpart",trControl=control)
tree_control_model


# LDA ---------------------------------------------------------------------####
set.seed(87)
lda_model <- lda(Ethnicity~.,data)
lda_values <- predict(lda_model,data[c("Income","Limit","Rating","Cards","Age","Education","Balance")])
lda_model$counts
#cv
control <- trainControl(method="cv",number=10)
cv_ldamodel <- train(Ethnicity~.,data,method="lda",trControl=control)
#plot
ggplot(newdata)+geom_point(aes(lda.LD1,lda.LD2,colour=Ethnicity),size=2.5)
table(data["Ethnicity"],lda_values$class)
plot(lda_model,col=as.integer(data$Ethnicity))


# QDA ---------------------------------------------------------------------####
set.seed(87)
qda_model <- qda(Ethnicity~.,data)
qda_predict <- predict(qda_model,data)
qda_cvmodel <- qda(Ethnicity~.,data,CV=TRUE) #default:leave one out
qda_cvpredict <- predict(qda_cvmodel,data)
table(data$Ethnicity,qda_predict$class)
table(data$Ethnicity,qda_cvmodel$class)
##由上述多元常態檢定拒絕H0，資料未服從常態分佈，在不滿足的前提下可預期QDA做出來的結果不會太好
##cv結果沒有優化模型
control <- trainControl(method="cv",number=10)
cv_qdamodel <- train(Ethnicity~.,data,method="qda",trControl=control)


# knn ----------------------------------------------------------------------####
n=80
result_knn <- matrix(0,nrow = n,ncol = 2)
for (i in 1:n){ 
  predicted_nn <- knn.cv(sd_data_x,data$Ethnicity,k=i,prob=T)
  confusion_matrix_nn<- table(data$Ethnicity, predicted_nn)
  true_rate <- sum(diag(confusion_matrix_nn))/sum(confusion_matrix_nn)
  print(c(i,true_rate))
  result_knn[i,1] <- i
  result_knn[i,2] <- 1-true_rate
}
plot(result_knn,xlab = "k", ylab = "error rate",type='l')
lines(x=40,col='red')
##由圖決定k=40有最小的true error rate:0.5


# Logistic Discrimination -------------------------------------------------####
x <- as.matrix(data_x)
logdis_cvmodel <- cv.glmnet(x,y=as.matrix(data$Ethnicity),family='multinomial',type.measure = 'class',nfolds = 10)
predicted_logdis <- predict(logdis_cvmodel,x,s = "lambda.min", type = "class")
confusion_matrix_logdis<- table(data$Ethnicity, predicted_logdis)
zero_matrix <- matrix(0,nrow=3,ncol = 2)
colnames(zero_matrix) <- c("African American","Asian")
confusion_matrix_logdis <- cbind(zero_matrix,confusion_matrix_logdis)
true_rate_logdis <- sum(diag(confusion_matrix_logdis))/sum(confusion_matrix_logdis)
#logistic discrimination true error rate為0.5025
#cv
control <- trainControl(method="cv",number=10)
cv_logdismodel <- train(Ethnicity~.,data,method="glm",trControl=control,family=binomial())


# SVM ---------------------------------------------------------------------####

model_svm <- svm(data_x,data$Ethnicity)
predicted_svm <- predict(svm1,test)
confusion_matrix_svm<- table(data$Ethnicity, predicted_svm)
true_rate <- sum(diag(confusion_matrix_svm))/sum(confusion_matrix_svm)
svm1<-svm(x=Credit[,-c(1,11)],y=Credit[,11],cost=10,gamma=9.4)
summary(svm1)
tune_svmmodel <- tune.svm(Ethnicity ~ Income+Limit+Rating+Cards+Age+Education+Balance+Gender+Student+Married, data=Credit, cost= 10*(1:10), gamma=0.1*(1:100))
plot(tune_svmmodel, xlab = "gamma", ylab="C")

control <- trainControl(method="cv",number=10)
cv_svmmodel <- train(Ethnicity~.,data,method="svmRadial",trControl=control)
cv_svmmodel


# Predict -----------------------------------------------------------------####
test <- read_table2("D:/Desmond/Lesson/1072/Multivariate Analysis/midterm/data/test.txt")
predict_test <- predict(svm1,test[c("Income","Limit","Rating","Cards","Age","Education","Balance")])

predict2_svm <-predict(svm1,Credit[,-c(1,11)]) 
table(Credit$Ethnicity,predict2_svm)

predict3_svm <- predict(svm1,newdata=test)
# RandomForest ------------------------------------------------------------####
set.seed(87)
#tune parameter
control <- trainControl(method="repeatedcv",number=10,repeats=3,search = 'grid')
tunegrid <- expand.grid(.mtry=c(1:6))
grid_rfmodel <- train(Ethnicity~.,metric='Accuracy',data,tuneGrid=tunegrid,trControl=control,method='rf')
plot(grid_rfmodel)
predicted_rf <- predict(grid_rfmodel,data)
table(data$Ethnicity, predicted_rf)

rf_model <- randomForest(Ethnicity~., importance=TRUE, nodesize=2, proximity=TRUE, data=data)
temp <- predict(grid_rfmodel,data)
rf <- train(Ethnicity~.,data,method='rf')
varImp(rf)



# Adaboost ----------------------------------------------------------------####
fish.control<-rpart.control(minsplit=10,minbucket=3,cp=0.01)
newfish.adaboost <- boosting(Ethnicity~Income+Limit+Rating+Cards+Age+Education+Balance, data=Credit, boos=F, mfinal=20,control=fish.control)
predicted_adaboost <- predict.boosting(newfish.adaboost,Credit)
table(data$Ethnicity, predicted_adaboost$class)
newfish.adaboost$class

# model_cvadaboost <- boosting.cv(Ethnicity~Income+Limit+Rating+Cards+Age+Education+Balance, data=Credit,v=10, boos=F, mfinal=20,control=fish.control)

# naiveBayes --------------------------------------------------------------
nb_model <- naiveBayes(Ethnicity~.,Credit[-1])
summary(nb_model)
predicted_nb <- predict(nb_model,newdata=Credit)
table(data$Ethnicity, predicted_nb)
