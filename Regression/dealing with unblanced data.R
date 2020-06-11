install.packages("ROSE")
library(ROSE)

dt_un <- read.csv("D:/Desmond/Lesson/Regression/HW/Heart Disease Study/data/framingham.csv")
dt_un$education <- factor(dt_un$education)
dt_un$male <- factor(dt_un$male)
dt_un$currentSmoker <- factor(dt_un$currentSmoker)
dt_un$BPMeds<- factor(dt_un$BPMeds)
dt_un$prevalentStroke <- factor(dt_un$prevalentStroke)
dt_un$prevalentHyp <- factor(dt_un$prevalentHyp)
dt_un$diabetes <- factor(dt_un$diabetes)

data <- knnImputation(dt_un)


# Same proportion  --------------------------------------------------------
id <- which(data$TenYearCHD==1)
id_train <- sample(id,451, replace = FALSE)
id_0 <- which(data$TenYearCHD==0)
id_train0 <- sample(id_0, 2517, replace = FALSE)
id_select <- c(id_train,id_train0)
train <- data[id_select,]
test <- data[-id_select,]


# Smpling Method-----------------------------------------------------------------
data_over <- ovun.sample(TenYearCHD ~ ., train, method="over", N=5034)$data
data_under <- ovun.sample(TenYearCHD ~ ., train, method="under", N=902, seed=1)$data
data_both <- ovun.sample(TenYearCHD ~ ., train, method="both", N=2968, seed=1)$data




# SMOTE --------------------------------------------------------------------
data_rose <- ROSE(TenYearCHD ~ ., train, seed=1)$data



model_select <- glm(TenYearCHD~male+age+cigsPerDay+sysBP+glucose+BMI+prevalentStroke
                    # +education+prevalentHyp+prevalentStroke+diabetes+totChol+sysBP+BMI+heartRate
                    ,family="binomial",data = data_both)
prediction <- predict(model_select, type="response",newdata = test)
prediction_TF <- ifelse(prediction>0.5,1,0)
confusion_matrix <- table(test$TenYearCHD, prediction_TF)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
sensitivity <- confusion_matrix[2,2]/sum(confusion_matrix[2,])
specificity <- confusion_matrix[1,1]/sum(confusion_matrix[1,])

pr <- prediction(prediction, labels=test$TenYearCHD)
prf <- performance(prediction.obj = pr, measure="tpr",x.measure="fpr")
auc  <- performance(pr, "auc")
dd   <- data.frame(FP=prf@x.values[[1]],TP=prf@y.values[[1]])
#畫圖
plot(prf, col = rainbow(7), main = "ROC curve", xlab = "Specificity(FPR)", ylab = "Sensitivity(TPR)")
ggplot()+geom_line(data=dd, mapping = aes(x=FP, y=TP, color="Logistic Regression"))+geom_segment(mapping = aes(x=0,xend=1,y=0,yend=1))+
    ggtitle(label="ROC curve")+labs(x="FP Rate", y="TP Rate")
#AUC = 0.5
abline(0, 1)
#實際AUC值
text(0.5, 0.5, as.character(auc@y.values[[1]]))
as.numeric(auc@y.values[[1]])


