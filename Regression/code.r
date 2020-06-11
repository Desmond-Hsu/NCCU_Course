
# Introduction ------------------------------------------------------------
# Author : Desmond
# Date : 2019/1/8- 2019/1/19
# Topic : Heart Disease Study  

# Packages ----------------------------------------------------------------
library(ggplot2)
library(glmnet)
library(ROCR)
library(DMwR)
library(ggcorrplot)
library(ROSE)
library(plotly)
# Pre-preocessing ---------------------------------------------------------
dt_un <- read.csv("D:/Desmond/Lesson/Regression/HW/Heart Disease Study/data/framingham.csv")
dt_un$education <- factor(dt_un$education)
dt_un$male <- factor(dt_un$male)
dt_un$currentSmoker <- factor(dt_un$currentSmoker)
dt_un$BPMeds<- factor(dt_un$BPMeds)
dt_un$prevalentStroke <- factor(dt_un$prevalentStroke)
dt_un$prevalentHyp <- factor(dt_un$prevalentHyp)
dt_un$diabetes <- factor(dt_un$diabetes)

data <- knnImputation(dt_un)


# EDA ---------------------------------------------------------------------
f <- ggplot(data, aes(x=male, fill=TenYearCHD))+geom_bar()
ggplotly(f)
ggplot(data_p1, aes(x=sysBP, y=male,col=TenYearCHD))+geom_point(size=5,alpha=0.7,position="jitter")
ggplot(data, aes(x=TenYearCHD, y=totChol))+geom_boxplot()+scale_y_continuous(limits=c(100,400))
ggplot(data, aes(x=BMI, y=education,col=TenYearCHD))+geom_point(size=5,alpha=0.7,position="jitter")

## correlation matrix and plotting
data_plotcor <- data[,-1]
corr <- round(cor(data_plotcor), 3)
ggcorrplot(corr, hc.order = TRUE,lab = TRUE, colors = c("#6D9EC1", "white", "#E46726"))
ggcorrplot(corr, hc.order = TRUE, type = "upper",lab = TRUE)
col <- colorRampPalette(c("blue", "white", "red"))
corrplot(corr, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE
)


# Train/Test --------------------------------------------------------------
id <- sample(1:4240, 0.7*round(4240))
train <- data[id,]
test  <- data[-id,]

p <- sum(data$TenYearCHD==1)/4240
id_1 <- which(data$TenYearCHD==1)
id_1train <- sample(id_1, round(0.7*644))
id_0 <- which(data$TenYearCHD==0)  
id_0train <- sample(id_0, round(0.7*3596))
id_train <- c(id_0train,id_1train)
train_sp <- data[id_train,]
test_sp  <- data[-id_train,] 
  
train_smote <- ROSE(TenYearCHD ~ ., train, seed=1,N=4000)$data


# Variable selection -----------------------------------------------------
## full model
model_full <- glm(TenYearCHD~.,family="binomial",data = train)
## forward/backward selection
model_null <- glm(TenYearCHD ~ 1, family="binomial", data=train)

backward_selection <- step(object = model_full,scope = list(lower = model_null, upper = model_full),direction = "backward")
forward_selection <- step(object = model_null,scope = list(lower = model_null, upper = model_full),direction = "forward")

## ridge/lasso
ridge <-  glmnet(x = as.matrix(train[, -16]),y = train[, 16],alpha = 0,family = "binomial")
lasso <-  glmnet(x = as.matrix(train[, -16]),y = train[, 16],alpha = 1,family = "binomial")

par(mfcol = c(1, 2))
plot(lasso, xvar='lambda', main="Lasso")
plot(ridge, xvar='lambda', main="Ridge")

cv.lasso <-  cv.glmnet(x = data.matrix(train[, -16]),y = train[, 16],alpha = 1,family = "binomial")
best.lambda <-  cv.lasso$lambda.min
best.lambda
## plot(lasso, xvar='lambda', main="Lasso")
## abline(v=log(best.lambda), col="blue", lty=5.5 )
coef(cv.lasso, s = "lambda.min")
cv.ridge <-  cv.glmnet(x = data.matrix(train[, -16]),y = train[, 16],alpha = 0,family = "binomial")
best.lambda_r <-  cv.lasso$lambda.min
best.lambda_r
## plot(ridge, xvar='lambda', main="Ridge")
## abline(v=log(best.lambda_r), col="blue", lty=5.5 )
coef(cv.ridge, s = "lambda.min")


# Building model ----------------------------------------------------------
model_select <- glm(TenYearCHD~male+age+cigsPerDay+sysBP+glucose+prevalentStroke+BMI+totChol
                    ,family="binomial",data = train_smote)


# Evaluation --------------------------------------------------------------
pred <- predict(model_select, type="response",newdata = test)
prediction_TF <- ifelse(pred>0.5,1,0)

confusion_matrix <- table(test$TenYearCHD, prediction_TF)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
sensitivity <- confusion_matrix[2,2]/sum(confusion_matrix[2,])
specificity <- confusion_matrix[1,1]/sum(confusion_matrix[1,])

pr <- prediction(pred, labels=test$TenYearCHD)
prf <- performance(prediction.obj = pr, measure="tpr",x.measure="fpr")
auc  <- performance(pr, "auc")
dd   <- data.frame(FP=prf@x.values[[1]],TP=prf@y.values[[1]])
#畫圖
ggplot()+geom_line(data=dd, mapping = aes(x=FP, y=TP, color="Logistic Regression"))+geom_segment(mapping = aes(x=0,xend=1,y=0,yend=1))+
ggtitle(label="ROC curve")+labs(x="FP Rate", y="TP Rate")
#實際AUC值
as.numeric(auc@y.values[[1]])






