####Packages####
library(cluster)
library(factoextra)
library(stats)
library(ggplot2)
library(plotly)
####Preprocessing####
data_orig <- read.csv("D:/Desmond/Lesson/1072/Multivariate Analysis/final/data/OnlineNewsPopularity.csv")
popularity <- ifelse(data_orig$shares>1400,1,0)
data_orig <- cbind(data_orig,popularity)
dataset <- data_orig[,c(-1,-2,-61)]
id <- rownames(data_mds_df2)
data <- dataset[id,]

####mds####
distance <- daisy(training[-59],metric=c("gower"))
hope <- cmdscale(distance, k = 2, eig = TRUE)
data_mds <- hope$points
data_mds_df <- data.frame(data_mds)
data_mds_df <- cbind(data_mds_df,pop=training$popularity)

####kmeans####
data_mds_df2 <- read.csv("D:/Desmond/Lesson/1072/Multivariate Analysis/final/data/data_mds_df.csv")
rownames(data_mds_df2) <- data_mds_df2$X
data_mds_df2 <- data_mds_df2[,-1]
data_mds_df2 <- cbind(data_mds_df2,pop=data$popularity)
#plot_mds
cluster <- as.factor(data_mds_df2$pop)
ggplot(data_mds_df2,aes(x=X1,y=X2))+geom_point(aes(color=cluster))+scale_color_manual(values = c("red","blue"))

# 2 cluster
kmeans.cluster2 <- kmeans(data_mds_df2[-3], centers=2)
fviz_cluster(kmeans.cluster2,           # 分群結果
             data = data_mds_df2,              # 資料
             geom = c("point"), # 點和標籤(point & label)
             ellipse.type = "norm") 
kmeans.cluster2 <- kmeans(data_mds_df2[-3], centers=2)
cluster2 <- as.factor(kmeans.cluster2$cluster)
ggplot(data_mds_df2,aes(x=X1,y=X2))+geom_point(aes(color=cluster2))+scale_color_manual(values = c("red","blue"))

# 3 cluster
kmeans.cluster3 <- kmeans(data_mds_df2[-3], centers=3)
kmedoid.cluster3 <- pam(data_mds_df2[-3], k=3)

fviz_cluster(kmeans.cluster3,           # 分群結果
             data = data_mds_df2,              # 資料
             geom = c("point"), # 點和標籤(point & label)
             ellipse.type = "norm") 
fviz_cluster(kmedoid.cluster3,           # 分群結果
             data = data_mds_df2[-3],              # 資料
             geom = c("point"), # 點和標籤(point & label)
             ellipse.type = "norm") 

kmeans.cluster3 <- kmeans(data_mds_df2, centers =3)
cluster3 <- as.factor(kmeans.cluster3$cluster)
ggplot(data_mds_df2,aes(x=X1,y=X2))+geom_point(aes(color=cluster3))+scale_color_manual(values = c("red","blue","yellow"))

# 4 cluster
kmeans.cluster4 <- kmeans(data_mds_df2[-3], centers=4)
fviz_cluster(kmeans.cluster4,           # 分群結果
             data = data_mds_df2,              # 資料
             geom = c("point"), # 點和標籤(point & label)
             ellipse.type = "norm") 

# Elbow Method
cluster_var <- c()
for(i in 1:12){
  kmeans_cluster <- kmeans(data_mds_df2[-3], centers = i)
  temp <- kmeans_cluster$tot.withinss
  cluster_var <- c(cluster_var,temp)
}
plot(cluster_var,type = 'b',ylab='tot.within') #k=3

####Kmedoid####
data_orig
kmedoid.cluster <- pam(data_orig[-46], k=2) 
cf_kmedoid <- table(data_orig$popularity,kmedoid.cluster$cluster) 
acc_kmedoid <- sum(diag(cf_kmedoid))/sum(cf_kmedoid)
cf_kmedoid
####Hierarchical Clustering####
E.dist <- dist(data_mds_df2[,-3], method="euclidean") #歐式距離
h.E.cluster <- hclust(E.dist, method="ward.D2") 
plot(h.E.cluster)
abline(h=6.7, col='red')
cut.h.cluster <- cutree(h.E.cluster, k=2)

look <- data.frame(cut.h.cluster)
look <- cbind(look-1,data_mds_df2$pop)
cf_HierClus<- table(data_mds_df2$pop,cut.h.cluster)


####Explain Cluster####
data_c <- cbind(data,cluster3)

topic <- c()
for(i in 1:nrow(data_c)){
  n <- which.max(data_c[i,38:42])
  topic[i] <- n
}
data_c <- cbind(data_c,topic=topic)

channel <- c()
for(i in 1:nrow(data_c)){
  n <- which.max(data_c[i,12:17])
  channel[i] <- n
}
data_c <- cbind(data_c,channel=channel)

day <- c()
for(i in 1:nrow(data_c)){
  n <- which.max(data_c[i,30:36])
  day[i] <- n
}
data_c <- cbind(data_c,day=day)


clu_topic<- ggplot(data_c,aes(x=cluster3))+geom_bar(aes(fill=as.factor(topic)),position='fill')+ylab('proportion')
clu_topic2<- ggplot(data_c,aes(x=as.factor(topic)))+geom_bar(aes(fill=cluster3),position = 'fill')
ggplotly(clu_topic) 
ggplotly(clu_topic2)
#cluster1 -> topic1(40%)
#cluster2 -> topic4(42%)
#cluster3 -> topic5(83.5%)

clu_channel<- ggplot(data_c,aes(x=cluster3))+geom_bar(aes(fill=as.factor(channel)),position='fill')+ylab('proportion')
clu_channel2<- ggplot(data_c,aes(x=as.factor(channel)))+geom_bar(aes(fill=cluster3),position = 'fill')
ggplotly(clu_channel)
ggplotly(clu_channel2)
#cluster1 -> c6(40.5% world)
#cluster2 -> 
#cluster3 -> c5(76.6% tech)
                     

clu_day<- ggplot(data_c,aes(x=cluster3))+geom_bar(aes(fill=as.factor(day)),position='fill')
ggplot(data_c,aes(x=as.factor(day)))+geom_bar(aes(fill=cluster3),position = 'fill')
ggplotly(clu_day)

log_model <- glm(popularity~.,data=data_c,family = 'binomial')

