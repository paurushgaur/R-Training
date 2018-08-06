setwd("C:/Users/ab97627/Desktop/Clusters")
mydata=read.csv("C:/Users/ekratsh/Desktop/Training/Day 3/Clustering/Cars_data.csv")
str(mydata)
names(mydata)
head(mydata)

  
  # # To standarize the variables 
colSums(is.na(mydata))
nrow(mydata)
newdatafinal = scale(mydata[,2:ncol(mydata)])
newdatafinal1 = (mydata[,2:ncol(mydata)])


a <- c(1,2,3,4)
mean(a)
sd(a)
(1-mean(a))/sd(a)
b <- c(100,200,250,300)
scale(a)
scale(b)
head(newdatafinal)
summary(newdatafinal)

# Assessing cluster tendency
install.packages("clustertend")
library(clustertend)
# Compute Hopkins statistic for the dataset
set.seed(123)
hopkins(newdatafinal, n = nrow(newdatafinal)-1)
#Since the H value = 0.2411563 which is far below the threshold 0.5, it is highly clusterable

# Determining Optimal Number of Clusters-  
#http://www.sthda.com/english/articles/29-cluster-validation-essentials/96-determining-the-optimal-number-of-clusters-3-must-know-methods/


# K-mean - Determining optimal number of clusters
install.packages("NbClust")
library(NbClust)

# A higher silhouette width is preferred to determine the optimal number of clusters
optimalcluster = NbClust(newdatafinal,distance = "euclidean", min.nc=2, 
                         max.nc=15, method = "kmeans",index = "silhouette")

optimalcluster$All.index
optimalcluster$Best.nc
# All index
optimalcluster1 = NbClust(newdatafinal,distance = "euclidean", min.nc=2,
                          max.nc=15, method = "kmeans",index = "all")
optimalcluster1$Best.nc

#Method II : Same Silhouette Width analysis with fpc package
install.packages("fpc")
library(fpc)
pamkClus <- pamk(newdatafinal, krange = 2:15, criterion="multiasw", ns=2, critout=TRUE)
pamkClus$nc
# plot


# K-Means Cluster Analysis

fit <- kmeans(newdatafinal,pamkClus$nc)
fit
summary(fit)
clustmem <- fit$cluster
clustmem

fit$totss
fit$withinss
fit$tot.withinss
fit$betweenss

fit$size

# get cluster means
aggregate(newdatafinal,by=list(fit$cluster),FUN=mean)

# Plot  representation 
library(cluster)
clusplot(newdatafinal,fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2)

# append cluster assignment
data <- data.frame(mydata, clusterid=fit$cluster)
write.csv(data,"C:/Users/ekratsh/Desktop/Training/Day 3/Clustering/cluster.csv")

str(data)

data$clusterid <- as.factor(data$clusterid)

data1 <- data[-1]


car_tree <- rpart(clusterid~., data= data1)
rpart.plot(car_tree,type=4,extra=104,
            nn=T,cex=.6)


