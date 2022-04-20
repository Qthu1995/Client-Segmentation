library(ggplot2)
library(corrplot) ## corrplot
library(cluster) ## clustering algorithms
library(factoextra) ## clustering algorithms &visualization
library(mclust)

###Wholesale customers data
setwd("C:\\Users\\Thu Beo\\Desktop\\4leto\\VSas2\\Thu_Project_VSAS2")
wholesale  <- read.table("data.txt", header=TRUE)
head(wholesale)
attach(wholesale)

wholesale$Channel = as.factor(wholesale$Channel)
wholesale$Region = as.factor(wholesale$Region)
summary(wholesale)


# Test normality:

qqnorm(Fresh)  # QQ-plot
qqline(Fresh) 
ks.test(Fresh,"pnorm", mean=0,sd=sqrt(var(Fresh))) # Kolmogorov-Smirnovov test
shapiro.test(Fresh) #shapiro test
shapiro.test(Fresh) #shapiro test

qqnorm(Milk) # QQ-plot
qqline(Milk) 
ks.test(Milk,"pnorm", mean=0,sd=sqrt(var(Milk))) # Kolmogorov-Smirnovov test
shapiro.test(Milk)

qqnorm(Grocery) # QQ-plot
qqline(Grocery) 
ks.test(Grocery,"pnorm", mean=0,sd=sqrt(var(Milk))) # Kolmogorov-Smirnovov test
shapiro.test(Grocery)

#Visualizing data
round(cor(wholesale, 4))
plot(wholesale)
plot(wholesale[, 3:8], cex=0.7, pch=19)


#Data Preprocessing###
print(apply(wholesale, 2, function (x) sum(is.na(x)))) #Check missing values
wholesale1  <- wholesale
wholesale1 <- na.omit(wholesale1) #elimate missing values


wholesale1$Channel <- NULL #Delete feature Channel
wholesale1$Region <- NULL  #Delete feature Region

#applying log for each feature
wholesale1.log <- wholesale1
wholesale1.log$Fresh <- log(wholesale1.log$Fresh)
wholesale1.log$Milk <- log(wholesale1.log$Milk)
wholesale1.log$Grocery <- log(wholesale1.log$Grocery)
wholesale1.log$Frozen <- log(wholesale1.log$Frozen)
wholesale1.log$Det_Pap <- log(wholesale1.log$Det_Pap)
wholesale1.log$Delicassen <- log(wholesale1.log$Delicassen)
plot(wholesale1.log)

#Standardize the variables
good_data <- scale(wholesale1.log) 


####PCA###########

PCA <- prcomp(good_data, scale = TRUE)
print(PCA)
summary(PCA)
print(summary(PCA)$importance)
atribute <- dim(good_data)
plot(0:atribute[2], c(0, summary(PCA)$importance[3,])*100, type="b", ylim=c(0, 100), xlab = "Dimensions", ylab = "Percentage of explained variances", main = "Scree plot")
# Graf do 2D:
par(mfrow=c(1, 2))
x <- t(PCA$x[, 1])
y <- t(PCA$x[, 2])
plot(x, y, pch = 19, xlab = "1. component", ylab = "2. component", xlim=c(-4,6), ylim=c(-6,4))
# Graf do 3D:
library(plot3D)
z <- t(PCA$x[, 3])
scatter3D(x, y, z, pch = 19, xlab = "1. component ", ylab = "2. component", zlab = "3. component")

######k-means####
library(NbClust)
# determine k-value 
NbClust(good_data, distance="euclidean", method = "kmeans") #k = 2
NbClust(good_data, distance="maximum", method = "kmeans")$Conclusion #k = 2
NbClust(good_data, distance="manhattan", method = "kmeans")$Conclusion #k=2

Channel

library(cluster)
# Compute kmeans()
wholesale.kmeans <- kmeans(good_data, centers = 2, nstart = 50)
wholesale.kmeans$tot.withinss
wholesale.kmeans <- kmeans(good_data, centers = 2, nstart = 2002)
wholesale.kmeans$tot.withinss
wholesale.kmeans$cluster

kmeans_plot <- data.frame(good_data,  wholesale.kmeans$cluster)
clusplot(kmeans_plot, wholesale.kmeans$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


#find k-value if data is not preprececessed
NbClust(wholesale1, distance="euclidean", method = "kmeans")


######Divisive clusterinig-DIANA#####

# Compute diana()
wholesale.diana <- diana(good_data, metric = "euclidean", stand = FALSE)
summary(wholesale.diana)
wholesale.diana$merge


# plotting the dendrogram from diana output
plot(wholesale.diana, cez = 0.6, hang = -1, main = "Dendrogram of diana")

# Cut tree into 2 groups
grp <- cutree(wholesale.diana, k = 2)

# Visualization of clusters
diana_plot <- data.frame(good_data, grp)
clusplot(diana_plot, grp, color = TRUE, shade=TRUE, labels=2, line=0)


####### Classification trees###########
#The wholesale dataset is then split for training and testing.
wholesale2 <- wholesale
wholesale2$Region <- NULL  #Delete feature Region
n <- dim(wholesale2)[1]
n.train <- n/2
n.test <- n - n.train
wholesale2$Channel <- as.factor(wholesale2$Channel)

set.seed(1)
indexy <- sample(1:n)
wholesale.train <- wholesale2[indexy[1:n.train], ]
wholesale.test <- wholesale2[indexy[(n.train+1):n], ]

#Train the classifier and plot the results
library(rpart)
library(rpart.plot)
fit <- rpart(Channel~., data = wholesale.train, method = "class")
par(mfrow=c(1, 1))
rpart.plot(fit)

pred <- round(predict(fit, wholesale.test), 0)   
test <- rep(0, n.test)
for(i in 1:n.test)
{
  if(pred[i, 1] == 1) test[i] <- 1
  if(pred[i, 2] == 1) test[i] <- 2
}
table(test, wholesale.test[, 1])
1 - mean(test == wholesale.test[, 1])
