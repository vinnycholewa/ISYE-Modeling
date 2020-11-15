

#QUESTION 4.2
#install & read in libraries
pkgs <- c("ClusterR", "factoextra",  "NbClust") #factoextra,nbclust for optimal cluster
install.packages(pkgs)
library(CluseterR)
library(factoextra)
library(NbClust)

#slice iris data to exclude species
df_iris = iris[1:4]
class(df_iris)
head(df_iris)
head(iris)
nrow(df_iris)
#review data set & run correlation report
summary(df_iris)
cor(df_iris) #strongest corr among petal length & petal width. sepal length & width are negatively corr

#visualize data set
#petal length and petal width
ggplot(iris,aes(x = Petal.Length, y = Petal.Width)) + geom_point()
ggplot(iris,aes(x = Petal.Length, y = Petal.Width, col= Species)) + geom_point() #actual clluster
#sepal length and sepal width
ggplot(iris,aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()
ggplot(iris,aes(x = Sepal.Length, y = Sepal.Width, col= Species)) + geom_point() #actual cluster


#determine optimal number of clusters
#elbow method
fviz_nbclust(df_iris, FUNcluster = kmeans, method = "wss")  #k = 3 or 4 # n = 57.2
#gap method
fviz_nbclust(iris[1:4], FUNcluster = kmeans, method = "gap_stat") #k = 4
#dilhouette method
fviz_nbclust(iris[1:4], FUNcluster = kmeans, method = "silhouette") #k = 2
#plot optimal method using elbow method
fviz_nbclust(df_iris, kmeans, method = "wss") +
        geom_vline(xintercept = 3, linetype = 4)+
        labs(subtitle = "Elbow method")

#kmeans model
set.seed(123)
#kmeans k=3, full data set
kmeans_result_3_full = kmeans(df_iris,3)
print(kmeans_result_3_full)
table(kmeans_result_3_full$cluster,iris$Species)
#kmeans k=3, considering only petal columns 
kmeans_result_3 = kmeans(df_iris[,3:4],3)
print(kmeans_result_3)
table(kmeans_result_3$cluster,iris$Species)
#k means k =4, full data set
kmeans_result_4 = kmeans(df_iris,4, nstart = 20)
print(kmeans_result_4)
table(kmeans_result_4$cluster,iris$Species)
#k means k =4, full data set
kmeans_result_4 = kmeans(df_iris[,3:4],4, nstart = 20)
print(kmeans_result_4)
table(kmeans_result_4$cluster,iris$Species)




plot(x = df_iris$Petal.Length, y = df_iris$Petal.Width, col=kmeans_result_3$cluster, 
     ylab = "Petal Width", xlab = "Petal Length") 
plot(x = df_iris$Petal.Length, y = df_iris$Petal.Width, col = iris$Species, 
     ylab = "Petal Width", xlab = "Petal Length")




