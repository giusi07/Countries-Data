#make decision to choose the countries that are in the direst need of aid. 
#categorise the countries using some socio-economic and health factors that determine 
#the overall development of the country. suggest the countries which the CEO 
#needs to focus on the most

####### IMPORT DATASET #######
country <- read.csv("C:/Users/giusy/Desktop/Countrydata.csv", header=TRUE, sep=",")
attach(country)

#setting country as row name
country2 <- country[,-1]
rownames(country2) <- country[,1]

#exploratory data analysis
sum(is.na(country2))

summary(country2)


#같같캰CA같같같같
library("ggplot2")
library("factoextra")
pca.country= prcomp(country2, scale=TRUE)
summary(pca.country)

fviz_eig(pca.country,addlabels = TRUE, ylim = c(0, 50)) #scree plot

#correlation circle
fviz_pca_var(pca.country, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE 
)

# Contributions of variables to PC1
fviz_contrib(pca.country, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(pca.country, choice = "var", axes = 2, top = 10)

#datapoints 
fviz_pca_ind(pca.country, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE 
)

#biplot
fviz_pca_biplot(pca.country, 
                col.var = "#E7B800", # Variables color
                col.ind = "#696969"  # datapoints color
)

#같같같같캩-means같같같같같
countryS= scale(country2)
set.seed(3)

fviz_nbclust(countryS, kmeans, method = "wss") #elbow rule 
fviz_nbclust(countryS, kmeans, method = "silhouette") #silhouette rule
kmeans.country=kmeans(countryS, 5, nstart=25)
kmeans.country
clusters= kmeans.country$cluster

fviz_cluster(kmeans.country, data = countryS) #clusters plot


#mean of each variables by clusters using the original data
aggregate(country2, by=list(cluster=kmeans.country$cluster), mean)

#같같같같H-Clustering같같같�
## Minkowski distances
# Euclidean
eucl.dist= dist(countryS, method="minkowski", p=2)
# City-block
cityblock.dist= dist(countryS, method="minkowski", p=1)



#H-clustering using Complete Linkage and Euclidean distance
hc1 <- hclust(eucl.dist, method = "complete" )
plot(hc1, cex = 0.6, hang = -1) #dendogram

#H-clustering using Complete Linkage and City-block distance
hc2 <- hclust(cityblock.dist, method = "complete" )
plot(hc2, cex = 0.6, hang = -1) #dendogram

#H-clustering using Average Linkage and Euclidean distance
hc3 <- hclust(eucl.dist, method = "average" )
plot(hc3, cex = 0.6, hang = -1) #dendogram

#H-clustering using Average Linkage and City-block distance
hc4 <- hclust(cityblock.dist, method = "average" )
plot(hc4, cex = 0.6, hang = -1) #dendogram

#H-clustering using Single Linkage and Euclidean distance
hc5 <- hclust(eucl.dist, method = "single" )
plot(hc5, cex = 0.6, hang = -1) #dendogram

#H-clustering using Single Linkage and City-block distance
hc6 <- hclust(cityblock.dist, method = "single" )
plot(hc6, cex = 0.6, hang = -1) #dendogram

#comparison of dendograms with euclidean distance
par ( mfrow =c(1 ,3) )
plot(hc1 ,main =" Complete Linkage ", xlab ="", sub ="",
       cex =.9)
plot(hc3 , main =" Average Linkage ", xlab ="", sub ="",
       cex =.9)
plot(hc5 , main =" Single Linkage ", xlab ="", sub ="",
       cex =.9)

#comparison of dendograms with city block distance
par ( mfrow =c(1 ,3) )
plot(hc2 ,main =" Complete Linkage ", xlab ="", sub ="",
     cex =.9)
plot(hc4 , main =" Average Linkage ", xlab ="", sub ="",
     cex =.9)
plot(hc6 , main =" Single Linkage ", xlab ="", sub ="",
     cex =.9)

#cutting each dendogram to obtain 5 groups
table(cutree (hc1 , 5))
aggregate(country2, by=list(cluster=cutree (hc1 , 5)), mean)
plot(hc1 ,main =" Complete Linkage - EU ", xlab ="", sub ="",
     cex =.9)
rect.hclust(hc1, k = 5, which = 1:5, border = 1:5, cluster = cutree (hc1 , 5))



table(cutree (hc2 , 5))
aggregate(country2, by=list(cluster=cutree (hc2 , 5)), mean)
plot(hc2 ,main =" Complete Linkage - CB ", xlab ="", sub ="",
     cex =.9)
rect.hclust(hc2, k = 5, which = 1:5, border = 1:5, cluster = cutree (hc2 , 5))


table(cutree (hc3 , 5))
aggregate(country2, by=list(cluster=cutree (hc3 , 5)), mean)
plot(hc3 ,main =" Average Linkage - EU ", xlab ="", sub ="",
     cex =.9)
rect.hclust(hc3, k = 5, which = 1:5, border = 1:5, cluster = cutree (hc3 , 5))

table(cutree (hc4 , 5))
aggregate(country2, by=list(cluster=cutree (hc4 , 5)), mean)
plot(hc4 ,main =" Average Linkage - CB ", xlab ="", sub ="",
     cex =.9)
rect.hclust(hc4, k = 5, which = 1:5, border = 1:5, cluster = cutree (hc4 , 5))

table(cutree (hc5 , 5))
aggregate(country2, by=list(cluster=cutree (hc5 , 5)), mean)
plot(hc5 ,main =" Single Linkage - EU ", xlab ="", sub ="",
     cex =.9)
rect.hclust(hc5, k = 5, which = 1:5, border = 1:5, cluster = cutree (hc5 , 5))

table(cutree (hc6 , 5))
aggregate(country2, by=list(cluster=cutree (hc6 , 5)), mean)
plot(hc6 ,main =" Single Linkage - CB ", xlab ="", sub ="",
     cex =.9)
rect.hclust(hc6, k = 5, which = 1:5, border = 1:5, cluster = cutree (hc6 , 5))


