library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(rayshader)

#Import the "Mall_Customers.csv" data 
data = Mall_Customers
View(data)
names(data)
str(data)
dev.off()

### Exploratory Data Analysis ###
# Rename some column names
data <- rename(data, annual_income = Annual.Income..k..,
               spending_score = Spending.Score..1.100.)

head(data)
tail(data)
summary(data)

## barplot for the variable "Gender" using ggplot
ggplot(data,aes(x=Gender))+
  geom_bar(stat="count",width=0.5,fill="steelblue")+theme_minimal()+
  labs(title="Barplot to display Gender Comparison",xlab="Gender")
#We have more females in the data than males

## Histogram for the variable "Age"
ggplot(data, aes(x = Age)) +
  geom_vline(aes(xintercept = mean(Age)), color = "blue",
             linetype = "dashed", size = 1.5) +
  geom_histogram(binwidth = 3, aes(y = ..density..), 
                 color = "black", fill = "white") +
  geom_density(alpha = 0.4, fill = "blue") +
  labs(title = "Histogram to Show Density of Age Class")
#Most of the customers are in the age 25 to 45


## Density for the variable "annual_income"
ggplot(data,aes(x=annual_income))+
  geom_density(fill=rgb(0,1,0,0.4))+
  labs(title="Density Plot for the annual income variable")
#approximately normally distributed

## Boxplot for the variable "spending_score"
ggplot(data, aes(x = spending_score, y= Gender)) +
  geom_boxplot() +
  labs(title = "Boxplot for the Spending Score Variable")

### Clustering using k-means Algorithm: Hartigan-Wong K-Means ###
## Set seed 
set.seed(124)

#Get the optimal number of clusters using Elbow Method
##Elbow Method
fviz_nbclust(data[,4:5],kmeans,method="wss")
## gives 6 clusters


## Create the customer clusters with KMeans
k6=kmeans(data[,4:5],centers=6)
#Print the results
k6

segment = k6$cluster
data1=cbind(data,segment)
data1

##Plot the six KMeans clusters
clusplot(data, k6$cluster, color=TRUE, shade=TRUE, labels=0, lines=0)

set.seed(2)

# Create a plot of the customers segments
ggplot(data, aes(x = annual_income, y = spending_score)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name = " ", 
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", 
                                "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", 
          subtitle = "Using K-means Clustering")

## We can group the clusters as
#Cluster 1 : Low income earners with low spending score. I can assume that this is so because people with low income will tend to purchase less item at the store
#Cluster 2 and cluster 4 : This group of customers have a higher income but they do not spend more at the store. One of the assumption could be that they are not satisfied with the services rendered at the store. They are another ideal group to be targeted by the marketing team because they have the potential to bring in increased profit for the store.
#Cluster 3 : These are low income earning customers with high spending scores. I can assume that why this group of customers spend more at the retail store despite earning less is because they enjoy and are satisfied with the services rendered at the retail store.
#Cluster 5 : The customers in this group are high income earners and with high spending scores. They bring in profit. Discounts and other offers targeted at this group will increase their spending score and maximize profit.
#Cluster 6 : These are average income earners with average spending scores. They are cautious with their spending at the store.
