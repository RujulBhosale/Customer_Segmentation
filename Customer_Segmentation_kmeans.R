library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)

#Import the "Mall_Customers.csv" data 
data = Mall_Customers
View(data)
names(data)
str(data)


### Exploratory Data Analysis ###
#Rename some column names
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

### Clustering using k-means Algorithm ###
## Set seed 
set.seed(125)

#Get the optimal number of clusters using Elbow Method
##Elbow Method
fviz_nbclust(data[,4:5],kmeans,method="wss")

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

#Create a plot of the customers segments
ggplot(data, aes(x = annual_income, y = spending_score)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name = " ", 
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", 
                                "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", 
          subtitle = "Using K-means Clustering")
