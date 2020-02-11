#Object
a <- 20
a

b <- 'meja'
b

#Vector
v.data <-c(1,2,3,4)
v.data

v.data2 <-c(1:9)
v.data2

#List
mylist <- list(buku=3, pensil=4)
mylist

mylist$baru <- c(1,2,3,4)
mylist

#matrix 
mymatrix<- matrix(c(1:12),3,4)
mymatrix

#Dataframe
years <- c(1980,1985,1990)
scores <- c(34,44,83)
football <- data.frame(years,scores)
football
football$year

#factor
diterima <- factor(c('ya','tidak','ya','ya','tidak'))
diterima

# load data dari csv files
dataset <- read.csv("iris.csv", header=FALSE)
View(dataset)

#menggunakan library dataset
library(datasets)
iris <- iris
View(iris)

#Eksploarsi Data
#Summarize Data
#structure data
str(iris)

#data dim
dim(iris)

#had & tail
head(iris)
tail(iris)

#summary
summary(iris)

#class distribution
# distribution of class variable
y <- iris$Species
cbind(freq=table(y), percentage=prop.table(table(y))*100)

# korelasi
# calculate a correlation matrix for numeric variables
correlations <- cor(iris[,1:4])
# display the correlation matrix
print(correlations)

#standart deviasi
# calculate standard deviation for all attributes
sapply(iris[,1:4], sd)

#Visualize Data
#Histogram
hist(iris$Sepal.Length)
hist(iris$Sepal.Length, main="Histogram of Sepal Length", 
     xlab="Sepal Length",
     xlim=c(4,8), col="blue", freq=FALSE)
install.packages("ggplot2")

library(ggplot2)
histogram <- ggplot(data=iris, aes(x=Petal.Width))
histogram +
  geom_histogram(binwidth=0.2, 
                 color="black",
                 aes(fill=Species)) + 
  xlab("Petal Width") +  ylab("Frequency") + 
  ggtitle("Histogram of Sepal Width")

#Pie Chart
pie(table(iris$Species))

#Density
plot(density(iris$Sepal.Width))
density <- ggplot(data=iris, aes(x=Sepal.Width))
density +
  geom_density(stat="density", alpha=I(0.2), fill="blue") +
  xlab("Sepal Width") +  ylab("Density") +
  ggtitle("Histogram & Density Curve")

#Density ++
density <- ggplot(data=iris, aes(x=Petal.Width))
density + geom_histogram(binwidth=0.2, 
                         color="black", 
                         fill="steelblue", aes(y=..density..)) +
  geom_density(stat="density", alpha=I(0.2), fill="blue") +
  xlab("Petal Width") +  ylab("Density") + 
  ggtitle("Histogram & Density Curve")

density2 <- ggplot(data=iris, aes(x=Petal.Width, fill=Species))
density2 + geom_density(stat="density", alpha=I(0.2)) +
  xlab("Petal Width") +  ylab("Density") +
  ggtitle("Histogram & Density Curve of Sepal Width")

#boxplot
boxplot(iris$Petal.Length)
boxplot(Petal.Length~Species,
        data=iris, 
        xlab="Species", 
        ylab="Petal Length", 
        main="Iris Boxplot")

box <- ggplot(data=iris, 
              aes(x=Species, y=Petal.Length))
box + geom_boxplot(aes(fill=Species)) + 
  ylab("Petal Length") + 
  ggtitle("Iris Boxplot") +
  stat_summary(fun.y=mean, 
               geom="point", shape=5, size=4) 

#Barplot
barplot(table(iris$Species))
bar <- ggplot(data=iris, aes(x=Species))
bar + geom_bar() + 
  xlab("Species") +  ylab("Count") + 
  ggtitle("Bar plot of Sepal Length") 

#Scatter Plot
plot(x=iris$Sepal.Length, y=iris$Sepal.Width, 
     xlab="Sepal Length", ylab="Sepal Width",
     main="Sepal Length-Width")

scatter <- ggplot(data=iris, 
                  aes(x = Sepal.Length, y = Sepal.Width)) 
scatter + geom_point(aes(color=Species, shape=Species)) +
  xlab("Sepal Length") +  ylab("Sepal Width") +
  ggtitle("Sepal Length-Width")

#Scatter Plot Multi
pairs(iris[,1:4],  
      pch = 23, 
      bg = c("red", "blue", "green")
      [unclass(iris$Species)])

#Scatter Plot 3d
library(scatterplot3d)
scatterplot3d(iris$Sepal.Length, 
              iris$Sepal.Width, 
              iris$Petal.Length, 
              pch = 21, 
              bg = c("red", "blue", 
                     "green")
              [unclass(iris$Species)])

#Cor plot
library(corrplot)
i <- cor(iris[,1:4])
corrplot(i, method = "circle")

#Heat Map
library(ggplot2)
library(reshape2)
dat <- iris[,1:4]
cor <- melt(cor(dat, use="p"))
heat <- ggplot(data=cor, aes(x=Var1, y=Var2, fill=value)) 
heat + geom_tile() + labs(x = "", y = "") + 
  scale_fill_gradient2(limits=c(-1, 1))