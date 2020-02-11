#Read Data
titanic_r <- read.csv(file="titanic_r.csv",
                      header=TRUE, sep=",")
titanic_r

Groceries<-read.transactions(file = "Groceries.csv", 
                             sep = ",")
Groceries

#Apriori
library(arules)
#find association rules with default setting
rules_titanic_all <- apriori(titanic_r)
rules_titanic_all
inspect(rules_titanic_all)

rules_groceries_all <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.9))
rules_groceries_all

rules_titanic <- apriori(titanic_r, control = list(verbose=F), 
                 parameter = list(minlen =2, supp=0.005, conf=0.9),
                 appearance = list(rhs=c("Survived=No","Survived=Yes"),default="lhs"))
rules_titanic
inspect(rules_titanic)

rules_groc <- apriori (Groceries,
                       parameter = list(supp = 0.001, conf = 0.9))
inspect(rules_groc)

#Sort Rules
sort.rule <- sort(rules_titanic, by="lift")
inspect(sort.rule) 

#Visualization
library(arulesViz)
plot(sort.rule, method="graph", control=list(nodeCol="red", edgeCol="blue"))
plot(sort.rule)
plot(sort.rule, method="grouped", control=list(col=2))

#FPGrowth
library(rJava)
library(rCBA)
rules_fp <- rCBA::fpgrowth(titanic_r, support = 0.03, 
                           confidence = 0.8, maxLength = 4, 
                           consequent = "Survived", parallel=FALSE)
rules_fp
inspect(rules_fp)


#Sequential Generalized Pattern
library(arulesSequences)
data(zaki)
zaki_df<- as(zaki, "data.frame")

#read from csv
zaki_mtx <- read_baskets("zaki3.csv", sep=",", 
                         info = c("sequenceID","eventID" , "SIZE"))
zaki_mtx_df<- as(zaki_mtx, "data.frame")

#Run Spade
s1 <- cspade(zaki, parameter = list(support = 0.4), control = list(verbose = TRUE))
summary(s1)
as(s1, "data.frame")
s1.df <- as(s1, "data.frame")

s12 <- cspade(zaki_mtx, parameter = list(support = 0.4), control = list(verbose = TRUE))
summary(s12)
as(s12, "data.frame")
s12.df <- as(s12, "data.frame")

#Sequence to Rule
# Get induced temporal rules from frequent itemsets
r1 <- as(ruleInduction(s1, confidence = 0.5, control = list(verbose = TRUE)), 
         "data.frame")






#make dataframe on sgp data
df_zaki <- data.frame(sequenceID = c(1,1,1,1,2,2,3,4,4,4),
                 eventID = c(10,15,20,25,15,20,10,10,20,25),
                 Size = c(2,3,3,4,3,1,3,3,2,3),
                 items = c("C,D","A,B,C","A,B,F","A,C,D,F","A,B,F", "E", "A,B,F", "D,G,H","B,F","A,G,H")
)

df_zaki <- data.frame(lapply(df_zaki, as.factor))

write.table(df_zaki, file="zaki3.csv", sep = ",", row.names = F, col.names = F, quote=F)

# "sequenceID" (sequence or customer identifier) and "eventID" (time or event identifier)
zaki_mtx <- read_baskets("zaki3.csv", sep=",", info = c("sequenceID","eventID" , "SIZE"))

# convert transaction as data frame
as(zaki_mtx, "data.frame")

