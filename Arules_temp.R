library(readxl)
library(arules)
library(dplyr)

setwd("C:/Users/563525/Desktop/Capstone")

orig_data <- read.csv("Loyal_Trans_ID.csv")
attach(orig_data)

View(orig_data)
names(orig_data)


transactiondata <- ddply(orig_data, c("New_Trans"), function(df1)paste(df1$Product, collapse = ","))

head(transactiondata)
View(transactiondata)
class(transactiondata)

str(orig_data)
orig_data$New_Trans <- as.factor(orig_data$New_Trans)

txn <- split(orig_data$Product,orig_data$New_Trans)
class(txn)
View(txn)

# agg.txn <- list()
# for (i  in 1:length(txn)) 
#   {
#   agg.txn[[i]] <- as.character(txn[[i]][!dulpicated(txn[[i]])])
#   }

trans <- as(txn,"transactions")
trans
View(trans)
summary(trans)


#  Frequency
freq <- itemFrequency(trans)
freq
freq <- freq[order(-freq)]

barplot(freq[1:20])
itemFrequencyPlot(trans,topN = 10)

model1 <- apriori(data = trans)
summary(model1)
