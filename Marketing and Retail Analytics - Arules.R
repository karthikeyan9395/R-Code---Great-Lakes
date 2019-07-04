### Marketing and Retail Analytics ###

## Load Packages
install.packages("tidyverse")
install.packages("arules")
install.packages("arulesViz")
install.packages("knitr")
install.packages("gridExtra")
install.packages("lubridate")

library(tidyverse)
library(arules)
library(arulesViz)
library(knitr)
library(gridExtra)
library(lubridate)

## Read the data
data <- read.csv(file.choose())
View(data)
data <- data[ , -11]

## Converting dataframe into transaction data

transactiondata <- ddply(data, c("Bill.Number"), function(df1)paste(df1$Item.Desc, collapse = ","))

head(transactiondata)

## Since the column Bill.Number is nor of any use we remove it
transactiondata$Bill.Number <- NULL

## Provide a Column name to V1
colnames(transactiondata) <- c("Items")

head(transactiondata)

## Store the above transaction data into a CSV
write.csv(transactiondata,"C:/Users/rkart/OneDrive/Documents/Great Lakes BABI.csv", quote = FALSE, row.names = FALSE)

## Convert transaction data file in basket format and convert it into an object of the transaction class
trans <- read.transactions("C:/Users/rkart/OneDrive/Documents/Great Lakes BABI.csv", format = 'basket', sep = ',')

## Transaction Object
trans

## Summary
summary(trans)

## Structure
glimpse(trans)

## Data Analysis ###

## Absolute Item Frequency Plot
itemFrequencyPlot(trans, topN= 15, type = "absolute", col = "wheat2", xlab = "Item name", ylab = "Frequency(absolute)", main = "Absolute Item Frequency Plot - Top 15")


## The top 5 best selling Items are Nirvana Hookah Single, Mint Flavour Single, Cappuccino, Great Lakes Shake and Sambuca

## Relative Item Frequency Plot
itemFrequencyPlot(trans, topN=15, type = "relative", col = "lightcyan2", xlab = "Item name", ylab = "Frequency(relative)", main ="Relative Item Frequency Plot - Top 15")

## Apriori Algorithm

# Support and Confidence values
supportlevels <- c(0.1, 0.05, .01, .005, .0025, .0010)

confidencelevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)

# Empty Integers
rules_sup10 <- integer(length = 9)
rules_sup5 <- integer(length = 9)
rules_sup1 <- integer(length = 9)
rules_sup0.5 <- integer(length = 9)
rules_sup0.25 <- integer(length = 9)
rules_sup0.10 <- integer(length = 9)

# Apriori algorithm with a support level of 10%
for(i in 1:length(confidencelevels)){
  rules_sup10[i] <- length(apriori(trans, parameter = list(sup=supportlevels[1],
                                                           conf = confidencelevels[i], target = "rules")))
}

# Apriori algorithm with a support level of 5%
for(i in 1:length(confidencelevels)){
  rules_sup5[i] <- length(apriori(trans, parameter = list(sup=supportlevels[2],
                                                           conf = confidencelevels[i], target = "rules")))
}

# Apriori algorithm with a support level of 1%
for(i in 1:length(confidencelevels)){
  rules_sup1[i] <- length(apriori(trans, parameter = list(sup=supportlevels[3],
                                                          conf = confidencelevels[i], target = "rules")))
}

# Apriori algorithm with a support level of 0.5%
for(i in 1:length(confidencelevels)){
  rules_sup0.5[i] <- length(apriori(trans, parameter = list(sup=supportlevels[4],
                                                          conf = confidencelevels[i], target = "rules")))
}

# Apriori algorithm with a support level of 0.25%
for(i in 1:length(confidencelevels)){
  rules_sup0.25[i] <- length(apriori(trans, parameter = list(sup=supportlevels[5],
                                                            conf = confidencelevels[i], target = "rules")))
}

# Apriori algorithm with a support level of 0.10%
for(i in 1:length(confidencelevels)){
  rules_sup0.10[i] <- length(apriori(trans, parameter = list(sup=supportlevels[6],
                                                             conf = confidencelevels[i], target = "rules")))
}

## Graphical Representation of rules generated with a support level of 10%, 5%, 1%, 0.5% and 0.25%

## Number of rules found with a support level of 10%
plot1 <- qplot(confidencelevels, rules_sup10, geom = c("point", "line"), xlab = "Confidence Level", ylab = "Number of rules found", main = "Apriori with a support level of 10%") + theme_bw()

## Number of rules found with a support level of 5%
plot2 <- qplot(confidencelevels, rules_sup5, geom = c("point", "line"), xlab = "Confidence Level", ylab = "Number of rules found", main = "Apriori with a support level of 5%") + theme_bw()

## Number of rules found with a support level of 1%
plot3 <- qplot(confidencelevels, rules_sup1, geom = c("point", "line"), xlab = "Confidence Level", ylab = "Number of rules found", main = "Apriori with a support level of 1%") + theme_bw()

## Number of rules found with a support level of 0.5%
plot4 <- qplot(confidencelevels, rules_sup0.5, geom = c("point", "line"), xlab = "Confidence Level", ylab = "Number of rules found", main = "Apriori with a support level of 0.5%") + theme_bw()

## Number of rules found with a support level of 0.25%
plot5 <- qplot(confidencelevels, rules_sup0.25, geom = c("point", "line"), xlab = "Confidence Level", ylab = "Number of rules found", main = "Apriori with a support level of 0.25%") + theme_bw()


## Number of rules found with a support level of 0.10%
plot6 <- qplot(confidencelevels, rules_sup0.10, geom = c("point", "line"), xlab = "Confidence Level", ylab = "Number of rules found", main = "Apriori with a support level of 0.10%") + theme_bw()

## Subplot
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 3)

## Based on the plot we choose a support level of 0.10% and confidence level of 40%

### Execution
rules_final <- apriori(trans, parameter = list(sup = .001,
                                               conf = 0.4, target = "rules"))

## Association rules
inspect(rules_final)

## Rule Interpretation
## 46% of the customer who bought Herb Roast Chicken also bought Lemon Infused Char Grilled Veg - It had the higest Lift of 79.3

## Combo Meal Suggestion for the Restaurant
## Based on the association the best combo that the restaurant could offer
## is Herb Roast Chicken and Lemon Infused Char Grilled Veg

### Visualization of the Association Rules

## Scatter Plot
plot(rules_final, measure=c("support", "lift"), shading = "confidence")

## Graph V1
plot(rules_final, method = "graph")

## Graph V2
plot(rules_final, method = "graph", control = list(layout = igraph::in_circle()))

## Grouped Matrix Plot
plot(rules_final, method = "grouped")

### Exploratory Data Analysis

coffee <- read.csv(file.choose())

coffee <- coffee[ , -11]

str(coffee)

View(coffee)



## Transactions per Month

coffee %>%
  mutate(Month=as.factor(month(Date))) %>%
  group_by(Month) %>%
  summarise(Transactions=n_distinct(Bill.Number)) %>%
  ggplot(aes(x=Month, y=Transactions)) +
  geom_bar(stat="identity", fill="mistyrose2", show.legend=FALSE) +
  geom_label(aes(label=Transactions)) +
  labs(title="Transactions per month") +
  theme_bw() 

## Transactions per day

coffee %>%
  mutate(Day=as.factor(day(Date))) %>%
  group_by(Day) %>%
  summarise(Transactions=n_distinct(Bill.Number)) %>%
  ggplot(aes(x=Day, y=Transactions)) +
  geom_bar(stat="identity", fill="mistyrose2", show.legend=FALSE) +
  geom_label(aes(label=Transactions)) +
  labs(title="Transactions per day") +
  theme_bw() 

## Transactions per Week day

  coffee %>%
    mutate(WeekDay=as.factor(weekdays(as.Date(Date)))) %>%
    group_by(WeekDay) %>%
    summarise(Transactions=n_distinct(Bill.Number)) %>%
    ggplot(aes(x=WeekDay, y=Transactions)) +
    geom_bar(stat="identity", fill="peachpuff2", show.legend=FALSE) +
    geom_label(aes(label=Transactions)) +
    labs(title="Transactions per weekday") +
    scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
    theme_bw() 
  
  # Transactions per hour
  coffee %>%
    mutate(Hour=as.factor(hour(hms(Time)))) %>%
    group_by(Hour) %>%
    summarise(Transactions=n_distinct(Bill.Number)) %>%
    ggplot(aes(x=Hour, y=Transactions)) +
    geom_bar(stat="identity", fill="steelblue1", show.legend=FALSE) +
    geom_label(aes(label=Transactions)) +
    labs(title="Transactions per hour") +
    theme_bw()
  
  ## Transactions by Category
  
  coffee %>%
    mutate(Category=as.factor(weekdays(as.Date(Date)))) %>%
    group_by(WeekDay) %>%
    summarise(Transactions=n_distinct(Bill.Number)) %>%
    ggplot(aes(x=WeekDay, y=Transactions)) +
    geom_bar(stat="identity", fill="peachpuff2", show.legend=FALSE) +
    geom_label(aes(label=Transactions)) +
    labs(title="Transactions per weekday") +
    scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
    theme_bw() 
