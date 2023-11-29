library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(plyr)

df <- read.csv("online_retail_1.csv")

# preview the missing values
mean(complete.cases(df))

df %>%
  filter(rowSums(is.na(.)) > 0)

# remove a missing values 
df <- df[complete.cases(df), ]

# convert to factor
df %>%
  mutate(Decription = as.factor(Description))

df %>%
  mutate(Country = as.factor(Country))

df$Date <- as.Date(df$InvoiceDate)

df$InvoiceDate <- as.POSIXct(df$InvoiceDate, format = "%Y-%m-%d %H:%M:%S")

# convert to vector
TransTime <- format(df$InvoiceDate,"%H:%M:%S")
InvoiceNo <- as.numeric(df$InvoiceNo) 

cbind(df,TransTime)
cbind(df,InvoiceNo)

transaction <- ddply(df,c("Invoice","Date"),
                function(df1)paste(df1$Description,
                                   collapse = ","))
transaction$Invoice <- NULL
transaction$Date <- NULL
colnames(transaction) <- c("items")

write.csv(transaction, "project_market.csv", quote = FALSE, row.names = FALSE)

tran <- read.transactions("project_market.csv", format = 'basket', sep = ',')
summary(tran)

itemFrequencyPlot(tran, topN=20, type='absolute') 

## Start to Generating Rules
# Min Support as 0.004, confidence as 0.6
rules <- apriori(tran, parameter = list(supp=0.004, conf=0.6, target="rules"))
inspect(rules[1:10]) 

## Visualization
# Scatter
plot(rules, measure=c("support", "lift"), shading="confidence")

# Group
top10_rules_c <- head(rules, n = 10, by = "confidence")
plot(top10_rules , method="grouped")

# Parallel
plot(rules, method="paracoord", control=list(reorder=TRUE))

## Apply 
regency_rules <- apriori(tran, parameter = list(supp=0.004, conf=0.6),appearance = list(default="lhs",rhs="GREEN REGENCY TEACUP AND SAUCER"))
inspect(head(regency_rules))
plot(regency_rules, method="graph", engine = "htmlwidget")
