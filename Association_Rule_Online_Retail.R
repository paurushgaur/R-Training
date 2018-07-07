#library(tidyverse)
library(readxl)
#library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)

# Read file

retail <- read_xlsx("D:/R Training/Training/Market Basket Analysis/Online Retail.xlsx",sheet = "Online Retail",col_names = T)

# Exploratory data analysis

retail <- retail[complete.cases(retail), ]

sum(complete.cases(retail))
str(retail)

retail <- retail %>% mutate(Description = as.factor(Description))
retail <- retail %>% mutate(Country = as.factor(Country))

retail$Date <- as.Date(retail$InvoiceDate)
retail$Time <- format(retail$InvoiceDate,"%H:%M:%S")
retail$InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))

str(retail)


"After preprocessing, the dataset includes 406,829 records and 10 fields:
InvoiceNo, StockCode, Description, Quantity, InvoiceDate, UnitPrice, CustomerID, Country, Date, Time."


retail$Time <- as.factor(retail$Time)
a <- hms(as.character(retail$Time))
retail$Time = hour(a)


# Visualizations for better understanding

hist<- plot_ly(x = ~retail$Time, type = "histogram")

layout(hist,title = "Hourly Purchase count",xaxis = list(title = " Hours "),
       yaxis = list(title = "Count"))


retail %>% 
  group_by(InvoiceNo) %>% 
  summarize(n_items = n()) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(fill="indianred", bins = 10000) + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))

tmp <- retail %>% 
  group_by(StockCode, Description) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
tmp <- head(tmp, n=10)


tmp %>% 
  ggplot(aes(x=reorder(Description,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()



# combining product descriptions

retail_sorted <- retail[order(retail$CustomerID),]
library(plyr)
itemList <- ddply(retail,c("CustomerID","Date"), 
                  function(df1)paste(df1$Description, 
                                     collapse = ","))

?ddply

itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")

write.csv(itemList,"D:/R Training/Training/Market Basket Analysis/market_basket.csv", quote = FALSE, row.names = TRUE)



(unique(retail$Description))

sum(table(unique(retail$Description)))

# convert data into transactions which can ve used to create rules

tr <- read.transactions('D:/R Training/Training/Market Basket Analysis/market_basket.csv', format = 'basket', sep=',')
tr
summary(tr)

#"trans = as((itemList), "transactions")

#itemList <- as.data.frame(itemList)"

#trans <- as(split(retail$Description, retail$CustomerID,retail$Date), "transactions")
#inspect(trans[1:2])


rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8, minlen=2, maxlen=8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
rules <- sort(rules, by=c('lift','confidence'), decreasing = TRUE)
rules <- sort(rules, by=c('confidence','lift'), decreasing = TRUE)
summary(rules)

inspect(rules[1:10])


# Redundant Rules

redundant= which(is.redundant(rules))

inspect(rules[head(redundant)])

# Remove Redundant Rules

rulesNR <- rules[-redundant]

sum(is.redundant(rulesNR))

summary(rulesNR)

inspect(rulesNR[1:10])


# Rules with LHS and RHS: Single or Combination

rules2 <- rulesNR

rules.lhs1 <- subset(rules2, lhs %in% c('WOBBLY CHICKEN'))

rules.lhs2 <- subset(rules2, rhs %in% c('COFFEE'))

inspect(rules.lhs1)
inspect(rules.lhs2)

# Another Method for setting LHS and RHS

rules_test <- apriori(tr, parameter = list(supp=0.005, conf=0.8, minlen=2, maxlen=8),
                 appearance = list(default= "lhs",rhs= "COFFEE"))

inspect(rules_test)

which(is.redundant(rules_test))

# Save the rules as csv

rulesNR12 <- as(rulesNR, "dataframe")

rulesNR1 <- DATAFRAME(rulesNR, separate = TRUE)

write.csv(rulesNR1, 'D:/R Training/Training/Market Basket Analysis/rules.csv')


# Visualization  on rules

topRules <- rules[1:10]
plot(topRules)



plot(topRules, method="graph", engine='interactive', shading = 'confidence')

?plotly()

plotly_arules(rules, method = "scatterplot", measure = c("support", "confidence"), 
             shading = "lift", max = 1000)
