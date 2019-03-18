View(who4)
who4 %>% group_by(year) %>% summarise(totalCases = sum(count), avgCases = mean(count)) %>% View

library(readr)
library(dplyr)
library(tidyverse)

df_storeData <- read_csv("D:/Data Analytics/Course Material/rfmData.csv")

# if using read.csv(), it converts all character strings to factor
# to switch off this behaviour, pass an argument (see help)


View(df_storeData)

df_storeData %>% mutate(subTotal = Quantity*UnitPrice) %>% View

df_storeData %>% mutate(subTotal = Quantity*UnitPrice) -> df_storeData2
df_storeData2 %>% group_by(InvoiceNo) %>% summarise(total = sum(subTotal)) %>% View

View(df_storeData2)

class(df_storeData$InvoiceDate)

# complete.cases is !is.na()
df_storeData %>% 
  filter(complete.cases(CustomerID)) %>%
  group_by(CustomerID) %>%
  summarise(n=n_distinct(InvoiceNo)) %>% arrange(desc(n)) %>% View

# bias is there
# some customers having frequency =1 maybe acquired yesterday. it can have potential
# so u cant compare him with someone having 250 frequency over a period of 3 years for eg

# rfm analysis to the rescue
# recency, frequency, monetory

df_storeData %>% 
  separate(InvoiceDate, into = c("date", NA), sep="\\s") -> df_storeData1

class(df_storeData1$date)

#converting into DATE
as.Date(df_storeData1$date, format = "%m/%d/%Y")

#now finding recency with last purchase
df_storeData1 %>% 
  filter(complete.cases(CustomerID)) %>%
  group_by(CustomerID) %>%
  summarise(n=n_distinct(InvoiceNo), lastPurchaseDate = max(date)) %>% arrange(desc(n), desc(lastPurchaseDate)) ->df_rfMetrics

View(df_rfMetrics)
View(df_storeData2)

#finding monetory value

#assignment = how much money spent by each customer
df_storeData1 %>% 
  filter(complete.cases(CustomerID)) %>%
  group_by(CustomerID) %>%
  summarise(n=n_distinct(InvoiceNo), lastPurchaseDate = max(date), money = sum(Quantity*UnitPrice)) %>% 
  arrange(desc(n), desc(lastPurchaseDate)) ->df_rfmMetrics

View(df_rfmMetrics)


# binning of customers -> grouping in ranges of frequency
# here grouping will be in percentiles 20%, 40%, 60%, 80%, 100%

library(Hmisc)
cut2(df_rfMetrics$n, g=5)

# top 20% -> 8-248
# 80% are less than 8
# 60% are less than 5

# make histogram
# using cut2, make 5 bins each of recency, frequency and monetory
# store it in new dataframe

lastPurchaseDate <- df_rfmMetrics$lastPurchaseDate
class(lastPurchaseDate)

lastPurchaseDate <- as.Date(lastPurchaseDate, format = "%m/%d/%Y")

View(lastPurchaseDate)

bin_frequency <- cut2(df_rfmMetrics$n, g=5)
bin_date <- cut2(lastPurchaseDate, g=5)
bin_money <- cut2(df_rfmMetrics$money, g=5)

table(bin_frequency)
table(bin_money)
table(bin_date)

#------------------------------------------------------------------

load("D:/Data Analytics/Course Material/umair/df_gglMerchandise.RData")

df_gglMerchandise$daysofweek <- weekdays(df_gglMerchandise$date)
View(df_gglMerchandise)

fun_weekend <- function(weekday) {
  ifelse(weekday %in% c("Saturday", "Sunday"), "weekend", "weekday")
}

fun_weekend("Saturday")

# finding retention of customer
lst_customers <- lapply(unique(df_storeData1$CustomerID), function(customer){
  df_customer <- subset(df_storeData1, CustomerID == customer)
  minDate <- min(df_customer$date)
  df_customer$retention <- sapply(1:nrow(df_customer), function(i){
    days(df_customer$date[i] - minDate)$day %/% 30
  })
})

View(lst_customers[[1]])

length(unique(lst_customers[[1]]$InvoiceNo))
