library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(data.table)
library("xlsx")
library(readxl)
library(forecast)


### 1 Data Pre-processing:


#Setting the directory
getwd()
setwd("E:/Docs/Fall 2019/Data Mining/Assignments/Project")

# Loading the given data
mydata1 = read.csv("sales_train_v2.csv", header=T, sep=",") 
mydata2 = read.csv("items.csv", header=T, sep=",")
mydata3 = read.csv("item_categories.csv", header=T, sep=",")
mydata4 = read.csv("shops.csv", header=T, sep=",")
mydata5 = read.csv("sales.csv", header=T, sep=",")

# Merging the data
mytempdata = merge(mydata1, mydata2) 
mytempdata = merge(mytempdata, mydata3)
mytempdata = merge(mytempdata, mydata4)

# Arranging the merged data
colnames(mytempdata)
mytempdata1 <- mytempdata[, c(4, 5, 3, 2, 7, 6, 1)]
#view(mytempdata1)

# Checking for the missing data 
any(is.na(mytempdata1))
sum(is.na(mytempdata1))
#There is no missing value in the dataset


### 2. Outlier detection


sale_by_month <- aggregate(mytempdata1$item_cnt_day, by = list(date_block_num = mytempdata1$date_block_num), FUN = sum)
plot(sale_by_month, ylab="item_cnt_day", main = "Sale by month",type = "l", lwd = 2, col = "blue")
summary(sale_by_month)
# Overall sales decreases with time but there are peaks in November, thereby outlier lies in November 2013


### 3. Feature Engineering


mytempdata1$revenue <- mytempdata1$item_price * mytempdata1$item_cnt_day
head(mytempdata1)

# Replacing those values with NA
#mytempdata1$item_cnt_day[mytempdata1$item_cnt_day < 0]  <- NA

mytempdata2 <- mytempdata1[!(mytempdata1$item_cnt_day <0 || mytempdata1$item_price < 0),]
sale_by_month <- aggregate(mytempdata2$item_cnt_day, by = list(date_block_num = mytempdata2$date_block_num), FUN = sum)
plot(sale_by_month, ylab="item_cnt_day", main = "Sale by month",type = "l", lwd = 2, col = "red")
# No impact while modelling


###4. Modelling 


# Training data - remove october 2015
mytempdata3 <- mytempdata1[!(mytempdata1$date_block_num == 33),]
any(mytempdata3$date_block_num == 33)
mytempdata4 <- mytempdata1[, c(2, 3, 7, 8) ]
mytempdata5 <- mytempdata4[order(mytempdata4$date_block_num), ]

#Testing data (October 2015)
mytempdata7 <- mytempdata1[(mytempdata1$date_block_num == 33),]
mytempdata8 <- mytempdata7[, c(2, 3, 7, 8  )]



##1. K nearest neighbours (KNN):

# Training data - remove october 2015:

mytempdata3 <- mytempdata1[!(mytempdata1$date_block_num == 33),]
any(mytempdata3$date_block_num == 33)
colnames(mytempdata3)

# Testing data (October 2015):

mytempdata4 <- mytempdata1[(mytempdata1$date_block_num == 33),]
colnames(mytempdata4)

# Normalize function

normalize=function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

# Normalizing data:

mytempdata3=mytempdata3[,c(2,3,5,6,7)]
colnames(mytempdata3)
mytempdata3_n=as.data.frame(lapply(mytempdata3[,c(2,3,4,5)],normalize))

mytempdata4 <- mytempdata4[,c(2,3,5,6,7)]
colnames(mytempdata4)
mytempdata4_n=as.data.frame(lapply(mytempdata4[,c(2,3,4,5)],normalize))

mytempdata6=mytempdata3[,c(2,5)]
colnames(mytempdata6)
mytempdata6_n=as.data.frame(lapply(mytempdata6[,], normalize))


# Building model and results:

require(class)
model_knn=knn(train = mytempdata3_n,test = mytempdata4_n, cl=mytempdata3_n$item_cnt_day,k=length(unique(mytempdata3_n$item_cnt_day)))
model_knn
#summary(model_knn)
#predict(model_knn)
#table(mytempdata3_n$item_cnt_day,model_knn)
#CrossTable(x = mytempdata3_n$item_cnt_day, y = model_knn, prop.chisq=FALSE)


## 2. ARIMA:

train_data_arima <- sale_by_month [-34,]
#sales_data <- fread(file.choose(),header = TRUE,sep=",") #sales_train.csv
#sale_by_month <- aggregate(sales_data$item_cnt_day, by=list(date_block_num = sales_data$date_block_num), FUN = sum)
tsdata <- ts(train_data_arima$x, frequency = 12, start = c(2013,1))
plot(tsdata)
autoarima1 <- auto.arima(tsdata)
forecast1 <- forecast(autoarima1,h=2)
forecast1
plot(forecast1)


### 5. Validation:


## KNN:

# 2. Root Mean Square Error:

RMSE <- mytempdata4_n$item_cnt_day - as.numeric(as.character(model_knn))

RMSE=RMSE^2
RMSEout=mean(RMSE)
RMSEout

# 1. Mean Square Error:

RMSEout^1/2

# Validation for Arima model( RMSE and MSE):

summary(autoarima1)


### 6. Computing confidence interval of Model 1 and Model 2 for the following different confidence levels: 80%, 90%, 95%


library(rcompanion)

## 1. KNN:

t.test(mytempdata4,
       conf.level=0.80)

t.test(mytempdata4,
       conf.level=0.90)

t.test(mytempdata4,
       conf.level=0.95)


## 2. ARIMA:

t.test(tsdata, 
       conf.level=0.80)

t.test(tsdata, model_knn, paired = TRUE, 
       conf.level=0.90)

t.test(tsdata, model_knn, paired = TRUE, 
       conf.level=0.95)

### 7. Comparision between Model 1 and Model 2:

## a. Error:

## b. Efficiency in training time(scalability):

