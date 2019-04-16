# title : 101_BTC100_MT.R
# author : hjy

# options
options(scipen = 999)
"%ni%" = Negate("%in%")

# library
library(cryptor)
library(data.table)
library(plotly)
library(lubridate)
library(h2o)

# available exchanges
exchanges = get_exchanges()
head(exchanges)

# available coins
coins = get_coins()
head(coins)

# get data
data = get_historical_price(fsym="BTC", tsym="KRW", end_time="2019-03-22", unit='day',limit=365)
data = as.data.table(data)
tail(data)
range(data$time)

# cut range
data[,date:=as.numeric(gsub("-","",substr(time,1,10)))]
data = data[date>=20190101,]
head(data)

# features
data[,oc_diff:=close-open]
data[,daily_avg:=(open+high+low+close)/4]
data[,daily_avg_After_Month:=shift(daily_avg,30,type="lead")]
tail(data)

# shift
sapply(data,function(x)sum(is.na(x)))
data = data[!is.na(daily_avg_After_Month),]
newdata = data[is.na(daily_avg_After_Month),]
head(data)
head(newdata)

# h2o
h2o.init()

# data h2oframe
data[,time:=as.character(time)]
data_hex = as.h2o(data)
head(data_hex)

# newdata h2oframe
newdata[,time:=as.character(time)]
newdata_hex = as.h2o(newdata)
head(newdata_hex)

# set x and y 
x = colnames(data)[colnames(data) %ni% c('daily_avg_After_Month','daily_avg','date','time')]
y = 'daily_avg_After_Month'

# split dataset
splits = h2o.splitFrame(data_hex,ratios=0.8,seed=43)
train = splits[[1]]
test = splits[[2]]
X_train_BTC = train[x]
y_train_BTC = train[y]
X_test_BTC = test[x]
y_test_BTC = test[y]
X_forecast_BTC = newdata_hex[x]

# BTC100_AML  
BTC100_AML = h2o.automl(
  training_frame = train,
  x = x,
  y = y,
  exclude_algos = "DeepLearning",
  max_models = 100,
  max_runtime_secs = 60*60*10,
  nfolds = 1,
  seed = 1234
)
BTC100_AML@leaderboard
BTC100_AML@leader

# predict
forecasted_BTC = h2o.predict(BTC100_AML@leader,X_forecast_BTC)
forecasted_BTC = as.data.table(forecasted_BTC)
head(forecasted_BTC)

# next 30 days
modified_date = as.Date(max(data$time)) + days(1)
new_date = seq(from=modified_date,to=modified_date+days(29),by=1)

# set YHAT and Y
Y = data[,list(date=as.Date(time),price=daily_avg)]
YHAT = data.table(date=new_date,price=forecasted_BTC$predict)
YHAT = rbind(tail(Y,1),YHAT)

# plot 
plot_ly() %>%
  add_trace(data=Y, x=~date, y=~price, type = 'scatter',  mode = 'lines+markers', line = list(color = 'blue', width = 1), name="실제값") %>%
  add_trace(data=YHAT, x=~date, y=~price, type = 'scatter',  mode = 'lines+markers', line = list(color = 'red', width = 1, dash='dot'), name="예측값") %>% 
  # layout(xaxis=list(fixedrange=T, title="date"),yaxis=list(fixedrange=T, title="price"),autosize = F, width = 900) %>% 
  layout(xaxis=list(fixedrange=T, title="date"),yaxis=list(fixedrange=T, title="price"),autosize = T) %>% 
  config(displayModeBar = F)

# 출력
cat(">> ")






