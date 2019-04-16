# title : 202_BTC200_ML.R
# author : hjy

# predict_days
predict_days = 2

# shift
data_tmp = data 
data_tmp[,daily_avg_After_2day:=shift(daily_avg,predict_days,type="lead")]
newdata = data_tmp[is.na(daily_avg_After_2day),]
xydata = data_tmp[!is.na(daily_avg_After_2day),]

# h2o
h2o.removeAll()

# data h2oframe
data_hex = as.h2o(xydata)
head(data_hex)

# newdata h2oframe
newdata_hex = as.h2o(newdata)
head(newdata_hex)

# set x and y 
x = colnames(data_hex)[colnames(data_hex) %ni% c('date','daily_avg_After_2day')]
y = 'daily_avg_After_2day'

# X_forecast_BTC
X_forecast_BTC = newdata_hex[x]

# BTC100_AML  
BTC100_AML = h2o.automl(
  training_frame = data_hex, # train
  x = x,
  y = y,
  exclude_algos = "DeepLearning",
  max_runtime_secs = 60*60*5,
  max_models = max_models,
  nfolds = nfolds,
  stopping_rounds = stopping_rounds,
  stopping_tolerance = stopping_tolerance,
  stopping_metric = "RMSLE",
  seed = 1234
)
BTC100_AML@leaderboard
BTC100_AML@leader

# predict
forecasted_BTC = h2o.predict(BTC100_AML@leader,X_forecast_BTC)
forecasted_BTC = as.data.table(forecasted_BTC)
head(forecasted_BTC)

# next 2 days
modified_date = as.Date(max(data_tmp$date)) + days(1)
new_date = seq(from=modified_date,to=modified_date+days(predict_days-1),by=1)
YHAT_list[['daily_avg_After_2day']] = data.table(date=as.character(new_date),price=forecasted_BTC$predict)

# Ãâ·Â
cat(">> 202_BTC200_ML done! \n")


