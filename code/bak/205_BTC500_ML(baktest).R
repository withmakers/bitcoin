# title : 205
# author : hjy

# user function
baktest = function(data,start,end,coin,target){
  
  TARGET = target
  COIN_NAME = coin 
  
  # ML_DIR
  if(TARGET=="close"){
    ML_DIR = paste0("201_",COIN_NAME)
  }else if(TARGET=="open"){
    ML_DIR = paste0("201_",COIN_NAME)
  }else if(TARGET=="high"){
    ML_DIR = paste0("203_",COIN_NAME)
  }else if(TARGET=="low"){
    ML_DIR = paste0("204_",COIN_NAME)
  }
  
  # data_TMP
  data_TMP = data[date>=start & date<=end,]
  cat(">> input date:",range(data_TMP$date),"\n")
  colnames(data_TMP) = paste0("x",1:ncol(data))

  # h2o.frame
  data_TMP[,':='(x1=NULL,x8=NULL)]
  data_hex = as.h2o(data_TMP)
  
  # load model 
  ML_NAME = list.files(file.path(PATH_HOME,"output",ML_DIR))
  ML_PATH = file.path(PATH_HOME,"output",ML_DIR,ML_NAME)
  ml = h2o.loadModel(ML_PATH)
  
  # pred
  pred = h2o.predict(ml,newdata=data_hex)
  pred = as.data.table(pred)
  colnames(pred) = TARGET

  # return 
  modified_date = as.Date(max(data[date>=start & date<=end,]$time)) + days(1)
  new_date = seq(from=modified_date,to=modified_date+days(predict_days-1),by=1)
  new_date = as.numeric(gsub("-","",new_date))
  cat(">> new date:",range(new_date),"\n")
  list(
    YHAT=data.table(date=new_date,pred),
    Y=data[date>=min(new_date) & date<=max(new_date),c("date",TARGET),with=F]
  )
}

# baktest_plot
baktest_plot = function(data_close,data_open,data_high,data_low){
  
  YHAT = Reduce(function(...)merge(...,by="date"),list(data_close$YHAT,data_open$YHAT,data_high$YHAT,data_low$YHAT))
  YHAT[,date:=as.character(date)]
  Y = Reduce(function(...)merge(...,by="date"),list(data_close$Y,data_open$Y,data_high$Y,data_low$Y))
  Y[,date:=as.character(date)]
  
  plot_ly() %>%
    add_trace(data=Y, x = ~date, type="candlestick",open = ~open, close = ~close, high = ~high, low = ~low, 
              increasing = list(line = list(color = "red")),decreasing = list(line = list(color = "blue")),name="실제") %>%
    add_trace(data=YHAT, x = ~date, type="candlestick",open = ~open, close = ~close, high = ~high, low = ~low, increasing = 
                list(line = list(color = "pink")),decreasing = list(line = list(color = "skyblue")),name="예측") %>%
    layout(xaxis=list(fixedrange=T, title="date",rangeslider = list(visible = F)),yaxis=list(fixedrange=T, title="price"),autosize = T) %>% 
    config(displayModeBar = F)
}

# up and down 
date_updown = list(up=c(20190324,20190330),down=c(20190331,20190406))

# data
COIN_NAME = "BTC"
source(file.path(PATH_HOME,"code","101_BTC500_DM.R"))

# baktest
data_close = baktest(data=data,start=date_updown$up[1],end=date_updown$up[2],coin=COIN_NAME,target="close")
data_open = baktest(data=data,start=date_updown$up[1],end=date_updown$up[2],coin=COIN_NAME,target="open")
data_high = baktest(data=data,start=date_updown$up[1],end=date_updown$up[2],coin=COIN_NAME,target="high")
data_low = baktest(data=data,start=date_updown$up[1],end=date_updown$up[2],coin=COIN_NAME,target="low")
baktest_plot(data_close,data_open,data_high,data_low)


# data
COIN_NAME = "BTC"
source(file.path(PATH_HOME,"code","101_BTC500_DM.R"))

# baktest
data_close = baktest(data=data,start=date_updown$down[1],end=date_updown$down[2],coin=COIN_NAME,target="close")
data_open = baktest(data=data,start=date_updown$down[1],end=date_updown$down[2],coin=COIN_NAME,target="open")
data_high = baktest(data=data,start=date_updown$down[1],end=date_updown$down[2],coin=COIN_NAME,target="high")
data_low = baktest(data=data,start=date_updown$down[1],end=date_updown$down[2],coin=COIN_NAME,target="low")
baktest_plot(data_close,data_open,data_high,data_low)


