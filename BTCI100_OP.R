# title: bitcoin op
# author : hjy

# library
library(data.table)
library(plotly)

# read data 
data_w_yhat = fread("./BITCOIN/output/backtest_20190222.csv")
data_w_y = fread("./BITCOIN/output/realdata_20190320.csv")

# date 
data_w_yhat[,ymd:=as.numeric(gsub("-","",substr(V1,1,10)))]
data_w_yhat[,time:=gsub(" ","",substr(V1,11,19))]
data_w_yhat = data_w_yhat[time=="22:00:00",]
data_w_y[,ymd:=as.numeric(gsub("-","",substr(date,1,10)))]
data_w_y[,time:=gsub(" ","",substr(date,11,19))]
data_w_y = data_w_y[time=="22:00:00",]

# cut range
data_w_yhat = data_w_yhat[ymd>=20190222 & ymd<20190320,list(date=V1,Yhat=daily_avg)]
data_w_y = data_w_y[ymd>=20190222 & ymd<20190320,list(date,Y=close)]

# merge
data = merge(data_w_y,data_w_yhat,by="date",all.y=T)
data[,date:=substr(date,1,10)]

# plot
ply = plot_ly(data, x = ~date) %>%
  add_trace(y = ~Y, type = 'scatter', mode = 'lines', line = list(color = 'blue', width = 1), name="실제값") %>%
  add_trace(y = ~Yhat, type = 'scatter', mode = 'lines', line = list(color = 'red', width = 1, dash='dot'), name="예측값") %>% 
  layout(title="2019년 3월 비트코인 예측결과",xaxis=list(fixedrange=TRUE, title="date"),yaxis=list(fixedrange=TRUE, title="price")) %>% 
  config(displayModeBar = F)

htmlwidgets::saveWidget(as_widget(ply), "./BTCI100_OP_201903.html")
