# title : 102_BTC100_DM
# author : hjy

# time
data[,date:=paste(substr(date,1,4),substr(date,5,6),substr(date,7,8),sep="-")]
head(data)

# aggregate
data = data[,list(
  time_yday=yday(date),
  time_mday=mday(date),
  time_wday=wday(date),
  time_wdays=weekdays(as.Date(date,abbreviate=T)),
  close_MAX=max(close,na.rm=T),
  close_MIN=min(close,na.rm=T),
  close_vAR=var(close,na.rm=T),
  high_MAX=max(high,na.rm=T),
  high_MIN=min(high,na.rm=T),
  high_VAR=var(high,na.rm=T),
  low_MAX=max(low,na.rm=T),
  low_MIN=min(low,na.rm=T),
  low_VAR=var(low,na.rm=T),
  open_MAX=max(open,na.rm=T),
  open_MIN=min(open,na.rm=T),
  open_VAR=var(open,na.rm=T),
  volumefrom_MAX=max(volumefrom,na.rm=T),
  volumefrom_MIN=min(volumefrom,na.rm=T),
  volumefrom_VAR=var(volumefrom,na.rm=T),
  volumeto_MAX=max(volumeto,na.rm=T),
  volumeto_MIN=min(volumeto,na.rm=T),
  volumeto_VAR=var(volumeto,na.rm=T),
  close=mean(close,na.rm=T),
  high=mean(high,na.rm=T),
  low=mean(low,na.rm=T),
  open=mean(open,na.rm=T),
  volumefrom=mean(volumefrom,na.rm=T),
  volumeto=mean(volumeto,na.rm=T),
  close_MED=median(close,na.rm=T),
  high_MED=median(high,na.rm=T),
  low_MED=median(low,na.rm=T),
  open_MED=median(open,na.rm=T),
  volumefrom_MED=median(volumefrom,na.rm=T),
  volumeto_MED=median(volumeto,na.rm=T)
),by=list(date)]
head(data)

# kor to eng
data[,time_wdays:=ifelse(time_wdays=="월요일","mon",time_wdays)]
data[,time_wdays:=ifelse(time_wdays=="화요일","tue",time_wdays)]
data[,time_wdays:=ifelse(time_wdays=="수요일","wed",time_wdays)]
data[,time_wdays:=ifelse(time_wdays=="목요일","tur",time_wdays)]
data[,time_wdays:=ifelse(time_wdays=="금요일","fri",time_wdays)]
data[,time_wdays:=ifelse(time_wdays=="토요일","sat",time_wdays)]
data[,time_wdays:=ifelse(time_wdays=="일요일","sun",time_wdays)]
data[,time_wdays:=as.factor(time_wdays)]

# derived features
data[,oc_diff:=close-open]
data[,daily_avg:=(open+high+low+close)/4]

# lag and diff
data[,daily_avg_lag1:=shift(daily_avg,type = "lag",n=1)]
data[,daily_avg_lag2:=shift(daily_avg,type = "lag",n=2)]
data[,daily_avg_lag3:=shift(daily_avg,type = "lag",n=3)]
data[,daily_avg_diff1:=daily_avg-daily_avg_lag1]
data[,daily_avg_diff2:=daily_avg-daily_avg_lag2]
data[,daily_avg_diff3:=daily_avg-daily_avg_lag3]

# lag and diff
data[,volumeto_lag1:=shift(volumeto,type = "lag",n=1)]
data[,volumeto_lag2:=shift(volumeto,type = "lag",n=2)]
data[,volumeto_lag3:=shift(volumeto,type = "lag",n=3)]
data[,volumeto_diff1:=volumeto-volumeto_lag1]
data[,volumeto_diff2:=volumeto-volumeto_lag2]
data[,volumeto_diff3:=volumeto-volumeto_lag3]
head(data)

# 출력
data = data[complete.cases(data),]
cat(">> 101_BTC100_DM done! \n")


