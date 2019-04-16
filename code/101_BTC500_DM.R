# title : 101_BTC500_DM
# author : hjy

# data
tmp = get_historical_price(fsym=COIN_NAME, tsym="KRW", end_time=Sys.Date(), unit='day',limit=1000)
data = as.data.table(tmp)

# date
colnames(data) = c("time","close","high","low","open","volume_of_coin","volume_of_money")
data[,date:=as.numeric(gsub("-","",substr(time,1,10)))]
data[,month:=as.factor(substr(date,5,6))]
data[,day:=as.factor(substr(date,7,8))]
data[,day_of_week:=as.factor(weekdays(as.Date(substr(time,1,10),abbreviate=F)))]
head(data)

# 
data[,평균가:=(close+high+low+open)/4]
data[,전일종가:=shift(close,n = 1,type = "lag",fill = 770001)]
data[,고가저가차이:=high-low]

#
# data[,open_log:=log(open)]
# data[,high_log:=log(high)]
# data[,low_log:=log(low)]
# data[,close_log:=log(close)]
# data[,평균가_log:=log(평균가)]
# data[,고가저가차이_log:=log(고가저가차이_log)]

# 
data[,전일종가대비종가증가액:=close-전일종가]
data[,전일종가대비종가증가율:=전일종가대비종가증가액/전일종가]
data[,전일종가대비종가상승YN:=ifelse(전일종가대비종가증가율>=0,1,0)]

# 
data[,전일종가대비고가증가액:=high-전일종가]
data[,전일종가대비고가증가율:=전일종가대비고가증가액/전일종가]
data[,전일종가대비고가상승YN:=ifelse(전일종가대비고가증가율>=0,1,0)]

# 
data[,전일종가대비저가증가액:=low-전일종가]
data[,전일종가대비저가증가율:=전일종가대비저가증가액/전일종가]
data[,전일종가대비저가상승YN:=ifelse(전일종가대비저가증가율>=0,1,0)]

# 
data[,전일종가대비평균가증가액:=평균가-전일종가]
data[,전일종가대비평균가증가율:=전일종가대비평균가증가액/전일종가]
data[,전일종가대비평균가상승YN:=ifelse(전일종가대비평균가증가율>=0,1,0)]

#
data[,심플이동편균선종가n2:=movavg(close,n=2,type = "s")]
data[,심플이동편균선종가n3:=movavg(close,n=3,type = "s")]
data[,심플이동편균선종가n5:=movavg(close,n=5,type = "s")]
data[,심플이동편균선종가n6:=movavg(close,n=7,type = "s")]

#
data[,심플이동편균선고가n2:=movavg(high,n=2,type = "s")]
data[,심플이동편균선고가n3:=movavg(high,n=3,type = "s")]
data[,심플이동편균선고가n5:=movavg(high,n=5,type = "s")]
data[,심플이동편균선고가n6:=movavg(high,n=7,type = "s")]

#
data[,심플이동편균선저가n2:=movavg(low,n=2,type = "s")]
data[,심플이동편균선저가n3:=movavg(low,n=3,type = "s")]
data[,심플이동편균선저가n5:=movavg(low,n=5,type = "s")]
data[,심플이동편균선저가n6:=movavg(low,n=7,type = "s")]

#
data[,심플이동편균선코인볼륨n2:=movavg(volume_of_coin,n=2,type = "s")]
data[,심플이동편균선코인볼륨n3:=movavg(volume_of_coin,n=3,type = "s")]
data[,심플이동편균선코인볼륨n5:=movavg(volume_of_coin,n=5,type = "s")]
data[,심플이동편균선코인볼륨n6:=movavg(volume_of_coin,n=7,type = "s")]

#
data[,심플이동편균선금액볼륨n2:=movavg(volume_of_money,n=2,type = "s")]
data[,심플이동편균선금액볼륨n3:=movavg(volume_of_money,n=3,type = "s")]
data[,심플이동편균선금액볼륨n5:=movavg(volume_of_money,n=5,type = "s")]
data[,심플이동편균선금액볼륨n6:=movavg(volume_of_money,n=7,type = "s")]

#
data[,최근2일종가최대값:=runmax(close,k = 2)]
data[,최근3일종가최대값:=runmax(close,k = 3)]
data[,최근5일종가최대값:=runmax(close,k = 5)]
data[,최근7일종가최대값:=runmax(close,k = 7)]

#
data[,종가최근2일종가최대값차이:=close-최근2일종가최대값]
data[,종가최근3일종가최대값차이:=close-최근3일종가최대값]
data[,종가최근5일종가최대값차이:=close-최근5일종가최대값]
data[,종가최근7일종가최대값차이:=close-최근7일종가최대값]

#
data[,종가최근2일종가최대값상승YN:=ifelse(종가최근2일종가최대값차이>=0,1,0)]
data[,종가최근3일종가최대값상승YN:=ifelse(종가최근3일종가최대값차이>=0,1,0)]
data[,종가최근5일종가최대값상승YN:=ifelse(종가최근5일종가최대값차이>=0,1,0)]
data[,종가최근7일종가최대값상승YN:=ifelse(종가최근7일종가최대값차이>=0,1,0)]
data[,종가최근2357일종가최대값상승YN문자:=as.factor(paste0(종가최근2일종가최대값상승YN,종가최근3일종가최대값상승YN,종가최근5일종가최대값상승YN,종가최근5일종가최대값상승YN))]

#
data[,최근2일종가최소값:=runmin(close,k = 2)]
data[,최근3일종가최소값:=runmin(close,k = 3)]
data[,최근5일종가최소값:=runmin(close,k = 5)]
data[,최근7일종가최소값:=runmin(close,k = 7)]

#
data[,종가최근2일종가최소값차이:=close-최근2일종가최소값]
data[,종가최근3일종가최소값차이:=close-최근3일종가최소값]
data[,종가최근5일종가최소값차이:=close-최근5일종가최소값]
data[,종가최근7일종가최소값차이:=close-최근7일종가최소값]

#
data[,종가최근2일종가최소값상승YN:=ifelse(종가최근2일종가최소값차이>0,1,0)]
data[,종가최근3일종가최소값상승YN:=ifelse(종가최근3일종가최소값차이>0,1,0)]
data[,종가최근5일종가최소값상승YN:=ifelse(종가최근5일종가최소값차이>0,1,0)]
data[,종가최근7일종가최소값상승YN:=ifelse(종가최근7일종가최소값차이>0,1,0)]
data[,종가최근2357일종가최소값상승YN문자:=as.factor(paste0(종가최근2일종가최소값상승YN,종가최근3일종가최소값상승YN,종가최근5일종가최소값상승YN,종가최근5일종가최소값상승YN))]

#
data[,최근1일종가상승YN:=ifelse(close-shift(close,n = 1,type = "lag",fill = 770001)>=0,1,0)]
data[,최근2일종가상승YN:=ifelse(shift(close,n = 1,type = "lag",fill = 770001)-shift(close,n = 2,type = "lag",fill = 770001)>=0,1,0)]
data[,최근3일종가상승YN:=ifelse(shift(close,n = 2,type = "lag",fill = 770001)-shift(close,n = 3,type = "lag",fill = 770001)>=0,1,0)]
data[,최근4일종가상승YN:=ifelse(shift(close,n = 3,type = "lag",fill = 770001)-shift(close,n = 4,type = "lag",fill = 770001)>=0,1,0)]
data[,최근1234종가상승YN문자:=as.factor(paste0(최근1일종가상승YN,최근2일종가상승YN,최근3일종가상승YN,최근4일종가상승YN))]

# 
data[,전일종가상승폭1p:=ifelse(floor((close-전일종가)/전일종가*100)==1,1,0)]
data[,전일종가상승폭2p:=ifelse(floor((close-전일종가)/전일종가*100)==2,1,0)]
data[,전일종가상승폭3p:=ifelse(floor((close-전일종가)/전일종가*100)==3,1,0)]
data[,전일종가상승폭3p이상:=ifelse(floor((close-전일종가)/전일종가*100)>=3,1,0)]
data[,전일종가상승폭5p이상:=ifelse(floor((close-전일종가)/전일종가*100)>=5,1,0)]
data[,전일종가상승폭10p이상:=ifelse(floor((close-전일종가)/전일종가*100)>=10,1,0)]

# 출력
fwrite(head(data,100),file.path(PATH_HOME,"output",paste0(COIN_NAME,"_DM.csv")))
cat(">> 101 done! \n")

