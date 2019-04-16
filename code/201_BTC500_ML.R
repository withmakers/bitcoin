# title : 201_BTC500_ML.R
# author : hjy

# data_TMP
data_TMP = data
colnames(data_TMP) = paste0("x",1:ncol(data))
data.table(colnames(data_TMP),colnames(data))[V2=="time"]
data.table(colnames(data_TMP),colnames(data))[V2=="date"]
data.table(colnames(data_TMP),colnames(data))[V2=="open"]
data.table(colnames(data_TMP),colnames(data))[V2=="close"]
data.table(colnames(data_TMP),colnames(data))[V2=="high"]
data.table(colnames(data_TMP),colnames(data))[V2=="low"]

# shift
data_TMP[,':='(x1=NULL,x8=NULL)]
data_TMP[,TARGET:=shift(x2,predict_days,type="lead")]
newdata = data_TMP[is.na(TARGET),]
xydata = data_TMP[!is.na(TARGET),]
head(data_TMP)

# h2o
h2o.removeAll()

# data h2oframe
data_hex = as.h2o(xydata)
head(data_hex)

# newdata h2oframe
newdata_hex = as.h2o(newdata)
head(newdata_hex)

# set x and y 
x = colnames(data_hex)[colnames(data_hex) %ni% c('TARGET')]
y = 'TARGET'

# ml  
if(simple){
  ml = h2o.gbm(
    training_frame = data_hex,
    x = x,
    y = y,
    nfolds = nfolds,
    stopping_rounds = stopping_rounds,
    stopping_tolerance = stopping_tolerance,
    stopping_metric = "RMSLE",
    seed = 201
  )
  unlink(file.path(PATH_HOME,"output",paste0("201_",COIN_NAME),"*"))
  h2o.saveModel(ml,path=file.path(PATH_HOME,"output",paste0("201_",COIN_NAME)),force = T)
}else{
  ml = h2o.automl(
    training_frame = data_hex, # train
    x = x,
    y = y,
    exclude_algos = c("DeepLearning","StackedEnsemble"),
    max_runtime_secs = max_runtime_secs,
    max_models = max_models,
    nfolds = nfolds,
    stopping_rounds = stopping_rounds,
    stopping_tolerance = stopping_tolerance,
    stopping_metric = "RMSLE",
    seed = 201
  )
  unlink(file.path(PATH_HOME,"output",paste0("201_",COIN_NAME),"*"))
  h2o.saveModel(ml@leader,path=file.path(PATH_HOME,"output",paste0("201_",COIN_NAME)),force = T)
  ml = ml@leader
}

# predict
pred = h2o.predict(ml,newdata_hex[x])
pred = as.data.table(pred)
print(pred)

# next 30 days
modified_date = as.Date(max(data$time)) + days(1)
new_date = seq(from=modified_date,to=modified_date+days(predict_days-1),by=1)
new_date = as.numeric(gsub("-","",new_date))

# set YHAT and Y
data[,idx:=as.numeric(gsub("-","",substr(date,1,10)))]
data = data[idx>=20190101,]
Y = data[,list(date=as.character(date),close=close)]
YHAT = data.table(date=as.character(new_date),close=pred$predict)
YHAT = rbind(tail(Y,1),YHAT)
data.table(tail(Y),head(YHAT))
saveRDS(list("Y"=Y,"YHAT"=YHAT),file.path(PATH_HOME,"output",paste0(COIN_NAME,"_close.Rda")))

# Ãâ·Â
cat(">> 201 done! \n")


