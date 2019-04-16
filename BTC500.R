# title : BTC500
# author : hjy

# save memory
rm(list=ls())
gc(reset=T)

# options
options(scipen = 999)
Sys.setlocale("LC_TIME", "English")

# library
library(cryptor)
library(data.table)
library(plotly)
library(lubridate)
library(pracma)
library(caTools)
library(h2o)
library(htmltools)
library(htmlwidgets)

# user functions
range01 = function(x){(x-min(x))/(max(x)-min(x))}
"%ni%" = Negate("%in%")

# set PATH
PATH_HOME = "c:/Users/user/Documents/BITCOIN"

# h2o
h2o.init()
h2o.removeAll()

# predict_days 
predict_days = 7

# train options (1hour: 60*60)
max_runtime_secs = 60*60
max_models = 50
nfolds = 5
stopping_rounds = 20
stopping_tolerance = 0.001
simple = TRUE

# BTC
tmp = c("BTC","BCH","ETH","XRP")
for(i in tmp){
  COIN_NAME = i
  cat(">> start..",COIN_NAME,"\n")
  source(file.path(PATH_HOME,"code","101_BTC500_DM.R"))
  source(file.path(PATH_HOME,"code","201_BTC500_ML.R"))
  source(file.path(PATH_HOME,"code","202_BTC500_ML.R"))
  source(file.path(PATH_HOME,"code","203_BTC500_ML.R"))
  source(file.path(PATH_HOME,"code","204_BTC500_ML.R"))
}

# dashboard
source(file.path(PATH_HOME,"code","301_RPT500_OP.R"))






