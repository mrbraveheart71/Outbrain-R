library(data.table)
library(Metrics)
library(xgboost)
library(caret)

setwd("C:/R-Studio/Outbrain/Outbrain R")
# runType='Train'
# source("Outbrain-Pre-Process.R")

#preProcess("Train")

# Get Display samples to down-scale data
# do an xgboost

load("Outbrain XGBOOST Data")

display_ids <- unique(clicks_train$display_id)
display_ids <- sample(display_ids,3000000)
clicks_train <- clicks_train[clicks_train$display_id %in% display_ids]
gc()
strainDisplayIds <- sample(display_ids, length(display_ids)*0.8)
svalidDisplayIds <- setdiff(display_ids, strainDisplayIds)
strain <- which(clicks_train$display_id %in% strainDisplayIds)
svalid <- which(clicks_train$display_id %in% svalidDisplayIds)

xgb_params = list(
  seed = 0,colsample_bytree = 0.9,colsample_bylevel=1,subsample = 0.9,
  eta = 0.1,max_depth =10,num_parallel_tree = 1,
  min_child_weight = 10,
  #objective='binary:logistic'
  objective="reg:linear"
)
gc()
clicks_train <- clicks_train[,c(feature.names,"clicked","ad_id","display_id"),with=FALSE]
gc()
dtrain = xgb.DMatrix(as.matrix(clicks_train[strain,feature.names,with=FALSE]), label=clicks_train$clicked[strain], missing=NA)
dvalid = xgb.DMatrix(as.matrix(clicks_train[svalid,feature.names,with=FALSE]), label=clicks_train$clicked[svalid], missing=NA)
gc()

watchlist <- list(valid=dvalid,train=dtrain)
xgboost.fit <- xgb.train (data=dtrain,xgb_params,missing=NA,early.stop.round = 5,
                          #eval_metric="auc",
                          #eval_metric="logloss",
                          nrounds=50,
                          maximize=FALSE,verbose=1,watchlist = watchlist)

# Valid Score of 35.55
save("xgboost.fit",file="Outbrain XGBOOST Model")
load("Outbrain XGBOOST Model")

importance_matrix <- xgb.importance(model = xgboost.fit, feature.names)
options(scipen=999)
print(importance_matrix,200)

rm(dtrain,dvalid)
rm(agg.tables.train)
gc()
clicks_train <- clicks_train[,c(feature.names,"clicked","ad_id","display_id"),with=FALSE]
gc()
dall = xgb.DMatrix(as.matrix(clicks_train[,feature.names,with=FALSE]), label=clicks_train$clicked, missing=NA)
clicks_train <- clicks_train[,c("ad_id","display_id","clicked"),with=FALSE]
gc()
clicks_train$preds <- predict(xgboost.fit,newdata=dall)

# in sample testing
#
setkey(clicks_train,"preds")
train.preds <- clicks_train[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
train.preds <- train.preds[order(train.preds$display_id)]
clicks_train <- clicks_train[order(clicks_train$display_id)]
clicks_train_clicked <- clicks_train[clicks_train$clicked==1]

# now just test validation Displays
s <- sample(svalidDisplayIds,200000)
#s <- sample(valid_display_ids,100000)

predicted <- lapply(train.preds$ad_id[train.preds$display_id %in% s], function(x) as.numeric(tstrsplit(x,split=" ")))
actual <- as.list(clicks_train_clicked$ad_id[clicks_train_clicked$display_id %in% s])
  
map12 <- mapk(12, actual, predicted)
print(map12)
# Results was 66.1


#
# Now prepare test
#
library(data.table)
library(Metrics)
library(xgboost)

setwd("C:/R-Studio/Outbrain/Outbrain R")
#runType <- "Test"
#source("Outbrain-Pre-Process.R")
#rm(clicks_train)
gc()
load("Outbrain XGBOOST Data Test")
load("Outbrain XGBOOST Model")

# Now prepare submission
# Break it into two pieces
myseq <- c(seq(1,nrow(clicks_test),nrow(clicks_test)/20),nrow(clicks_test)+1)

for (i in 1:20) {
  print(i)
  currSeq <- myseq[i]:(myseq[i+1]-1)
  dtest = xgb.DMatrix(as.matrix(clicks_test[currSeq,feature.names,with=FALSE]),missing=NA)
  clicks_test[currSeq, preds := predict(xgboost.fit,newdata=dtest)]
}

# Work in the leakage
load("Outbrain Data Leakage")
leaks$leak_prob <- 1
leaks <- unique(leaks)
setkeyv(clicks_test,c("display_id","ad_id"))
setkeyv(leaks,c("display_id","ad_id"))
clicks_test[,leak_prob:=leaks[clicks_test[,list(display_id,ad_id)],list(leak_prob)]]
clicks_test[,preds:=ifelse(!is.na(clicks_test$leak_prob),1,clicks_test$preds)]
#clicks_train[,ad_category_prob:=ad_category_metrics[clicks_train[,list(ad_id,category_id)],list(ad_category_prob)]]

setkey(clicks_test,"preds")
submission <- clicks_test[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
setkey(submission,"display_id")

write.csv(submission,file = "submission.csv",row.names = F)


# Feature         Gain       Cover   Frequence
# 1:          ad_publisher_prob 0.3796257059 0.024709590 0.028405909
# 2:     advertiser_source_prob 0.1618886856 0.024545582 0.036395744
# 3:            ad_display_prob 0.1152892634 0.280006705 0.167528104
# 4:             ad_source_prob 0.0650963953 0.033011788 0.027199897
# 5:                   ad_count 0.0575255437 0.109248534 0.055045872
# 6:                ad_doc_prob 0.0444348563 0.067157079 0.038980058
# 7:           ad_category_prob 0.0326912884 0.025362171 0.038484731
# 8:  advertiser_publisher_prob 0.0311409044 0.026137132 0.036934143
# 9:           ad_platform_prob 0.0306735391 0.074718225 0.066782961
# 10:                       prob 0.0259432452 0.053482794 0.066955248
# 11:               ad_doc_count 0.0097565492 0.037331350 0.028965844
# 12:        advertiser_geo_prob 0.0081237345 0.044038702 0.058814662
# 14:         ad_publisher_count 0.0045256920 0.016083172 0.021966662
# 15:           ad_display_count 0.0040501108 0.029023701 0.051600121
# 16:            ad_source_count 0.0040020665 0.026206800 0.023947969
# 17:    advertiser_source_count 0.0032855239 0.021788683 0.030301072
# 18:          ad_category_count 0.0027932999 0.016952212 0.031420942
# 19: advertiser_publisher_count 0.0023520762 0.014351507 0.028018262
# 20:       advertiser_geo_count 0.0022540578 0.009583748 0.038807770
# 22:          ad_platform_count 0.0019961679 0.010114789 0.022311237
# 23:                      count 0.0018861331 0.014328634 0.022418917
# Feature         Gain       Cover   Frequence

# [0]	valid-rmse:0.477020	train-rmse:0.477969
# [1]	valid-rmse:0.457318	train-rmse:0.457605
# [2]	valid-rmse:0.440487	train-rmse:0.440707
# [3]	valid-rmse:0.426356	train-rmse:0.426494
# [4]	valid-rmse:0.414542	train-rmse:0.413979
# [5]	valid-rmse:0.404744	train-rmse:0.404838
# [6]	valid-rmse:0.396630	train-rmse:0.395977
# [7]	valid-rmse:0.389983	train-rmse:0.389735
# [8]	valid-rmse:0.384429	train-rmse:0.384237
# [9]	valid-rmse:0.379858	train-rmse:0.379258
# [10]	valid-rmse:0.376136	train-rmse:0.375421
# [11]	valid-rmse:0.373041	train-rmse:0.372524
# [12]	valid-rmse:0.370553	train-rmse:0.370119
# [13]	valid-rmse:0.368466	train-rmse:0.368052
# [14]	valid-rmse:0.366786	train-rmse:0.366278
# [15]	valid-rmse:0.365412	train-rmse:0.364832
# [16]	valid-rmse:0.364285	train-rmse:0.363713
# [17]	valid-rmse:0.363354	train-rmse:0.362747
# [18]	valid-rmse:0.362609	train-rmse:0.361872
# [19]	valid-rmse:0.361999	train-rmse:0.361111
# [20]	valid-rmse:0.361504	train-rmse:0.360458
# [21]	valid-rmse:0.361103	train-rmse:0.359920
# [22]	valid-rmse:0.360769	train-rmse:0.359472
# [23]	valid-rmse:0.360495	train-rmse:0.359102
# [24]	valid-rmse:0.360266	train-rmse:0.358790
# [25]	valid-rmse:0.360078	train-rmse:0.358533
# [26]	valid-rmse:0.359918	train-rmse:0.358304
# [27]	valid-rmse:0.359786	train-rmse:0.358109
# [28]	valid-rmse:0.359673	train-rmse:0.357944
# [29]	valid-rmse:0.359579	train-rmse:0.357802
# [30]	valid-rmse:0.359500	train-rmse:0.357686
# [31]	valid-rmse:0.359430	train-rmse:0.357581
# [32]	valid-rmse:0.359375	train-rmse:0.357487
# [33]	valid-rmse:0.359323	train-rmse:0.357406
# [34]	valid-rmse:0.359282	train-rmse:0.357329
# [35]	valid-rmse:0.359244	train-rmse:0.357261
# [36]	valid-rmse:0.359210	train-rmse:0.357205
# [37]	valid-rmse:0.359184	train-rmse:0.357152
# [38]	valid-rmse:0.359160	train-rmse:0.357099
# [39]	valid-rmse:0.359134	train-rmse:0.357041
# [40]	valid-rmse:0.359117	train-rmse:0.356993
# [41]	valid-rmse:0.359099	train-rmse:0.356947
# [42]	valid-rmse:0.359086	train-rmse:0.356903
# [43]	valid-rmse:0.359074	train-rmse:0.356864
# [44]	valid-rmse:0.359058	train-rmse:0.356815
# [45]	valid-rmse:0.359046	train-rmse:0.356775
# [46]	valid-rmse:0.359034	train-rmse:0.356736
# [47]	valid-rmse:0.359024	train-rmse:0.356705
# [48]	valid-rmse:0.359013	train-rmse:0.356670
# [49]	valid-rmse:0.359004	train-rmse:0.356632