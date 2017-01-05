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
#display_ids <- sample(display_ids,2000000)
display_ids <- sample(display_ids,200000)
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
  objective='rank:pairwise'
  #objective='binary:logistic'
  #objective="reg:linear"
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
                          nrounds=1000,
                          maximize=FALSE,verbose=1,watchlist = watchlist)

# Valid Score of 35.67
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

# 1:           ad_publisher_prob 0.395979736 0.04913857 0.03778994
# 2:      advertiser_source_prob 0.101545113 0.04092980 0.03902237
# 3:   advertiser_publisher_prob 0.065282028 0.04080149 0.03763427
# 4: ad_publisher_display_second 0.045686845 0.02126597 0.03976182
# 5:  ad_publisher_display_first 0.042771272 0.02850723 0.03239323
# 6:              ad_source_prob 0.030754152 0.04525058 0.03567537
# 7:           ad_display_second 0.028321475 0.03362846 0.04879093
# 8:                 ad_doc_prob 0.026611146 0.06515826 0.04304395
# 9:           ad_display_fourth 0.025774088 0.02508839 0.03409268
# 10:            ad_platform_prob 0.025592985 0.07338240 0.04380935
# 11:            ad_category_prob 0.024753318 0.04684926 0.04489907
# 12:                    ad_count 0.022363762 0.04214993 0.01600851
# 13:  ad_publisher_display_third 0.020429435 0.01689444 0.03509159
# 14:            ad_display_third 0.018562161 0.02234541 0.03974885
# 15:            ad_display_first 0.017845149 0.04301575 0.04358881
# 16:             ad_display_five 0.015193451 0.02340816 0.02812516
# 17:     ad_source_display_first 0.014901981 0.03366352 0.03480619
# 18:         ad_user_source_prob 0.012257212 0.05159957 0.04518447
# 19:                        prob 0.011250817 0.05663893 0.03930777
# 20:    ad_source_display_second 0.011159929 0.02193826 0.03585699
# 21:                ad_doc_count 0.008305919 0.03105307 0.03175756
# 22:     ad_source_display_third 0.007427144 0.01865051 0.03156297
# 23:     advertiser_source_count 0.005953639 0.03579462 0.03620725
# 24:           ad_platform_count 0.005808471 0.03907916 0.03830886
# 25:  advertiser_publisher_count 0.005569484 0.03755646 0.03703752
# 26:        ad_user_source_count 0.005160749 0.02943728 0.03511753
# 27:           ad_category_count 0.004738540 0.02677453 0.03537699
# Feature        Gain      Cover  Frequence

# [0]	valid-rmse:0.476697	train-rmse:0.478204
# [1]	valid-rmse:0.456973	train-rmse:0.457004
# [2]	valid-rmse:0.440246	train-rmse:0.440209
# [3]	valid-rmse:0.426146	train-rmse:0.426304
# [4]	valid-rmse:0.414380	train-rmse:0.413849
# [5]	valid-rmse:0.404518	train-rmse:0.404280
# [6]	valid-rmse:0.396307	train-rmse:0.395723
# [7]	valid-rmse:0.389531	train-rmse:0.389174
# [8]	valid-rmse:0.383912	train-rmse:0.383483
# [9]	valid-rmse:0.379289	train-rmse:0.378574
# [10]	valid-rmse:0.375492	train-rmse:0.374745
# [11]	valid-rmse:0.372369	train-rmse:0.371821
# [12]	valid-rmse:0.369779	train-rmse:0.369062
# [13]	valid-rmse:0.367638	train-rmse:0.366821
# [14]	valid-rmse:0.365914	train-rmse:0.365032
# [15]	valid-rmse:0.364484	train-rmse:0.363643
# [16]	valid-rmse:0.363299	train-rmse:0.362551
# [17]	valid-rmse:0.362320	train-rmse:0.361573
# [18]	valid-rmse:0.361544	train-rmse:0.360605
# [19]	valid-rmse:0.360897	train-rmse:0.359786
# [20]	valid-rmse:0.360336	train-rmse:0.359094
# [21]	valid-rmse:0.359884	train-rmse:0.358520
# [22]	valid-rmse:0.359518	train-rmse:0.358031
# [23]	valid-rmse:0.359215	train-rmse:0.357613
# [24]	valid-rmse:0.358971	train-rmse:0.357272
# [25]	valid-rmse:0.358762	train-rmse:0.356980
# [26]	valid-rmse:0.358579	train-rmse:0.356729
# [27]	valid-rmse:0.358410	train-rmse:0.356504
# [28]	valid-rmse:0.358270	train-rmse:0.356309
# [29]	valid-rmse:0.358156	train-rmse:0.356148
# [30]	valid-rmse:0.358066	train-rmse:0.356011
# [31]	valid-rmse:0.357971	train-rmse:0.355875
# [32]	valid-rmse:0.357896	train-rmse:0.355752
# [33]	valid-rmse:0.357822	train-rmse:0.355638
# [34]	valid-rmse:0.357763	train-rmse:0.355542
# [35]	valid-rmse:0.357712	train-rmse:0.355457
# [36]	valid-rmse:0.357659	train-rmse:0.355360
# [37]	valid-rmse:0.357612	train-rmse:0.355276
# [38]	valid-rmse:0.357570	train-rmse:0.355199
# [39]	valid-rmse:0.357534	train-rmse:0.355133
# [40]	valid-rmse:0.357508	train-rmse:0.355071
# [41]	valid-rmse:0.357483	train-rmse:0.355005
# [42]	valid-rmse:0.357460	train-rmse:0.354941
# [43]	valid-rmse:0.357437	train-rmse:0.354883
# [44]	valid-rmse:0.357412	train-rmse:0.354828
# [45]	valid-rmse:0.357398	train-rmse:0.354780
# [46]	valid-rmse:0.357380	train-rmse:0.354729
# [47]	valid-rmse:0.357369	train-rmse:0.354691
# [48]	valid-rmse:0.357354	train-rmse:0.354639
# [49]	valid-rmse:0.357335	train-rmse:0.354588
# [50]	valid-rmse:0.357320	train-rmse:0.354541
# [51]	valid-rmse:0.357309	train-rmse:0.354508
# [52]	valid-rmse:0.357295	train-rmse:0.354461
# [53]	valid-rmse:0.357277	train-rmse:0.354404
# [54]	valid-rmse:0.357264	train-rmse:0.354351
# [55]	valid-rmse:0.357256	train-rmse:0.354308
# [56]	valid-rmse:0.357247	train-rmse:0.354274
# [57]	valid-rmse:0.357234	train-rmse:0.354231
# [58]	valid-rmse:0.357227	train-rmse:0.354202
# [59]	valid-rmse:0.357220	train-rmse:0.354170
# [60]	valid-rmse:0.357209	train-rmse:0.354136
# [61]	valid-rmse:0.357203	train-rmse:0.354106
# [62]	valid-rmse:0.357196	train-rmse:0.354078
# [63]	valid-rmse:0.357191	train-rmse:0.354048
# [64]	valid-rmse:0.357184	train-rmse:0.354008
# [65]	valid-rmse:0.357182	train-rmse:0.353983
# [66]	valid-rmse:0.357177	train-rmse:0.353957
# [67]	valid-rmse:0.357169	train-rmse:0.353928
# [68]	valid-rmse:0.357165	train-rmse:0.353900
# [69]	valid-rmse:0.357161	train-rmse:0.353863
# [70]	valid-rmse:0.357155	train-rmse:0.353822
# [71]	valid-rmse:0.357151	train-rmse:0.353789
# [72]	valid-rmse:0.357147	train-rmse:0.353769
# [73]	valid-rmse:0.357142	train-rmse:0.353735
# [74]	valid-rmse:0.357140	train-rmse:0.353703
# [75]	valid-rmse:0.357137	train-rmse:0.353681
# [76]	valid-rmse:0.357136	train-rmse:0.353653
# [77]	valid-rmse:0.357131	train-rmse:0.353623
# [78]	valid-rmse:0.357127	train-rmse:0.353597
# [79]	valid-rmse:0.357122	train-rmse:0.353564
# [80]	valid-rmse:0.357121	train-rmse:0.353543
# [81]	valid-rmse:0.357118	train-rmse:0.353517
# [82]	valid-rmse:0.357112	train-rmse:0.353485
# [83]	valid-rmse:0.357109	train-rmse:0.353442
# [84]	valid-rmse:0.357108	train-rmse:0.353424
# [85]	valid-rmse:0.357103	train-rmse:0.353401
# [86]	valid-rmse:0.357097	train-rmse:0.353376
# [87]	valid-rmse:0.357094	train-rmse:0.353361
# [88]	valid-rmse:0.357091	train-rmse:0.353330
# [89]	valid-rmse:0.357087	train-rmse:0.353308
# [90]	valid-rmse:0.357084	train-rmse:0.353283
# [91]	valid-rmse:0.357082	train-rmse:0.353255
# [92]	valid-rmse:0.357079	train-rmse:0.353230
# [93]	valid-rmse:0.357076	train-rmse:0.353210
# [94]	valid-rmse:0.357076	train-rmse:0.353186
# [95]	valid-rmse:0.357072	train-rmse:0.353146
# [96]	valid-rmse:0.357070	train-rmse:0.353123
# [97]	valid-rmse:0.357067	train-rmse:0.353093
# [98]	valid-rmse:0.357066	train-rmse:0.353072
# [99]	valid-rmse:0.357064	train-rmse:0.353040