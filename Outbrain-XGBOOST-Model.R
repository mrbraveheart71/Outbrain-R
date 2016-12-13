library(data.table)
library(Metrics)
library(xgboost)

setwd("C:/R-Studio/Outbrain/Outbrain R")
runType <- "Train"
source("Outbrain-Pre-Process.R",echo=TRUE)

clicks_train <- clicks_train[clicks_train$display_id %in% valid_display_ids]
setkeyv(clicks_train,"ad_id")
clicks_train <- merge( clicks_train, ad_id_metrics, all.x = T )

display_ad_metrics <- clicks_train[,j=list(ad_display_prob=mean(prob),ad_display_count=sum(count)),
                              by=list(display_id)]
setkeyv(clicks_train,"display_id")
clicks_train <- merge( clicks_train, display_ad_metrics, all.x = T )
# setkeyv(clicks_train,c("ad_id","geo_location"))
# clicks_train <- merge( clicks_train, ad_geo_metrics, all.x = T )
setkeyv(clicks_train,c("ad_id","document_id"))
clicks_train <- merge( clicks_train, ad_doc_metrics, all.x = T )
setkeyv(clicks_train,c("ad_id","source_id"))
clicks_train <- merge( clicks_train, ad_source_metrics, all.x = T )
setkeyv(clicks_train,c("advertiser_id","source_id"))
clicks_train <- merge( clicks_train, advertiser_source_metrics, all.x = T )
setkeyv(clicks_train,c("ad_id","publisher_id"))
clicks_train <- merge( clicks_train, ad_publisher_metrics, all.x = T )
setkeyv(clicks_train,c("ad_id","category_id"))
clicks_train <- merge( clicks_train, ad_category_metrics, all.x = T )
setkeyv(clicks_train,c("ad_id","publisher_id","source_id"))
clicks_train <- merge( clicks_train, ad_publisher_source_prob, all.x = T )
setkeyv(clicks_train,c("advertiser_id","publisher_id"))
clicks_train <- merge( clicks_train, advertiser_publisher_metrics, all.x = T )
setkeyv(clicks_train,c("advertiser_id","geo_location"))
clicks_train <- merge( clicks_train, advertiser_geo_metrics, all.x = T )
setkeyv(clicks_train,c("ad_id","timestampHour"))
clicks_train <- merge( clicks_train, ad_hours_metrics, all.x = T )
# setkeyv(clicks_train,c("advertiser_id"))
# clicks_train <- merge( clicks_train, advertiser_id_metrics, all.x = T )
# setkeyv(clicks_train,c("campaign_id"))
# clicks_train <- merge( clicks_train, campaign_id_metrics, all.x = T )

#clicks_train$platform <- as.integer(clicks_train$platform)

# Get Display samples to down-scale data
# do an xgboost
set.seed(100)
strainDisplayIds <- sample(valid_display_ids, length(valid_display_ids)*0.8)
svalidDisplayIds <- setdiff(valid_display_ids, strainDisplayIds)
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
dtrain = xgb.DMatrix(as.matrix(clicks_train[strain,feature.names,with=FALSE]), label=clicks_train$clicked[strain], missing=NA)
dvalid = xgb.DMatrix(as.matrix(clicks_train[svalid,feature.names,with=FALSE]), label=clicks_train$clicked[svalid], missing=NA)

watchlist <- list(valid=dvalid,train=dtrain)
xgboost.fit <- xgb.train (data=dtrain,xgb_params,missing=NA,early.stop.round = 5,
                          #eval_metric="auc",
                          #eval_metric="logloss",
                          nrounds=50,
                          maximize=FALSE,verbose=1,watchlist = watchlist)

# Valid Score of 35.78
save("xgboost.fit",file="Outbrain XGBOOST Model")
importance_matrix <- xgb.importance(model = xgboost.fit, feature.names)
options(scipen=999)
print(importance_matrix,200)

rm(dtrain,dvalid)
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
#s <- sample(svalidDisplayIds,100000)
s <- sample(valid_display_ids,100000)

predicted <- lapply(train.preds$ad_id[train.preds$display_id %in% s], function(x) as.numeric(tstrsplit(x,split=" ")))
actual <- as.list(clicks_train_clicked$ad_id[clicks_train_clicked$display_id %in% s])
  
map12 <- mapk(12, actual, predicted)
print(map12)
# Results was 66.5

#
# Now prepare test
#
library(data.table)
library(Metrics)
library(xgboost)

setwd("C:/R-Studio/Outbrain/Outbrain R")
runType <- "Test"
#source("Outbrain-Pre-Process.R")
#rm(clicks_train)
gc()
load('Outbrain Aggregate Tables Test')
load("Outbrain XGBOOST Model")

clicks_test   <- fread( "../Data/clicks_test.csv", select=c("ad_id","source_id","document_id","display_id"))
events <- fread("../Data/events.csv",select = event.col.names)
documents_meta <- fread("../Data/documents_meta.csv", select = c("document_id","source_id","publisher_id"))
promoted_content <- fread("../Data/promoted_content.csv")
documents_categories <- fread("../Data/documents_categories.csv")
documents_topics <- fread("../Data/documents_topics.csv")

setkeyv(events,"display_id")
setkeyv(clicks_test,"display_id")
clicks_test <- merge(clicks_test, events, all.x=T)
setkeyv(documents_meta,"document_id")
setkeyv(clicks_test,"document_id")
clicks_test <- merge(clicks_test, documents_meta, all.x=T)
promoted_content <- promoted_content[,c("ad_id","advertiser_id", "campaign_id"),with=FALSE]
setkeyv(promoted_content,"ad_id")
setkeyv(clicks_test,"ad_id")
clicks_test <- merge(clicks_test,promoted_content,all.x=T)

setkeyv(documents_categories,"confidence_level")
max_doc_cat <- documents_categories[,j=list(category_id=last(category_id)),by=list(document_id)]
setkeyv(max_doc_cat,"document_id")
setkeyv(clicks_test,"document_id")
clicks_test <- merge(clicks_test,max_doc_cat,all.x=T)
# document top, just take the maximum topic
setkeyv(documents_categories,"confidence_level")
max_doc_top <- documents_topics[,j=list(topic_id=last(topic_id)),by=list(document_id)]
setkeyv(max_doc_top,"document_id")
setkeyv(clicks_test,"document_id")
clicks_test <- merge(clicks_test,max_doc_top,all.x=T)

rm("events","documents_meta","promoted_content","documents_categories","documents_topics")
gc()

clicks_test[, timestampHour := hour(ISOdatetime(1970,01,01,0,0,0) + 1465876799998/1e3 + timestamp/1e3)]

setkeyv(clicks_test,"ad_id")
clicks_test  <- merge( clicks_test , ad_id_metrics, all.x = T )
display_metrics <- clicks_test[,j=list(ad_count=length(ad_id)),by=list(display_id)]
setkeyv(clicks_test,c("display_id"))
clicks_test <- merge( clicks_test, display_metrics, all.x = T )

display_ad_metrics <- clicks_test[,j=list(ad_display_prob=mean(prob),ad_display_count=sum(count)),
                                   by=list(display_id)]
setkeyv(clicks_test,"display_id")
clicks_test <- merge( clicks_test, display_ad_metrics, all.x = T )

# setkeyv(clicks_test,c("ad_id","geo_location"))
# clicks_test  <- merge( clicks_test , ad_geo_metrics, all.x = T )
setkeyv(clicks_test,c("ad_id","document_id"))
clicks_test  <- merge( clicks_test , ad_doc_metrics, all.x = T )
setkeyv(clicks_test,c("ad_id","source_id"))
clicks_test <- merge(clicks_test, ad_source_metrics, all.x = T )

setkeyv(clicks_test,c("advertiser_id","source_id"))
clicks_test <- merge( clicks_test, advertiser_source_metrics, all.x = T )
setkeyv(clicks_test,c("ad_id","publisher_id"))
clicks_test <- merge( clicks_test, ad_publisher_metrics, all.x = T )
setkeyv(clicks_test,c("ad_id","category_id"))
clicks_test <- merge( clicks_test, ad_category_metrics, all.x = T )
setkeyv(clicks_test,c("ad_id","topic_id"))
clicks_test <- merge( clicks_test, ad_topic_metrics, all.x = T )

setkeyv(clicks_test,c("advertiser_id","publisher_id"))
clicks_test <- merge( clicks_test, advertiser_publisher_metrics, all.x = T )
setkeyv(clicks_test,c("advertiser_id","geo_location"))
clicks_test <- merge( clicks_test, advertiser_geo_metrics, all.x = T )
setkeyv(clicks_test,c("ad_id","timestampHour"))
clicks_test <- merge( clicks_test, ad_hours_metrics, all.x = T )

clicks_test <- clicks_test[,c(feature.names,"ad_id","display_id"),with=FALSE]
#clicks_test$platform <- as.integer(clicks_test$platform)
rm("ad_id_metrics","click_prob","ad_doc_metrics","ad_source_metrics","advertiser_source_metrics","ad_publisher_metrics","ad_category_metrics",
   "ad_topic_metrics")
gc()
save("clicks_test","feature.names",file="Outbrain Test File")


# Now prepare submission
library(data.table)
library(Metrics)
library(xgboost)

setwd("C:/R-Studio/Outbrain/Outbrain R")
load("Outbrain Test File")
load("Outbrain XGBOOST Model")
# Break it into two pieces
half <- as.integer(nrow(clicks_test)/2)
full <- as.integer(nrow(clicks_test))
dtest = xgb.DMatrix(as.matrix(clicks_test[1:half,feature.names,with=FALSE]),missing=NA)
gc()
clicks_test[1:half, preds := predict(xgboost.fit,newdata=dtest)]
rm(dtest)
gc()
dtest = xgb.DMatrix(as.matrix(clicks_test[(half+1):full,feature.names,with=FALSE]),missing=NA)
clicks_test[(half+1):full, preds := predict(xgboost.fit,newdata=dtest)]
rm(dtest)
gc()
#clicks_test <- clicks_test[,c("ad_id","display_id","preds"),with=FALSE]
#clicks_test$preds <- predict(xgboost.fit,newdata=dtest)

setkey(clicks_test,"preds")
submission <- clicks_test[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
setkey(submission,"display_id")

write.csv(submission,file = "submission.csv",row.names = F)


# 1:          ad_publisher_prob 0.4275698983 0.024007723 0.02561221
# 2:     advertiser_source_prob 0.1636324116 0.027049428 0.03161242
# 3:            ad_display_prob 0.0735522401 0.229355911 0.15736245
# 4:             ad_source_prob 0.0528760031 0.030723863 0.02514576
# 5:                   ad_count 0.0511424293 0.110287205 0.04808650
# 6:                ad_doc_prob 0.0459264892 0.077174699 0.03877876
# 7:  advertiser_publisher_prob 0.0367376890 0.032974111 0.03460193
# 8:           ad_category_prob 0.0333939488 0.024185786 0.03324499
# 9:                       prob 0.0253212370 0.058768867 0.06487862
# 10:               ad_hour_prob 0.0135797341 0.030727659 0.04891339
# 11:           ad_display_count 0.0109158259 0.064097648 0.08200997
# 12:        advertiser_geo_prob 0.0106742642 0.045744795 0.05930245
# 13:               ad_doc_count 0.0093811166 0.028137957 0.02559101
# 14:                      count 0.0089232915 0.048099174 0.05599491
# 15:              ad_topic_prob 0.0066246507 0.012415467 0.02548500
# 16:         ad_publisher_count 0.0050570165 0.018797682 0.02296194
# 17:            ad_source_count 0.0049571919 0.024371096 0.02705396
# 18:          ad_category_count 0.0035046096 0.017589095 0.02936500
# 19:    advertiser_source_count 0.0033626870 0.017716949 0.02815647
# 20: advertiser_publisher_count 0.0029227106 0.020490242 0.02724478
# 21:              ad_hour_count 0.0028624455 0.015696501 0.02913177
# 22:       advertiser_geo_count 0.0024940935 0.012426703 0.03585286
# 23:             ad_topic_count 0.0018996378 0.004779574 0.01961200
# 24:                   platform 0.0017948398 0.021689260 0.01043146
# 25:              timestampHour 0.0008935386 0.002692605 0.01356938
# Feature         Gain       Cover  Frequence