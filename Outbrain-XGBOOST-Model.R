library(data.table)
library(Metrics)
library(xgboost)

runType <- "Train"
source("Outbrain-Pre-Process.R")

clicks_train <- clicks_train[clicks_train$display_id %in% valid_display_ids]
setkeyv(clicks_train,"ad_id")
clicks_train <- merge( clicks_train, ad_id_metrics, all.x = T )
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
setkeyv(clicks_train,c("ad_id","topic_id"))
clicks_train <- merge( clicks_train, ad_topic_metrics, all.x = T )

clicks_train$platform <- as.integer(clicks_train$platform)

# Get Display samples to down-scale data
# do an xgboost
set.seed(100)
strainDisplayIds <- sample(valid_display_ids, length(valid_display_ids)*0.8)
svalidDisplayIds <- setdiff(valid_display_ids, strainDisplayIds)
strain <- which(clicks_train$display_id %in% strainDisplayIds)
svalid <- which(clicks_train$display_id %in% svalidDisplayIds)

xgb_params = list(
  seed = 0,colsample_bytree = 1,colsample_bylevel=1,subsample = 0.9,
  eta = 0.1,max_depth =10,num_parallel_tree = 1,
  min_child_weight = 10,objective='reg:linear'
  #objective="reg:linear"
)

dtrain = xgb.DMatrix(as.matrix(clicks_train[strain,feature.names,with=FALSE]), label=clicks_train$clicked[strain], missing=NA)
dvalid = xgb.DMatrix(as.matrix(clicks_train[svalid,feature.names,with=FALSE]), label=clicks_train$clicked[svalid], missing=NA)

watchlist <- list(valid=dvalid,train=dtrain)
xgboost.fit <- xgb.train (data=dtrain,xgb_params,missing=NA,early.stop.round = 5,
                          #eval_metric="auc",
                          nrounds=50,
                          maximize=FALSE,verbose=1,watchlist = watchlist)

# Valid Score of 36.39
save("xgboost.fit",file="Outbrain XGBOOST Model")
importance_matrix <- xgb.importance(model = xgboost.fit, feature.names)
options(scipen=999)
print(importance_matrix,200)

rm(dtrain,dvalid)
gc()
dall = xgb.DMatrix(as.matrix(clicks_train[,feature.names,with=FALSE]), label=clicks_train$clicked, missing=NA)
clicks_train$preds <- predict(xgboost.fit,newdata=dall)

# in sample testing
#
setkey(clicks_train,"preds")
clicks_train <- clicks_train[,c("ad_id","display_id","clicked","preds"),with=FALSE]
train.preds <- clicks_train[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
train.preds <- train.preds[order(train.preds$display_id)]
clicks_train <- clicks_train[order(clicks_train$display_id)]
clicks_train_clicked <- clicks_train[clicks_train$clicked==1]

# now just test validation Displays
s <- sample(valid_display_ids,100000)

predicted <- lapply(train.preds$ad_id[train.preds$display_id %in% s], function(x) as.numeric(tstrsplit(x,split=" ")))
actual <- as.list(clicks_train_clicked$ad_id[clicks_train_clicked$display_id %in% s])
  
map12 <- mapk(12, actual, predicted)
print(map12)
# Results was 67.08

#
# Now prepare test
#
library(data.table)
library(Metrics)
library(xgboost)

load("Outbrain XGBOOST Model")
runType <- "Test"
source("Outbrain-Pre-Process.R")
rm(clicks_train)
gc()
load('Outbrain Aggregate Tables Test')

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

rm(events)
rm(documents_meta)

#load("Outbrain Aggregate Tables")

display_metrics <- clicks_test[,j=list(ad_count=length(ad_id)),by=list(display_id)]
setkeyv(clicks_test,c("display_id"))
clicks_test <- merge( clicks_test, display_metrics, all.x = T )

setkeyv(clicks_test,"ad_id")
clicks_test  <- merge( clicks_test , ad_id_metrics, all.x = T )
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

clicks_test <- clicks_test[,c(feature.names,"ad_id","display_id"),with=FALSE]
#clicks_test$platform <- as.integer(clicks_test$platform)
rm("ad_id_metrics","click_prob","ad_doc_metrics","ad_source_metrics","advertiser_source_metrics","ad_publisher_metrics","ad_category_metrics")

dtest = xgb.DMatrix(as.matrix(clicks_test[,feature.names,with=FALSE]),missing=NA)
gc()
clicks_test <- clicks_test[,c("ad_id","display_id"),with=FALSE]
clicks_test$preds <- predict(xgboost.fit,newdata=dtest)

setkey(clicks_test,"preds")
submission <- clicks_test[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
setkey(submission,"display_id")

write.csv(submission,file = "submission.csv",row.names = F)