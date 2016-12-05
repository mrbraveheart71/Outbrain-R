library(data.table)
library(Metrics)
library(xgboost)

clicks_train  <- fread( "../Data/clicks_train.csv")
event.col.names=c("display_id","document_id","timestamp","platform","geo_location")
events <- fread("../Data/events.csv",select = event.col.names)
documents_meta <- fread("../Data/documents_meta.csv", select = c("document_id","source_id","publisher_id"))
#documents_meta <- fread("../Data/documents_meta.csv")
promoted_content <- fread("../Data/promoted_content.csv")

# Get Display samples to down-scale data
display_ids <- unique(clicks_train$display_id)
downScale <- 0.7
sample_display_ids <- sample(display_ids,length(display_ids)*downScale)

train_display_ids <- sample(sample_display_ids,length(sample_display_ids)*0.8)
valid_display_ids <- setdiff(sample_display_ids,train_display_ids)
clicks_train <- clicks_train[clicks_train$display_id %in% sample_display_ids]
events <- events[events$display_id %in% sample_display_ids]

# Aggregate columns
setkeyv(clicks_train,"display_id")
setkeyv(events,"display_id")
clicks_train <- merge( clicks_train, events, all.x = T )
setkeyv(documents_meta,"document_id")
setkeyv(clicks_train,"document_id")
clicks_train <- merge(clicks_train, documents_meta, all.x=T)
promoted_content <- promoted_content[,c("ad_id","advertiser_id", "campaign_id"),with=FALSE]
setkeyv(promoted_content,"ad_id")
setkeyv(clicks_train,"ad_id")
clicks_train <- merge(clicks_train,promoted_content,all.x=T)
rm(events)
rm(documents_meta)
rm(promoted_content)

click_prob = clicks_train[,.(sum(clicked)/.N)]
ad_id_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(prob=mean(clicked),count=length(clicked)),by=list(ad_id)]
# ad_geo_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(ad_geo_prob=mean(clicked),ad_geo_count=length(clicked)),
#                                by=list(ad_id,geo_location)]
ad_doc_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(ad_doc_prob=mean(clicked),ad_doc_count=length(clicked)),
                               by=list(ad_id,document_id)]
ad_source_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(ad_source_prob=mean(clicked),ad_source_count=length(clicked)),
                               by=list(ad_id,source_id)]
# campaign_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(campaign_prob=mean(clicked),campaign_count=length(clicked)),
#                                   by=list(campaign_id)]
ad_publisher_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(ad_publisher_prob=mean(clicked),ad_publisher_count=length(clicked)),
                                     by=list(ad_id,publisher_id)]

advertiser_source_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(advertiser_source_prob=mean(clicked),advertiser_source_count=length(clicked)),
                                  by=list(advertiser_id,source_id)]

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

clicks_train$platform <- as.integer(clicks_train$platform)

save("ad_id_metrics","click_prob","ad_doc_metrics", "ad_source_metrics","advertiser_source_metrics","ad_publisher_metrics",file="Outbrain Aggregate Tables")
rm("ad_id_metrics","click_prob","ad_doc_metrics","ad_source_metrics","advertiser_source_metrics","ad_publisher_metrics")
gc()

# do an xgboost
set.seed(100)
strainDisplayIds <- sample(valid_display_ids, length(valid_display_ids)*0.8)
svalidDisplayIds <- setdiff(valid_display_ids, strainDisplayIds)
strain <- which(clicks_train$display_id %in% strainDisplayIds)
svalid <- which(clicks_train$display_id %in% svalidDisplayIds)

xgb_params = list(
  seed = 0,colsample_bytree = 1,colsample_bylevel=1,subsample = 0.9,
  eta = 0.1,max_depth =6,num_parallel_tree = 1,
  min_child_weight = 10,gamma=1, objective='reg:linear'
  #objective="reg:linear"
)

#feature.names <- c("prob","count","ad_geo_prob","ad_geo_count","ad_doc_prob","ad_doc_count","ad_source_prob","ad_source_count")
feature.names <- c("prob","count","ad_doc_prob","ad_doc_count","ad_source_prob","ad_source_count",
                   "advertiser_source_prob","advertiser_source_count","ad_publisher_prob","ad_publisher_count")
dtrain = xgb.DMatrix(as.matrix(clicks_train[strain,feature.names,with=FALSE]), label=clicks_train$clicked[strain], missing=NA)
dvalid = xgb.DMatrix(as.matrix(clicks_train[svalid,feature.names,with=FALSE]), label=clicks_train$clicked[svalid], missing=NA)

watchlist <- list(valid=dvalid,train=dtrain)
xgboost.fit <- xgb.train (data=dtrain,xgb_params,missing=NA,early.stop.round = 5,
                          #eval_metric="auc",
                          nrounds=50,
                          maximize=FALSE,verbose=1,watchlist = watchlist)

# Valid Score of 36.5726
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
# Results was 
#
# Now prepare test
#
rm(clicks_train)
rm(clicks_train_clicked)
rm(train.preds)

clicks_test   <- fread( "../Data/clicks_test.csv", select=c("ad_id","source_id","document_id","display_id"))
events <- fread("../Data/events.csv",select = event.col.names)
documents_meta <- fread("../Data/documents_meta.csv", select = c("document_id","source_id","publisher_id"))
promoted_content <- fread("../Data/promoted_content.csv")

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

rm(events)
rm(documents_meta)

load("Outbrain Aggregate Tables")
load("Outbrain XGBOOST Model")
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

#clicks_test$platform <- as.integer(clicks_test$platform)
dtest = xgb.DMatrix(as.matrix(clicks_test[,feature.names,with=FALSE]),missing=NA)
gc()
clicks_test <- clicks_test[,c("ad_id","display_id"),with=FALSE]
clicks_test$preds <- predict(xgboost.fit,newdata=dtest)

setkey(clicks_test,"preds")
submission <- clicks_test[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
setkey(submission,"display_id")

write.csv(submission,file = "submission.csv",row.names = F)