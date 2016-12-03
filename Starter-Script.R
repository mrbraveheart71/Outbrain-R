library(data.table)
library(Metrics)
library(xgboost)

super_fread <- function( file , key_var=NULL){
  dt <- fread(file)
  if(!is.null(key_var)) setkeyv(dt,c(key_var))
  return(dt)
}

clicks_train  <- super_fread( "../Data/clicks_train.csv", key_var = "ad_id" )
clicks_test   <- super_fread( "../Data/clicks_test.csv" , key_var = "ad_id" )
events <- fread("../Data/events.csv")

# Get Display samples to down-scale data
display_ids <- unique(clicks_train$display_id)
sample_display_ids <- sample(display_ids,length(display_ids)*0.5)

train_display_ids <- sample(sample_display_ids,length(sample_display_ids)*0.8)
valid_display_ids <- setdiff(sample_display_ids,train_display_ids)
clicks_train <- clicks_train[clicks_train$display_id %in% sample_display_ids]

click_prob = clicks_train[,.(sum(clicked)/.N)]
ad_id_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(prob=mean(clicked),count=length(clicked)),by=list(ad_id)]

#ad_id_probs   <- get_probs(clicks_train[clicks_train$display_id %in% train_display_ids],"ad_id","clicked",8)
#rm(clicks_train)
gc()

clicks_train <- merge( clicks_train, ad_id_metrics, all.x = T )
clicks_test <- merge( clicks_test, ad_id_metrics, all.x = T )

# do an xgboost
set.seed(100)
strainDisplayIds <- sample(valid_display_ids, length(valid_display_ids)*0.8)
svalidDisplayIds <- setdiff(valid_display_ids, strainDisplayIds)
strain <- which(clicks_train$display_id %in% strainDisplayIds)
svalid <- which(clicks_train$display_id %in% svalidDisplayIds)

xgb_params = list(
  seed = 0,colsample_bytree = 1,colsample_bylevel=1,subsample = 0.9,
  eta = 0.1,max_depth =6,num_parallel_tree = 1,
  min_child_weight = 10,gamma=1, objective='reg:logistic'
  #objective="reg:linear"
)

feature.names <- c("prob","count")
dtrain = xgb.DMatrix(as.matrix(clicks_train[strain,feature.names,with=FALSE]), label=clicks_train$clicked[strain], missing=NA)
dvalid = xgb.DMatrix(as.matrix(clicks_train[svalid,feature.names,with=FALSE]), label=clicks_train$clicked[svalid], missing=NA)
dall = xgb.DMatrix(as.matrix(clicks_train[,feature.names,with=FALSE]), label=clicks_train$clicked, missing=NA)
dtest = xgb.DMatrix(as.matrix(clicks_test[,feature.names,with=FALSE]),missing=NA)

watchlist <- list(valid=dvalid,train=dtrain)
xgboost.fit <- xgb.train (data=dtrain,xgb_params,missing=NA,early.stop.round = 5,eval_metric="auc",nrounds=100,
                          maximize=TRUE,verbose=1,watchlist = watchlist)

importance_matrix <- xgb.importance(model = xgboost.fit, feature.names)
options(scipen=999)
print(importance_matrix,200)

clicks_train$preds <- predict(xgboost.fit,newdata=dall)
clicks_test$preds <- predict(xgboost.fit,newdata=dtest)

#DT_fill_NA( clicks_test, click_prob )

# in sample testing
setkey(clicks_train,"preds")

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

# Now prepare test
setkey(clicks_test,"preds")
submission <- clicks_test[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
setkey(submission,"display_id")

write.csv(submission,file = "submission.csv",row.names = F)