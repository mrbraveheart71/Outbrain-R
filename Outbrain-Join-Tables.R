library(data.table)
library(Metrics)
library(xgboost)
library(caret)

runType='Train'
#runType='Test'

if (runType=='Train') {
  clicks_train  <- fread( "../Data/clicks_train.csv")
  fileName <- "Outbrain Train No Aggregates"
} else {
  clicks_train  <- fread( "../Data/clicks_test.csv")
  fileName <- "Outbrain Test No Aggregates"
}

event.col.names=c("display_id","document_id","timestamp","platform","geo_location","uuid")
#events <- fread("../Data/events.csv",select = event.col.names)
events <- fread("../Data/events.csv")
documents_meta <- fread("../Data/documents_meta.csv", select = c("document_id","source_id","publisher_id"))
#documents_entities <- fread("../Data/documents_entities.csv")
documents_categories <- fread("../Data/documents_categories.csv")
documents_topics <- fread("../Data/documents_topics.csv")
promoted_content <- fread("../Data/promoted_content.csv")

# Join columns
display_metrics <- clicks_train[,j=list(ad_count=length(ad_id)),by=list(display_id)]
setkeyv(clicks_train,c("display_id"))
clicks_train <- merge( clicks_train, display_metrics, all.x = T )
setkeyv(clicks_train,"display_id")
setkeyv(events,"display_id")
clicks_train <- merge( clicks_train, events, all.x = T )
rm(events)
setkeyv(documents_meta,"document_id")
setkeyv(clicks_train,"document_id")
clicks_train <- merge(clicks_train, documents_meta, all.x=T)
rm(documents_meta)
gc()
promoted_content <- promoted_content[,c("ad_id","advertiser_id", "campaign_id"),with=FALSE]
setkeyv(promoted_content,"ad_id")
setkeyv(clicks_train,"ad_id")
clicks_train <- merge(clicks_train,promoted_content,all.x=T)
rm(promoted_content)
gc()
# document category, just take the maximum category
setkeyv(documents_categories,"confidence_level")
max_doc_cat <- documents_categories[,j=list(category_id=last(category_id)),by=list(document_id)]
rm(documents_categories)
setkeyv(max_doc_cat,"document_id")
setkeyv(clicks_train,"document_id")
clicks_train <- merge(clicks_train,max_doc_cat,all.x=T)
rm(max_doc_cat)
gc()
# document top, just take the maximum topic
# setkeyv(documents_categories,"confidence_level")
# max_doc_top <- documents_topics[,j=list(topic_id=last(topic_id)),by=list(document_id)]
# setkeyv(max_doc_top,"document_id")
# setkeyv(clicks_train,"document_id")
# clicks_train <- merge(clicks_train,max_doc_top,all.x=T)

clicks_train[, timestampDay := mday(ISOdatetime(1969,12,31,19,0,0) + 1465876799998/1e3 + timestamp/1e3)]
gc()

load("Outbrain Page view Data Aggregates")
setkeyv(clicks_train,c("uuid"))
setkeyv(user_publisher,c("uuid"))
clicks_train[,user_publisher_10:=user_publisher[clicks_train[,list(uuid)],list(user_publisher_10)]]
# clicks_train[,user_traffic_source:=user_page_views[clicks_train[,list(uuid)],list(user_traffic_source)]]
# rm(user_page_views)

if (runType=='Train') {
  save("clicks_train",file=fileName)
} else {
  clicks_test <- clicks_train
  save("clicks_test",file=fileName)
}

