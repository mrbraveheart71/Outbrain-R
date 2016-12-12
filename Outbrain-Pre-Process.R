library(data.table)
library(Metrics)
library(xgboost)

clicks_train  <- fread( "../Data/clicks_train.csv")
event.col.names=c("display_id","document_id","timestamp","platform","geo_location")
events <- fread("../Data/events.csv",select = event.col.names)
documents_meta <- fread("../Data/documents_meta.csv", select = c("document_id","source_id","publisher_id"))
#documents_entities <- fread("../Data/documents_entities.csv")
documents_categories <- fread("../Data/documents_categories.csv")
documents_topics <- fread("../Data/documents_topics.csv")
promoted_content <- fread("../Data/promoted_content.csv")

if (runType=='Train') {
  display_ids <- unique(clicks_train$display_id)
  downScale <- 0.7
  sample_display_ids <- sample(display_ids,length(display_ids)*downScale)
  train_display_ids <- sample(sample_display_ids,length(sample_display_ids)*0.8)
  valid_display_ids <- setdiff(sample_display_ids,train_display_ids)
  clicks_train <- clicks_train[clicks_train$display_id %in% sample_display_ids]
  events <- events[events$display_id %in% sample_display_ids]
  saveFileName <- 'Outbrain Aggregate Tables Train'
  # do Test, user all display ids
} else {
  train_display_ids <- unique(clicks_train$display_id)
  saveFileName <- 'Outbrain Aggregate Tables Test'
}

# Aggregate columns
display_metrics <- clicks_train[,j=list(ad_count=length(clicked)),by=list(display_id)]
setkeyv(clicks_train,c("display_id"))
clicks_train <- merge( clicks_train, display_metrics, all.x = T )
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
# document category, just take the maximum category
setkeyv(documents_categories,"confidence_level")
max_doc_cat <- documents_categories[,j=list(category_id=last(category_id)),by=list(document_id)]
setkeyv(max_doc_cat,"document_id")
setkeyv(clicks_train,"document_id")
clicks_train <- merge(clicks_train,max_doc_cat,all.x=T)
# document top, just take the maximum topic
setkeyv(documents_categories,"confidence_level")
max_doc_top <- documents_topics[,j=list(topic_id=last(topic_id)),by=list(document_id)]
setkeyv(max_doc_top,"document_id")
setkeyv(clicks_train,"document_id")
clicks_train <- merge(clicks_train,max_doc_top,all.x=T)

rm(events)
rm(documents_meta)
rm(promoted_content)
rm(max_doc_cat)
rm(max_doc_top)
rm(documents_topics)
gc()
# now do advertiser combinations
# setkeyv(clicks_train,c("display_id","advertiser_id"))
# advertiser_comb <- clicks_train[,.(advertiser_comb=paste(rev(advertiser_id),collapse=" ")),by=display_id]
# advertiser_comb$advertiser_id <- clicks_train$advertiser_id[clicks_train$clicked==1]
# setkeyv(advertiser_comb,c("advertiser_comb"))
# advertiser_comb$advertiser_comb_int <- as.integer(as.factor(advertiser_comb$advertiser_comb))
#rm(advertiser_comb)
# gc()
clicks_train[, timestampHour := hour(ISOdatetime(1970,01,01,0,0,0) + 1465876799998/1e3 + timestamp/1e3)]
gc()

click_prob = clicks_train[,.(sum(clicked)/.N)]
click_prob <- as.numeric(click_prob)
# ad_id_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,
#                               j=list(prob=mean(clicked),count=length(clicked)),by=list(ad_id)]
ad_id_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,
                               j=list(prob=(sum(clicked)+click_prob*40)/(length(clicked)+40),count=length(clicked)),by=list(ad_id)]
ad_hours_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(ad_hour_prob=mean(clicked),ad_hour_count=length(clicked)),
                               by=list(ad_id,timestampHour)]
# advertiser_id_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(advertiser_prob=mean(clicked),advertiser_count=length(clicked)),by=list(advertiser_id)]
# campaign_id_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(campaign_prob=mean(clicked),campaign_count=length(clicked)),by=list(campaign_id)]
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

ad_category_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(ad_category_prob=mean(clicked),ad_category_count=length(clicked)),
                                    by=list(ad_id,category_id)]

ad_topic_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(ad_topic_prob=mean(clicked),ad_topic_count=length(clicked)),
                                 by=list(ad_id,topic_id)]

advertiser_source_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(advertiser_source_prob=mean(clicked),advertiser_source_count=length(clicked)),
                                          by=list(advertiser_id,source_id)]

advertiser_publisher_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(advertiser_publisher_prob=mean(clicked),advertiser_publisher_count=length(clicked)),
                                          by=list(advertiser_id,publisher_id)]

advertiser_geo_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(advertiser_geo_prob=mean(clicked),advertiser_geo_count=length(clicked)),
                                             by=list(advertiser_id,geo_location)]

feature.names <- c("prob","count","ad_count","ad_doc_prob","ad_doc_count","ad_source_prob","ad_source_count",
                   "advertiser_source_prob","advertiser_source_count","ad_publisher_prob","ad_publisher_count",
                   "ad_category_prob","ad_category_count","ad_topic_prob","ad_topic_count",
                   "advertiser_publisher_prob","advertiser_publisher_count",
                   "advertiser_geo_prob","advertiser_geo_count",
                   "advertiser_geo_prob","advertiser_geo_count",
                   #"timestampHour","platform",
                   "ad_hour_prob","ad_hour_count","ad_display_prob","ad_display_count")

if (runType=='Train') {
  save("ad_id_metrics","advertiser_geo_metrics","ad_hours_metrics",
       "click_prob","ad_doc_metrics", "ad_source_metrics","advertiser_source_metrics","ad_publisher_metrics",
       "ad_category_metrics","ad_topic_metrics","advertiser_publisher_metrics","feature.names","event.col.names",
       "sample_display_ids","train_display_ids","valid_display_ids",file=saveFileName)
} else {
  save("ad_id_metrics","advertiser_geo_metrics","ad_hours_metrics",
       "click_prob","ad_doc_metrics", "ad_source_metrics","advertiser_source_metrics","ad_publisher_metrics",
       "ad_category_metrics","ad_topic_metrics","advertiser_publisher_metrics","feature.names","event.col.names",
       "train_display_ids",file=saveFileName)
}
# rm("ad_id_metrics","click_prob","ad_doc_metrics","advertiser_publisher_metrics","ad_source_metrics","advertiser_source_metrics","ad_publisher_metrics","ad_category_metrics",
#    "ad_topic_metrics")

