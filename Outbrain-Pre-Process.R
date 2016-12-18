library(data.table)
library(Metrics)
library(xgboost)
library(caret)


clicks_train  <- fread( "../Data/clicks_train.csv")
event.col.names=c("display_id","document_id","timestamp","platform","geo_location","uuid")
#events <- fread("../Data/events.csv",select = event.col.names)
events <- fread("../Data/events.csv")
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
                              j=list(prob=(sum(clicked)+click_prob*20)/(length(clicked)+20),count=length(clicked)),by=list(ad_id)]
# ad_hours_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(ad_hour_prob=mean(clicked),ad_hour_count=length(clicked)),
#                                by=list(ad_id,timestampHour)]
ad_platform_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(ad_platform_prob=mean(clicked),ad_platform_count=length(clicked)),
                                    by=list(ad_id,platform)]

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
ad_publisher_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,
                                     j=list(ad_publisher_prob=(sum(clicked)+click_prob*20)/(length(clicked)+20),ad_publisher_count=length(clicked)),
                                     by=list(ad_id,publisher_id)]

ad_category_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(ad_category_prob=mean(clicked),ad_category_count=length(clicked)),
                                    by=list(ad_id,category_id)]

# ad_topic_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(ad_topic_prob=mean(clicked),ad_topic_count=length(clicked)),
#                                  by=list(ad_id,topic_id)]

advertiser_source_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(advertiser_source_prob=mean(clicked),advertiser_source_count=length(clicked)),
                                          by=list(advertiser_id,source_id)]

advertiser_publisher_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(advertiser_publisher_prob=mean(clicked),advertiser_publisher_count=length(clicked)),
                                             by=list(advertiser_id,publisher_id)]

# advertiser_geo_metrics <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(advertiser_geo_prob=mean(clicked),advertiser_geo_count=length(clicked)),
#                                        by=list(advertiser_id,geo_location)]

# ad_publisher_source_prob <- clicks_train[clicks_train$display_id %in% train_display_ids,j=list(ad_pub_source_prob=mean(clicked),ad_pub_source_count=length(clicked)),
#                                          by=list(ad_id,publisher_id,source_id)]

# rm("ad_id_metrics","click_prob","ad_doc_metrics","advertiser_publisher_metrics","ad_source_metrics","advertiser_source_metrics","ad_publisher_metrics","ad_category_metrics",
#    "ad_topic_metrics")

clicks_train <- clicks_train[clicks_train$display_id %in% valid_display_ids]
setkeyv(clicks_train,"ad_id")
clicks_train <- merge( clicks_train, ad_id_metrics, all.x = T )
setkeyv(clicks_train,c("ad_id","publisher_id"))
clicks_train <- merge( clicks_train, ad_publisher_metrics, all.x = T )

setkeyv(clicks_train,c("display_id","ad_id"))
clicks_train[,ad_display_first:=prob[order(prob, decreasing=TRUE)][1],by=display_id]
clicks_train[,ad_display_second:=prob[order(prob, decreasing=TRUE)][2],by=display_id]
clicks_train[,ad_display_third:=prob[order(prob, decreasing=TRUE)][3],by=display_id]

clicks_train[,ad_publisher_display_first:=ad_publisher_prob[order(ad_publisher_prob, decreasing=TRUE)][1],by=display_id]
clicks_train[,ad_publisher_display_second:=ad_publisher_prob[order(ad_publisher_prob, decreasing=TRUE)][2],by=display_id]
clicks_train[,ad_publisher_display_third:=ad_publisher_prob[order(ad_publisher_prob, decreasing=TRUE)][3],by=display_id]

# clicks_train[,ad_display_max:=max(prob),by=display_id]
# clicks_train[,ad_publisher_display_ratio_to_max:=ad_publisher_prob/max(ad_publisher_prob),by=display_id]
# clicks_train[,ad_publisher_display_max:=max(ad_publisher_prob),by=display_id]
#clicks_train[,ad_publisher_display_order:=rank(-ad_publisher_prob,ties.method="first"),by=display_id]

# display_ad_metrics <- clicks_train[,j=list(ad_id,ad_display_order=rank(-prob,ties.method="first")),
#                                    by=list(display_id)]
# display_ad_publisher_metrics <- clicks_train[,j=list(ad_id,ad_publisher_display_order=rank(-ad_publisher_prob,ties.method="first")),
#                                    by=list(display_id)]


# setkeyv(clicks_train,c("display_id","ad_id"))
# clicks_train <- merge( clicks_train, display_ad_metrics, all.x = T )
# 
# setkeyv(clicks_train,c("display_id","ad_id"))
# clicks_train <- merge( clicks_train, display_ad_publisher_metrics, all.x = T )

setkeyv(clicks_train,c("ad_id","platform"))
setkeyv(ad_platform_metrics,c("ad_id","platform"))
clicks_train <- merge( clicks_train, ad_platform_metrics, all.x = T )
# setkeyv(clicks_train,c("ad_id","geo_location"))
# clicks_train <- merge( clicks_train, ad_geo_metrics, all.x = T )
setkeyv(clicks_train,c("ad_id","document_id"))
clicks_train <- merge( clicks_train, ad_doc_metrics, all.x = T )
setkeyv(clicks_train,c("ad_id","source_id"))
clicks_train <- merge( clicks_train, ad_source_metrics, all.x = T )
setkeyv(clicks_train,c("advertiser_id","source_id"))
clicks_train <- merge( clicks_train, advertiser_source_metrics, all.x = T )
setkeyv(clicks_train,c("ad_id","category_id"))
clicks_train <- merge( clicks_train, ad_category_metrics, all.x = T )
# setkeyv(clicks_train,c("ad_id","publisher_id","source_id"))
# clicks_train <- merge( clicks_train, ad_publisher_source_prob, all.x = T )
setkeyv(clicks_train,c("advertiser_id","publisher_id"))
clicks_train <- merge( clicks_train, advertiser_publisher_metrics, all.x = T )
# setkeyv(clicks_train,c("advertiser_id","geo_location"))
# clicks_train <- merge( clicks_train, advertiser_geo_metrics, all.x = T )
# setkeyv(clicks_train,c("ad_id","timestampHour"))
# clicks_train <- merge( clicks_train, ad_hours_metrics, all.x = T )

feature.names <- c("prob","count","ad_count","ad_doc_prob","ad_doc_count","ad_source_prob","ad_source_count",
                   "advertiser_source_prob","advertiser_source_count","ad_publisher_prob","ad_publisher_count",
                   "ad_category_prob","ad_category_count",
                   "ad_platform_prob","ad_platform_count",
                   #"ad_pub_source_prob","ad_pub_source_count",
                   #"ad_topic_prob","ad_topic_count",
                   #"ad_display_ratio_to_max","ad_publisher_display_ratio_to_max",
                   #"ad_display_max","ad_publisher_display_max",
                   "ad_display_first","ad_display_second","ad_display_third",
                   "ad_publisher_display_first","ad_publisher_display_second","ad_publisher_display_third",
                   "advertiser_publisher_prob","advertiser_publisher_count")
                   #"advertiser_geo_prob","advertiser_geo_count",
                   #"advertiser_geo_prob","advertiser_geo_count")
#"timestampHour","platform",
#"ad_hour_prob","ad_hour_count")
#"ad_display_prob","ad_display_count")

agg.tables.train <- c("ad_id_metrics","advertiser_geo_metrics",
                #"ad_hours_metrics",
                "ad_platform_metrics",
                "click_prob","ad_doc_metrics", "ad_source_metrics","advertiser_source_metrics","ad_publisher_metrics",
                "ad_category_metrics", 
                #"display_ad_publisher_metrics","display_ad_metrics",
                #"ad_publisher_source_prob",
                #"ad_topic_metrics",
                "advertiser_publisher_metrics","feature.names","event.col.names",
                "sample_display_ids","train_display_ids","valid_display_ids")
agg.tables.test <- setdiff(agg.tables.train,c("sample_display_ids","valid_display_ids"))

if (runType=='Train') {
  save(list=agg.tables.train,file=saveFileName)
} else {
  save(list=agg.tables.test,file=saveFileName)
}

# setkeyv(clicks_train,c("advertiser_id"))
# clicks_train <- merge( clicks_train, advertiser_id_metrics, all.x = T )
# setkeyv(clicks_train,c("campaign_id"))
# clicks_train <- merge( clicks_train, campaign_id_metrics, all.x = T )

#clicks_train$platform <- as.integer(clicks_train$platform)
