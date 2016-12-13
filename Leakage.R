library(data.table)
library(Metrics)
library(xgboost)

clicks_train  <- fread( "../Data/clicks_train.csv")
events <- fread("../Data/events.csv")
documents_meta <- fread("../Data/documents_meta.csv", select = c("document_id","source_id","publisher_id"))
#documents_entities <- fread("../Data/documents_entities.csv")
documents_categories <- fread("../Data/documents_categories.csv")
documents_topics <- fread("../Data/documents_topics.csv")
promoted_content <- fread("../Data/promoted_content.csv")

