library(data.table)
library(Metrics)
library(xgboost)
library(caret)

setwd("C:/R-Studio/Outbrain/Outbrain R")
load("Outbrain Train No Aggregates")

runType = 'Train'

keep.cols <- c("ad_id","source_id","publisher_id","document_id","display_id","clicked","ad_count")

if (runType=='Train') {
  display_ids <- unique(clicks_train$display_id)
  #downScale <- 1
  #sample_display_ids <- sample(display_ids,length(display_ids)*downScale)
  display_ids_24 <- unique(clicks_train$display_id[clicks_train$timestampDay<=24])
  display_ids_25_26 <- setdiff(display_ids,display_ids_24)
  train_display_ids <- sample(display_ids_24,length(display_ids_24)*0.8)
  valid_display_ids <- setdiff(display_ids,train_display_ids)
} else {
  train_display_ids <- unique(clicks_train$display_id)
}

clicks_train <- clicks_train[,keep.cols,with=FALSE]
clicks_train_raw <- copy(clicks_train)

cluster_publisher <- clicks_train_raw[,j=list(count=length(unique(display_id))),
                                               by=list(publisher_id)]
cluster_publisher <- cluster_publisher[order(cluster_publisher$count,decreasing=FALSE)]

total <- 0
clusterMin <- 10000
cluster <- 1
for (i in 1:nrow(cluster_publisher)) {
  cluster_publisher$cluster[i] <- cluster
  total <- total + cluster_publisher$count[i]
  if (total > clusterMin) {
    cluster <- cluster + 1
    total <- 0
  }
}

results <- data.frame(clusterName=integer(cluster),
                      displayCount= integer(cluster),map12=double(cluster)) 

# Loop through all clusters
for (i in 1:max(cluster_publisher$cluster)) {
  display_ids <- cluster_publisher$publisher_id[cluster_publisher$cluster==1]
  
  # Do another sample
  idx_train <- intersect(unique(clicks_train_raw$display_id[clicks_train_raw$publisher_id %in% display_ids]),train_display_ids)
  train_publisher_ids <- sample(idx_train,4000)
  #train_publisher_ids <- idx
  idx_valid <- intersect(unique(clicks_train_raw$display_id[clicks_train_raw$publisher_id %in% display_ids]),valid_display_ids)
  valid_publisher_ids <- sample(idx_valid,1000)
  #valid_publisher_ids <- idx
  clicks_train <- clicks_train_raw[clicks_train_raw$display_id %in% c(train_publisher_ids,valid_publisher_ids), keep.cols,with=FALSE]
  
  # format in the FFM format and save
  no_doc_ids <- length(unique(clicks_train$document_id))
  table_doc_ids <- table(clicks_train$document_id)
  doc_ids_omitted <- names(table_doc_ids[table_doc_ids<=20])
  
  no_ad_ids <- length(unique(clicks_train$ad_id))
  table_ad_ids <- table(clicks_train$ad_id)
  ad_ids_omitted <- names(table_ad_ids[table_ad_ids<=20])
  
  clicks_train$document_id <- ifelse(clicks_train$document_id %in% doc_ids_omitted,max(clicks_train$document_id)+1,clicks_train$document_id)
  clicks_train$ad_id <- ifelse(clicks_train$ad_id %in% ad_ids_omitted,max(clicks_train$ad_id)+1,clicks_train$ad_id)
  
  clicks_train$document_id_trans <- as.integer(as.factor(clicks_train$document_id))
  clicks_train$ad_id_trans <- as.integer(as.factor(clicks_train$ad_id))
  
  no_doc_ids <- length(unique(clicks_train$document_id_trans))
  no_ad_ids <- length(unique(clicks_train$ad_id_trans))
  
  noRows <- nrow(clicks_train)
  file.output <- data.frame(label=integer(noRows),
                            doc=character(noRows),
                            ad= character(noRows),
                            ad_count=character(noRows),stringsAsFactors = FALSE)
  
  for (i in 1:nrow(clicks_train)) {
    #print(i)
    file.output$label[i] <- clicks_train$clicked[i]
    doc.seq.before <- ifelse(clicks_train$document_id_trans[i]>1,paste0('1:',seq(1,clicks_train$document_id_trans[i]-1,1),':0',collapse=" "),'')
    doc.seq.after <- ifelse(no_doc_ids>=clicks_train$document_id_trans[i]+1,paste0('1:',seq(clicks_train$document_id_trans[i]+1,no_doc_ids,1),':0',collapse=" "),'')
    file.output$doc[i] <- paste0(
      doc.seq.before,' ',paste0('1:',clicks_train$document_id_trans[i],':1',collapse=" "),' ',
      doc.seq.after)
    ad.seq.before <- ifelse(clicks_train$ad_id_trans[i]>1,paste0('2:',seq(no_doc_ids+1,no_doc_ids+clicks_train$ad_id_trans[i]-1,1),':0',collapse=" "),'')
    ad.seq.after <- ifelse(no_ad_ids>=clicks_train$ad_id_trans[i]+1,paste0('2:',seq(no_doc_ids+1+clicks_train$ad_id_trans[i],no_doc_ids+no_ad_ids,1),':0',collapse=" "),'')
    file.output$ad[i] <- paste0(
      ad.seq.before,' ',paste0('2:',no_doc_ids+clicks_train$ad_id_trans[i],':1',collapse=" "),' ',ad.seq.after)
    offset <- no_doc_ids + no_ad_ids
    # ad_count.seq.before <- ifelse(clicks_train$ad_count[i]>1,paste0('3:',seq(offset+1,offset+clicks_train$ad_count[i]-1,1),':0',collapse=" "),'')
    # ad_count.seq.after <-ifelse(12>=clicks_train$ad_count[i]+1,paste0('3:',seq(offset+1+clicks_train$ad_count[i],offset+12,1),':0',collapse=" "),'')
    # file.output$ad_count[i] <- paste0(ad_count.seq.before,' ',paste0('3:',offset+clicks_train$ad_count[i],':1',collapse=" "),' ',ad_count.seq.after)
    file.output$ad_count[i] <- paste0('3:',offset+1,':',clicks_train$ad_count[i])
  }
  
  # displayIds <- unique(clicks_train$display_id)
  # trainDisplayIds <- sample(displayIds,0.8*length(displayIds))
  # validDisplayIds <- setdiff(displayIds,trainDisplayIds)
  
  setwd("C:/R-Studio/Outbrain/FFM")
  
  write.table(file.output[clicks_train$display_id %in% train_publisher_ids,],file="outbrain-data-train.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
  write.table(file.output[clicks_train$display_id %in% valid_publisher_ids,],file="outbrain-data-val.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
  write.table(file.output,file="outbrain-data-all.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
  
  #ffm-train -p va.ffm -l 0.00002 tr.ffm
  system("fm-train.exe -p outbrain-data-val.txt -t 10 -r 0.1 outbrain-data-train.txt model")
  #ffm-predict test_file model_file output_file
  system("fm-predict.exe outbrain-data-all.txt model outbrain-data-output.txt")
  
  #file.output$preds <- read.table("outbrain-data-output.txt")$V1
  clicks_train$preds <- read.table("outbrain-data-output.txt")$V1
  
  getMap12 <- function(clicks_train,valid_publisher_ids) {
    clicks_train <- copy(clicks_train)
    setkeyv(clicks_train,"preds")
    train.preds <- clicks_train[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
    train.preds <- train.preds[order(train.preds$display_id)]
    clicks_train <- clicks_train[order(clicks_train$display_id)]
    clicks_train_clicked <- clicks_train[clicks_train$clicked==1]
    
    s <- valid_publisher_ids
    predicted <- lapply(train.preds$ad_id[train.preds$display_id %in% s], function(x) as.numeric(tstrsplit(x,split=" ")))
    actual <- as.list(clicks_train_clicked$ad_id[clicks_train_clicked$display_id %in% s])
    map12 <- mapk(12, actual, predicted)
    map12
  }
  
  results$clusterName[i] <- i
  results$displayCount[i] <- sum(cluster_publisher$count[cluster_publisher$cluster==i])
  results$map12[i] <- getMap12(clicks_train,valid_publisher_ids)
  
  print(paste0("Current map was : ",results$map12[i]))
  map12.avg <- sum(results[1:i,"displayCount"]*results[1:i,"map12"])/sum(results[1:i,"displayCount"])
  print(paste0("We processed publisher no ",i," and now have a total map12 of ",map12.avg))
  
  print(getMap12(clicks_train,valid_publisher_ids))
  
} # loop through all clusters

