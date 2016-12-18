library(data.table)
library(Metrics)
library(xgboost)
library(LaF)
setwd("C:/R-Studio/Outbrain/Outbrain R")

clicks_test  <- fread( "../Data/clicks_test.csv")
#clicks_test <- clicks_test[clicks_test$clicked==1,]
events <- fread("../Data/events.csv",select=c("display_id","uuid","timestamp"))
promoted_content <- fread("../Data/promoted_content.csv", select=c("ad_id","document_id"))

setkeyv(clicks_test,c("ad_id"))
clicks_test <- merge( clicks_test, promoted_content, all.x = T )
setkeyv(clicks_test,c("display_id"))
clicks_test <- merge( clicks_test, events, all.x = T )
clicks_test[, usr_doc := paste0(uuid,"_",document_id)]

rm(events)
rm(promoted_content)

#
leaks <- as.data.table(NULL)
model <- detect_dm_csv("../Data/page_views.csv", sep=",", header=TRUE)
df.laf <- laf_open(model)
no.rows <- 1
total.rows <- 0
while (no.rows > 0 ) {
  page_views <- as.data.table(next_block(df.laf,nrows=5e6))
  page_views[,usr_doc := paste0(uuid,"_",document_id)]
  #leaks <- rbind(leaks,clicks_test[clicks_test$usr_doc %in% page_views$usr_doc,c("display_id","ad_id","clicked"),with=FALSE])
  leaks <- rbind(leaks,clicks_test[clicks_test$usr_doc %in% page_views$usr_doc,c("display_id","ad_id"),with=FALSE])
  no.rows <- nrow(page_views)
  total.rows <- total.rows + nrow(page_views)
  print(paste0("We processed ",total.rows," of rows in page_Views to find the leaks, and we found ", nrow(leaks)," leaks"))
}

save("leaks",file="Outbrain Data Leakage")
