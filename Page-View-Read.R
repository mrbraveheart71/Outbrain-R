library(data.table)
library(Metrics)
library(xgboost)
library(LaF)
setwd("C:/R-Studio/Outbrain/Outbrain R")

#clicks_test  <- fread( "../Data/clicks_test.csv")
#clicks_test <- clicks_test[clicks_test$clicked==1,]
events <- fread("../Data/events.csv",select=c("display_id","uuid","timestamp"))
#promoted_content <- fread("../Data/promoted_content.csv", select=c("ad_id","document_id"))
unique.user.id <- unique(events$uuid)
rm(events) 
# setkeyv(clicks_test,c("ad_id"))
# clicks_test <- merge( clicks_test, promoted_content, all.x = T )
# setkeyv(clicks_test,c("display_id"))
# clicks_test <- merge( clicks_test, events, all.x = T )
# clicks_test[, usr_doc := paste0(uuid,"_",document_id)]
# 
# rm(events)
# rm(promoted_content)
# 
#
page_views_user <- as.data.table(NULL)
model <- detect_dm_csv("../Data/page_views.csv", sep=",", header=TRUE)
vars <- c("uuid","document_id","platform","traffic_source")

df.laf <- laf_open(model)
no.rows <- 1
total.rows <- 0
total.sel.rows <- 0
while (no.rows > 0 ) {
  page_views <- as.data.table(next_block(df.laf,nrows=5e6,columns=c(1,2,4,6)))
  no.rows <- nrow(page_views)
  total.rows <- total.rows + nrow(page_views)
  page_views <- page_views[page_views$uuid %in% unique.user.id]
  #leaks <- rbind(leaks,clicks_test[clicks_test$usr_doc %in% page_views$usr_doc,c("display_id","ad_id","clicked"),with=FALSE])
  page_views_user <- rbind(page_views_user,page_views)
  total.sel.rows <- nrow(page_views_user)
  print(paste0("We processed ",total.rows," of rows in page_Views and we saved ", nrow(page_views_user)," rows"))
  print(paste0("object size is ",formatC(object.size(page_views_user),big.mark=",")))
}

save("page_views_user",file="Outbrain Page View Data of clicks user")

# Now do some aggregates
load("Outbrain Page View Data of clicks user")

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

setkeyv(page_views_user,c("uuid","document_id"))
user_page_views <- page_views_user[,j=list(user_count=length(document_id),
                                                  user_traffic_source=Mode(traffic_source)),
                                     by=list(uuid)]
save("user_page_views", file="Outbrain Page view Data Aggregates")
