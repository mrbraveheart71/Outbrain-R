
library(data.table)

# take care , cant be variables with the same name as var or target in dt... 
#if you have a beter implamentation of this functions,share it pls ^^
get_probs <- function (dt,var,target,w){
  p=dt[,sum(get(target))/.N]
  dt[ ,.( prob=(sum(get(target))+w*p )/(.N+w) ),by=eval(var)]
}
DT_fill_NA <- function(DT,replacement=0) {
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j,replacement)
}
super_fread <- function( file , key_var=NULL){
  dt <- fread(file)
  if(!is.null(key_var)) setkeyv(dt,c(key_var))
  return(dt)
}

clicks_train  <- super_fread( "../input/clicks_train.csv", key_var = "ad_id" )

click_prob = clicks_train[,.(sum(clicked)/.N)]
ad_id_probs   <- get_probs(clicks_train,"ad_id","clicked",8)
rm(clicks_train)
gc()

clicks_test   <- super_fread( "../input/clicks_test.csv" , key_var = "ad_id" )
clicks_test <- merge( clicks_test, ad_id_probs, all.x = T )

DT_fill_NA( clicks_test, click_prob )

setkey(clicks_test,"prob")
submission <- clicks_test[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
setkey(submission,"display_id")

write.csv(submission,file = "submission.csv",row.names = F)