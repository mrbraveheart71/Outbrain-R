################################################
## R version of most popular local hotels
##################################################
library(data.table)
library(Metrics)
library(xgboost)
library(e1071)  

set.seed(50)
setwd("C:/R-Studio/Grupo Bimbo")
# Log Output

source("Grupo Bimbo Functions.R")

# Create files for now
validation <- 'Training'
calc.aggregates <- FALSE
first.test.week <- 9
source("Grupo Bimbo Data Load.R")
gc()
source("Grupo Bimbo Calculate Aggregates.R")
gc()
source("Grupo Bimbo Merge.R")
save("test","test.y","feature.names",file="Grupo Bimbo Training File Set")

gc()
validation <- 'Meta-Training'
calc.aggregates <- FALSE
first.test.week <- 8
source("Grupo Bimbo Data Load.R")
gc()
source("Grupo Bimbo Calculate Aggregates.R")
gc()
round = "roundTrainTest"
source("Grupo Bimbo Merge.R")
save("test","test.y","feature.names",file="Grupo Bimbo Meta-Training File Set Test")
test <- test[1,]
test.y <- test.y[1]
gc()
round = "roundHoldOut"
source("Grupo Bimbo Merge.R")
save("holdout","holdout.y","feature.names",file="Grupo Bimbo Meta-Training File Set Holdout")
gc()


validation <- 'Submission'
calc.aggregates <- FALSE
first.test.week <- 10
source("Grupo Bimbo Data Load.R")
gc()
source("Grupo Bimbo Calculate Aggregates.R")
gc()
source("Grupo Bimbo Merge.R")
save("test","test.id","feature.names",file="Grupo Bimbo Submission File Set")

gc()
