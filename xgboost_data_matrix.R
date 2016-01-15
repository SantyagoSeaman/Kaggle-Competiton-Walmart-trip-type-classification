#library(data.table)
library(xgboost)
library(Matrix)
library(methods)
#library(randomForest)
library(caret)
library(dplyr)
library(Metrics)
#library(reshape2)
options(scipen=999)



feature.names <- names(final.test)
feature.names <- c('TotalDep', 'TotalCount', 'Weekday',
                   feature.names[grep('^dd_', feature.names)],
                   feature.names[grep('^fn_', feature.names)],
                   feature.names[grep('^upc_([0-9]{3,})', feature.names)])
#feature.names <- feature.names[-c(1, 2)]
#feature.formula <- formula(paste('TripType ~ ', paste(feature.names, collapse = ' + '), sep = ''))


dtest_cv <- final.test[, feature.names]
dtest_cv$Weekday <- as.numeric(dtest_cv$Weekday)
#dtest_cv$TripType = 0
#dtest_cv[is.na(dtest_cv)] <- 0
dtest <- data.matrix(dtest_cv)
sparseMatrixColNamesTest <- colnames(dtest)

dtrain_cv <- final.train[, c('TripType', feature.names)]
dtrain_cv$Weekday <- as.numeric(dtrain_cv$Weekday)

for(i in 2:20) {
    print(i)
    resultColName = paste0('pred_d_', i)

    indexes <- sample(seq_len(nrow(dtrain_cv)), floor(nrow(dtrain_cv)*0.95))
    dtrain <- xgb.DMatrix(data.matrix(dtrain_cv[indexes, feature.names]), label = dtrain_cv[indexes, 'TripType'])
    dvalid <- xgb.DMatrix(data.matrix(dtrain_cv[-indexes, feature.names]), label = dtrain_cv[-indexes, 'TripType'])
    print(paste0("Data created: ", Sys.time()))

    n_rounds = 1001
    watchlist <- list(eval = dvalid, train = dtrain)
    params <- list(booster = "gbtree", objective = "multi:softprob", # multi:softprob   multi:softmax
                   max_depth = 50, eta = 0.01,
                   colsample_bytree = 0.8, subsample = 0.8)

    num.class <- length(levels(final.train$TripType)) + 1
    model <- xgb.train(params = params, data = dtrain,
                       nrounds = n_rounds, early.stop.round = 10, maximize = F,
                       eval_metric = 'mlogloss', num_class = num.class,
                       watchlist = watchlist, print.every.n = 10)
    print(paste0("Model created: ", Sys.time()))

    pred.test <- predict(model, dtest)
    pred.matrix <- data.frame(matrix(pred.test, 95674, num.class, byrow=T))
    pred.matrix <- pred.matrix[, 3:ncol(pred.matrix)]
    colnames(pred.matrix) <- paste0('TripType_', levels(final.train$TripType)[2:39])

    print(paste0("Prediction created: ", Sys.time()))

    saveRDS(pred.matrix, file = paste0("./saved rds/", resultColName, ".rds"))
}




# n_rounds = 1001
# watchlist <- list(eval = dvalid, train = dtrain)
# params <- list(booster = "gbtree", objective = "multi:softprob", # multi:softprob   multi:softmax
#                max_depth = 50, eta = 0.05,
#                colsample_bytree = 0.8, subsample = 0.8)
#
# num.class <- length(levels(final.train$TripType)) + 1
# model <- xgb.train(params = params, data = dtrain,
#                    nrounds = n_rounds, early.stop.round = 10, maximize = F,
#                    eval_metric = 'mlogloss', num_class = num.class,
#                    watchlist = watchlist, print.every.n = 10)

# pred_c_1 [240]	eval-mlogloss:0.706325	train-mlogloss:0.102928
# pred_c_2 [240]	eval-mlogloss:0.698524	train-mlogloss:0.103086   -- 0.70048
# pred_c_3 [220]	eval-mlogloss:0.682842	train-mlogloss:0.111778   -- 0.70056
# pred_c_4 [210]	eval-mlogloss:0.692560	train-mlogloss:0.115789   -- 0.69912
# pred_c_5 [240]	eval-mlogloss:0.675416	train-mlogloss:0.103464   -- 0.70171


# 0.68568
# [200]	eval-mlogloss:0.657751	train-mlogloss:0.115194
# [210]	eval-mlogloss:0.657182	train-mlogloss:0.109970
# [220]	eval-mlogloss:0.656847	train-mlogloss:0.105154
# [230]	eval-mlogloss:0.656058	train-mlogloss:0.100793
# [240]	eval-mlogloss:0.655898	train-mlogloss:0.096943



pred.matrix <- readRDS("./saved rds/pred_c_2.rds")
