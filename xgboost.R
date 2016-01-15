#library(data.table)
library(xgboost)
library(Matrix)
library(methods)
#library(randomForest)
library(caret)
library(dplyr)
library(Metrics)
#library(reshape2)



feature.names <- names(final.test)
feature.names <- c('TotalDep', 'TotalCount', 'Weekday',
                   feature.names[grep('^dd_([0-9]{1,2})', feature.names)],
                   feature.names[grep('^fn_([0-9]{1,2})', feature.names)])
feature.names <- feature.names[-c(1, 2)]
feature.formula <- formula(paste('TripType ~ ', paste(feature.names, collapse = ' + '), sep = ''))


dtest_cv <- final.test[, feature.names]
dtest_cv$TripType = 0
dtest_cv[is.na(dtest_cv)]<- 0
dtest <- sparse.model.matrix(feature.formula, data = dtest_cv)
sparseMatrixColNamesTest <- colnames(dtest)
errors.valid <- c()

# сделать бинарную классификацию для каждого из классов - fail
# сделать мультикласс svm по всем предикторам
# сделать кластеризацию и посмотреть на группы и их кореляцию с классами
# колаборативная фильтрация. найти для каждого покупателя всех похожих покупателей по всем признакам, найти распределение классов у этих покупателей, найти вероятности каждого из этих классов для покупателя

for(i in 4:20) {
    print(i)
    resultColName = paste0('pred_b_', i)

    dtrain_cv <- final.train[, c(feature.names, 'TripType')]
    #dtrain_cv[is.na(dtrain_cv)] <- 0
    indexes <- sample(seq_len(nrow(dtrain_cv)), floor(nrow(dtrain_cv)*0.95))
    data <- sparse.model.matrix(feature.formula, data = dtrain_cv[indexes, ])
    sparseMatrixColNamesTrain <- colnames(data)
    dtrain <- xgb.DMatrix(data, label = dtrain_cv[indexes, 'TripType'])
    rm(data)
    dvalid <- xgb.DMatrix(sparse.model.matrix(feature.formula, data = dtrain_cv[-indexes, ]),
                          label = dtrain_cv[-indexes, 'TripType'])
#     dfull <- xgb.DMatrix(sparse.model.matrix(feature.formula, data = dtrain_cv),
#                          label = dtrain_cv$TripType)
    print(paste0("Data created: ", Sys.time()))

    n_rounds = 2001
    watchlist <- list(eval = dvalid, train = dtrain)
    params <- list(booster = "gbtree", objective = "multi:softprob", # multi:softprob   multi:softmax
                   max_depth = 30, eta = 0.01,
                   colsample_bytree = 0.7, subsample = 0.7)

    num.class <- length(levels(final.train$TripType)) + 1
    model <- xgb.train(params = params, data = dtrain,
                       nrounds = n_rounds, early.stop.round = 10, maximize = F,
                       eval_metric = 'mlogloss', num_class = num.class,
                       watchlist = watchlist, print.every.n = 10)
    #feature.importance <- xgb.importance(sparseMatrixColNamesTrain, model = model)
    print(paste0("Model created: ", Sys.time()))

    pred.test <- predict(model, dtest)
    pred.matrix <- data.frame(matrix(pred.test, 95674, num.class, byrow=T))
    pred.matrix <- pred.matrix[, 3:ncol(pred.matrix)]
    colnames(pred.matrix) <- paste0('TripType_', levels(final.train$TripType)[2:39])
    print(paste0("Prediction created: ", Sys.time()))

    saveRDS(pred.matrix, file = paste0("./saved rds/", resultColName, ".rds"))
    #df2 <- readRDS("mytweets.rds")

    final.train <- predict(model, dfull)

    #final.train[[resultColName]] <- predict(model, dfull)
    #final.test[[resultColName]] <- predict(model, dtest)
}

# 1 - [1410]	eval-mlogloss:0.853447	train-mlogloss:0.593084   - 0.89
# 2 - [1520]	eval-mlogloss:0.862251	train-mlogloss:0.581659
# 3 - [1560]	eval-mlogloss:0.845957	train-mlogloss:0.579467   - 0.89
# 4 - [1670]	eval-mlogloss:0.886612	train-mlogloss:0.567437
# 5 - [1620]	eval-mlogloss:0.875422	train-mlogloss:0.573119



# 2 - [1660]	eval-mlogloss:0.768898	train-mlogloss:0.184358   - 0.78854
# 3 - [1540]	eval-mlogloss:0.772780	train-mlogloss:0.194516   - 0.78877


pred.matrix <- readRDS("./saved rds/pred_b_3.rds")




# 0.78854
# n_rounds = 2001
# watchlist <- list(eval = dvalid, train = dtrain)
# params <- list(booster = "gbtree", objective = "multi:softprob", # multi:softprob   multi:softmax
#                max_depth = 30, eta = 0.01,
#                colsample_bytree = 0.7, subsample = 0.7)

