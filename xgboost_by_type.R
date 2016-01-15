#library(data.table)
library(xgboost)
library(Matrix)
library(methods)
#library(randomForest)
library(caret)
library(dplyr)
library(Metrics)
library(reshape2)
library(ROCR)

final.train$TripType_i <- as.integer(final.train$TripType)


levelsList <- levels(final.train$TripType)
for (level in levelsList[2:length(levelsList)]) {
    final.train[[paste0('TripType_f_', level)]] <- ifelse(final.train$TripType == level, 1, 0)
}
#names(final.train)
#View(final.train[, 100:ncol(final.train)])

level = "39"
for (level in levelsList[2:length(levelsList)]) {
    print(paste0('Level: ', level))

    predictorName <- paste0('TripType_f_', level)

    feature.names <- names(final.test)
    feature.names <- c('TotalDep', 'TotalCount', 'Weekday',
                       feature.names[grep('^dd_', feature.names)]
                       #feature.names[grep('^fn_', feature.names)],
                       #feature.names[grep('^upc_', feature.names)]
                       )
    feature.names <- feature.names[-c(1, 2)]
    feature.formula <- formula(paste(predictorName, ' ~ ', paste(feature.names, collapse = ' + '), sep = ''))

    dtest_cv <- final.test[, feature.names]
    dtest_cv[[predictorName]] <- 0
    dtest_cv[is.na(dtest_cv)] <- 0
    dtest <- sparse.model.matrix(feature.formula, data = dtest_cv)
    sparseMatrixColNamesTest <- colnames(dtest)
    print(paste0("Test data created: ", Sys.time()))

    for(i in 1:5) {
        print(paste0('i: ', i))
        resultColName = paste0('pred_l_', level, '_i_', i)

        dtrain_cv <- final.train[, c(feature.names, predictorName)]
        dtrain_cv[is.na(dtrain_cv)] <- 0
        indexes <- sample(seq_len(nrow(dtrain_cv)), floor(nrow(dtrain_cv)*0.95))
        data <- sparse.model.matrix(feature.formula, data = dtrain_cv[indexes, ])
        sparseMatrixColNamesTrain <- colnames(data)
        dtrain <- xgb.DMatrix(data, label = dtrain_cv[indexes, predictorName])
        rm(data)
        dvalid <- xgb.DMatrix(sparse.model.matrix(feature.formula, data = dtrain_cv[-indexes, ]),
                              label = dtrain_cv[-indexes, predictorName])
#         dfull <- xgb.DMatrix(sparse.model.matrix(feature.formula, data = dtrain_cv),
#                              label = dtrain_cv[[predictorName]])
        print(paste0("Train data created: ", Sys.time()))

        n_rounds = 1001
        watchlist <- list(eval = dvalid, train = dtrain)
        params <- list(booster = "gbtree", objective = "binary:logistic",
                       max_depth = 200, eta = 0.05,
                       colsample_bytree = 0.3, subsample = 0.3)

        model <- xgb.train(params = params, data = dtrain,
                           nrounds = n_rounds, early.stop.round = 10, maximize = F,
                           eval_metric = 'rmse',
                           watchlist = watchlist, print.every.n = 50)
        #feature.importance <- xgb.importance(sparseMatrixColNamesTrain, model = model)
        print(paste0("Model created: ", Sys.time()))

        final.train[indexes, resultColName] <- predict(model, dtrain)
        final.train[-indexes, resultColName] <- predict(model, dvalid)
        final.test[[resultColName]] <- predict(model, dtest)

        print(paste0("Data predicted: ", Sys.time()))
    }

    confusionMatrix(final.train[paste0('pred_l_', level, '_i_', i)] > 0.5, final.train$TripType == level)
}

submission <- data.frame(VisitNumber = final.test$VisitNumber, stringsAsFactors = F)
submission$VisitNumber <- as.integer(submission$VisitNumber)
for (level in levelsList[2:length(levelsList)]) {
    #print(paste0('Level: ', level))
    resultColName = paste0('pred_l_', level, '_i_', 1)
    submission[[paste0('TripType_', level)]] <- round(final.test[[resultColName]], 5)
}

View(final.train[final.train$TripType == 999, c('pred_l_999_i_1', colnames(final.train)[1:50])])
View(fullDataset[fullDataset$VisitNumber %in% c(5, 182, 190, 133, 207, 351, 7142, 17630, 5340, 17792), ])
View(fullDataset[fullDataset$Upc %in% c(76163520390), ])
View(final.train[final.train$VisitNumber == 57840, c('pred_l_999_i_1', colnames(final.train)[1:50])])

View(final.train[, c('VisitNumber', 'TripType', 'pred_l_3_i_1')])
nrow(final.train[final.train$TripType_f_999 == 1, ])
nrow(final.train[final.train$pred_l_999_i_1 > 0.5 & final.train$TripType_f_999 == 1, ])
nrow(final.train[final.train$pred_l_999_i_1 > 0.5 & final.train$TripType_f_999 == 0, ])


View(final.train[final.train$TripType == level, c('VisitNumber', 'TotalDep', 'TotalCount',
                                                  'pred_l_999_i_1', 'pred_l_999_i_2', 'pred_l_999_i_3')])

View(final.train[final.train$TripType == 3, c('VisitNumber', 'TotalDep', 'TotalCount',
                                                  'pred_l_3_i_1', 'pred_l_3_i_2', 'pred_l_3_i_3')])
View(final.train[final.train$TripType == 4, c('VisitNumber', 'TotalDep', 'TotalCount',
                                                  'pred_l_4_i_1', 'pred_l_4_i_2', 'pred_l_4_i_3')])


pred.matrix <- readRDS("./saved rds/pred_b_2.rds")
pred.matrix$TripType_3_1 <- rowMeans(final.test[, c('pred_l_3_i_1', 'pred_l_3_i_2', 'pred_l_3_i_3')])
pred.matrix$TripType_4_1 <- rowMeans(final.test[, c('pred_l_4_i_1', 'pred_l_4_i_2', 'pred_l_4_i_3')])
pred.matrix$TripType_5_1 <- rowMeans(final.test[, c('pred_l_5_i_1', 'pred_l_5_i_2', 'pred_l_5_i_3')])


View(pred.matrix[abs(pred.matrix$TripType_3 - pred.matrix$TripType_3_1) > 0.1, c('TripType_3', 'TripType_3_1')])
View(pred.matrix[abs(pred.matrix$TripType_4 - pred.matrix$TripType_4_1) > 0.1, c('TripType_4', 'TripType_4_1')])
View(pred.matrix[abs(pred.matrix$TripType_5 - pred.matrix$TripType_5_1) > 0.1, c('TripType_5', 'TripType_5_1')])


View(pred.matrix[pred.matrix$TripType_3 < 0.9 & pred.matrix$TripType_3_1 > 0.9, c('TripType_3', 'TripType_3_1')])



final.train$TripType_i <- as.integer(final.train$TripType)
final.train$pred_a_1
names(final.train)

View(final.train[final.train$TripType_i != final.train$pred_a_1, c('TripType', 'TripType_i', 'pred_a_1')])
hist(final.train[final.train$TripType_i != final.train$pred_a_1, c('TripType_i')], breaks = 1:40)



