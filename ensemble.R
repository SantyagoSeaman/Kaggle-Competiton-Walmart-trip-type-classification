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

best.pred.matrix <- (0.8 * xg_17_00_16_e_1[, 2:39] + 0.2 * xg_19_05_00pred_k_2[, 2:39])

best.pred.matrix <- xg_09_24_12d_ensemble[, 2:39]
best.pred.matrix <- xg_09_29_05d_2_ensemble[, 2:39]
best.pred.matrix <- xg_09_51_01d_2_ensemble[, 2:39]
best.pred.matrix <- xg_10_24_19e_3_ensemble[, 2:39]
best.pred.matrix <- xg_12_33_23d_5_ensemble_less_015[, 2:39]
best.pred.matrix <- xg_21_51_30j_6_ensemble[, 2:39]
best.pred.matrix <- xg_12_00_53j_7_ensemble[, 2:39]



#for (letter in letters[3:13]) {
#for (letter in c('c', 'd', 'e')) {
for (letter in c('c', 'd', 'j')) {
    print(letter)
    files <- list.files(path = './saved rds',
                        pattern = paste0('^pred_', letter, '_.*\\.rds'), full.names = T)
    files <- c(files, list.files(path = './saved rds',
                         pattern = paste0('^test_pred_', letter, '_.*\\.rds'), full.names = T))
    #print(files)
    if (length(files) > 0) {
        pred.matrix <- 0
        counter <- 0
        for (file in files) {
            print(file)
            pred <- readRDS(file)
            diff <- rmse(best.pred.matrix, pred)
            print(diff)
            if (diff < 0.15) {
                pred.matrix <- pred.matrix + pred
                counter <- counter + 1
            }
        }
        if (counter > 0) {
            pred.matrix <- pred.matrix / counter
            print(rmse(best.pred.matrix, pred.matrix))
            #saveRDS(pred, file = paste0("./saved rds/mean_7_", letter,".rds"))

            pred.matrix <- 0.95 * best.pred.matrix + 0.05 * pred.matrix
            print(rmse(best.pred.matrix, pred.matrix))
            submission <- data.frame(VisitNumber = xg_17_00_16_e_1$VisitNumber, stringsAsFactors = F)
            submission$VisitNumber <- as.integer(submission$VisitNumber)
            submission <- cbind(submission, round(pred.matrix, 7))

            submissionName <- paste0("results/xg_", format(Sys.time(), "%H_%M_%S"), letter, '_8_ensemble')
            submissionFile <- paste0(submissionName, ".csv")
            write.csv(submission, submissionFile, sep=",", dec=".", col.names=TRUE, row.names=FALSE, quote = FALSE)
        }
    }
}


# попробовать c
# попробовать d < 0.015
# попробовать усреднить все csv в каталоге



