options(scipen=999)


submission <- data.frame(VisitNumber = final.test$VisitNumber, stringsAsFactors = F)
submission$VisitNumber <- as.integer(submission$VisitNumber)

# l <- levels(final.train$TripType)
# for (vn in 1:nrow(final.test)) {
#     n <- paste0('TripType_', l[final.test[vn, 'pred_a_1']])
#     submission[vn, n] <- 1
# }
#
# submission[is.na(submission)] <- 0
# submission$TripType_14 <- 0

# tranNames <- names(train)
# submission$Sales <- as.integer(rowMeans(test[, tranNames[grep('pred[.]full[.]month[.]([0-9]{1,2})', tranNames)]]))

# pred.matrix.2 <- lapply(pred.matrix, function(v) {
#     sapply(v, function(v1){ifelse(v1 >= 0.9, 1, v1)})
# })
# pred.matrix.2 <- as.data.frame(pred.matrix.2)

submission <- cbind(submission, round(pred.matrix, 7))

submissionName <- paste0("results/xg_", format(Sys.time(), "%H_%M_%S"), '_superb_wide')
submissionFile <- paste0(submissionName, ".csv")
write.csv(submission, submissionFile, sep=",", dec=".", col.names=TRUE, row.names=FALSE, quote = FALSE)

