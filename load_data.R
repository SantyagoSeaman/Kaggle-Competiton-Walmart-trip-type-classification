#library(data.table)
library(xgboost)
library(Matrix)
library(methods)
#library(randomForest)
library(caret)
library(dplyr)
library(Metrics)
options(scipen=999)


test <- read.csv("./data/test.csv", stringsAsFactors = F)
train <- read.csv("./data/train.csv", stringsAsFactors = F)

test$TripType <- -1

# --------------------------------------
fullDataset <- rbind(train, test)

fullDataset[is.na(fullDataset$Upc), 'Upc'] <- 0
fullDataset[is.na(fullDataset$FinelineNumber), 'FinelineNumber'] <- 0
fullDataset$FinelineNumber <- as.integer(fullDataset$FinelineNumber)

fullDataset$TripType_i <- as.integer(fullDataset$TripType)
fullDataset$TripType <- factor(fullDataset$TripType_i)
fullDataset$Weekday <- factor(fullDataset$Weekday,
                              levels = c('Monday', 'Tuesday', 'Wednesday',
                                         'Thursday', 'Friday', 'Saturday', 'Sunday'))
fullDataset$Weekday_i <- as.integer(fullDataset$Weekday)
fullDataset$DepartmentDescription <- factor(fullDataset$DepartmentDescription)
fullDataset$DepartmentDescription_i <- as.integer(fullDataset$DepartmentDescription)

rm(test)
rm(train)

# --------------------------------------
visitNumbers <- unique(fullDataset$VisitNumber)
finalDataset <- data.frame(VisitNumber = visitNumbers, TotalDep = 0, TotalCount = 0, Weekday = 0)
for (dd.level in sort(unique(fullDataset$DepartmentDescription_i))) {
    finalDataset[[paste0('dd_', dd.level)]] <- 0
}

# --------------------------------------
idx <- duplicated(fullDataset$VisitNumber)
#table(idx)
finalDataset$TripType <- fullDataset[!idx, 'TripType']

# --------------------------------------
counter <- 1
for (visit in visitNumbers[1:length(visitNumbers)]) {
    visitData <- fullDataset[fullDataset$VisitNumber == visit, ]

    fields_i <- unique(visitData$DepartmentDescription_i)
    fields_c = paste('dd_', fields_i, sep = '')
    dataToAdd = c()
    for (field in fields_i) {
        dataToAdd <- c(dataToAdd,
                       sum(visitData[visitData$DepartmentDescription_i == field, 'ScanCount']))
    }

    finalDataset[finalDataset$VisitNumber == visit, fields_c] <- dataToAdd
    finalDataset[finalDataset$VisitNumber == visit, c('TotalCount', 'TotalDep', 'Weekday')] <- c(
        sum(dataToAdd),
        length(fields_i),
        visitData[1, 'Weekday_i'])

    counter <- counter + 1
    if (counter %% 1000 == 0) {
        print(counter)
    }
}


# -----------------------------------------------------
tranNames <- names(finalDataset)
ddNames <- tranNames[grep('dd_([0-9]{1,2})$', tranNames)]
for (name in ddNames) {
    finalDataset[[paste0(name, '_p')]] <- ifelse(finalDataset[[name]] > 0, 1, 0)
}

# -----------------------------------------------------
tranNames <- names(finalDataset)
ddNames <- tranNames[grep('dd_([0-9]{1,2})$', tranNames)]
ddNamesP <- tranNames[grep('dd_([0-9]{1,2})_p$', tranNames)]

ddNamesCounts <- colSums(finalDataset[, ddNamesP])
ddNamesCounts <- ddNamesCounts[order(ddNamesCounts, decreasing = T)]
ddNamesCounts <- gsub('_p', '', names(ddNamesCounts))
ddNamesTop <- head(ddNamesCounts, 30)

ddList_2 <- c()
ddList_3 <- c()
ddList_4 <- c()
ddList_5 <- c()
ddList_6 <- c()
for (i in 1:length(visitNumbers)) {
#for (i in 1:1000) {
    #colNames <- head(ddNamesCounts[which(finalDataset[i, ddNamesCounts] > 0)], 3)
    colNames <- head(ddNamesTop[which(finalDataset[i, ddNamesTop] > 0)], 2)
    ddList_2 <- c(ddList_2, paste(gsub('dd_', '', colNames), collapse = '_'))

    colNames <- head(ddNamesTop[which(finalDataset[i, ddNamesTop] > 0)], 3)
    ddList_3 <- c(ddList_3, paste(gsub('dd_', '', colNames), collapse = '_'))

    colNames <- head(ddNamesTop[which(finalDataset[i, ddNamesTop] > 0)], 4)
    ddList_4 <- c(ddList_4, paste(gsub('dd_', '', colNames), collapse = '_'))

    colNames <- head(ddNamesTop[which(finalDataset[i, ddNamesTop] > 0)], 5)
    ddList_5 <- c(ddList_5, paste(gsub('dd_', '', colNames), collapse = '_'))

    colNames <- head(ddNamesTop[which(finalDataset[i, ddNamesTop] > 0)], 6)
    ddList_6 <- c(ddList_6, paste(gsub('dd_', '', colNames), collapse = '_'))

    if (i %% 1000 == 0) {
        print (i)
    }
}

ddList.len <- unlist(lapply(ddList, function(r) { return(nchar(r)); }))
hist(ddList.len)

ddListFactor <- factor(ddList)
head(levels(ddListFactor), 50)
length(levels(ddListFactor))
table(ddListFactor)

ddListFactor <- factor(ddList_2)
head(levels(ddListFactor), 50)
table(ddListFactor)

ddListFactor <- factor(ddList_6)
head(levels(ddListFactor), 50)
table(ddListFactor)


#finalDataset$dd_aggregated <- ddListFactor
finalDataset$dd_aggregated_2 <- as.integer(factor(ddList_2))
finalDataset$dd_aggregated_3 <- as.integer(factor(ddList_3))
finalDataset$dd_aggregated_4 <- as.integer(factor(ddList_4))
finalDataset$dd_aggregated_5 <- as.integer(factor(ddList_5))
finalDataset$dd_aggregated_6 <- as.integer(factor(ddList_6))




--------------------------------------
--------------------------------------
for (fn.level in sort(unique(fullDataset$FinelineNumber))) {
    finalDataset[[paste0('fn_', fn.level)]] <- 0
}


counter <- 1
for (visit in visitNumbers[counter:length(visitNumbers)]) {
    visitData <- fullDataset[fullDataset$VisitNumber == visit, ]

    fields_i <- unique(visitData$FinelineNumber)
    fields_c = paste('fn_', fields_i, sep = '')
    finalDataset[finalDataset$VisitNumber == visit, fields_c] <- 1

    counter <- counter + 1
    if (counter %% 1000 == 0) {
        print(counter)
    }
}

saveRDS(finalDataset, file = "wideFinalDataset_191348_5495_2.rds")
#finalDataset <- readRDS("wideFinalDataset_191348_5495.rds")


# -----------------------------------------------------
finelineNumberFreq <- table(fullDataset$FinelineNumber)
finelineNumberFreq <- finelineNumberFreq[finelineNumberFreq > 50]
finelineNumberTop <- names(finelineNumberFreq)

mainColumns <- unique(c("VisitNumber", "TotalDep", "TotalCount", "TripType", "Weekday",
                        names(finalDataset)[1:142]))

finelineNumberTopColumns <- paste('fn_', finelineNumberTop, sep = '')
finalDataset <- finalDataset[, c(mainColumns, finelineNumberTopColumns)]

saveRDS(finalDataset, file = "wideFinalDataset_191348_3102.rds")
#finalDataset <- readRDS("wideFinalDataset_191348_3102.rds")

# -----------------------------------------------------
upcFreq <- table(fullDataset$Upc)
upcFreq <- upcFreq[upcFreq < 20]
upcFreq <- sort(upcFreq)
upcTop <- names(upcFreq)
upcTopNames <- paste0('upc_', upcTop)

for (upc in upcTop) {
    finalDataset[[paste0('upc_', upc)]] <- 0
}

for (v in split(visitNumbers, ceiling(seq_along(visitNumbers)/1000))) {
    upc.arr <- data.frame(t(apply(fullDataset[fullDataset$VisitNumber %in% v, c('VisitNumber', 'Upc')], 1,
        function(row) {
            return (c(row['VisitNumber'], as.integer(upcTop %in% intersect(unique(row['Upc']), upcTop))))
        })))

    print(paste0('Calculated: ', v[1], ' at ', Sys.time()))

    names(upc.arr) <- c('VisitNumber', upcTopNames)
    upc.arr <- aggregate(upc.arr, by = list(upc.arr$VisitNumber), FUN=sum)
    upc.arr[, 1] <- NULL
    upc.arr[, 1] <- NULL
    finalDataset[finalDataset$VisitNumber %in% v, upcTopNames] <- upc.arr

    print(paste0('Applied: ', v[1], ' at ', Sys.time()))
}


dataset.classes <- sapply(finalDataset, class)
dataset.classes <- names(dataset.classes[dataset.classes == "numeric"])
for (n in dataset.classes) {
    finalDataset[[n]] <- as.integer(finalDataset[[n]])
    print(n)
}

finalDataset[is.na(finalDataset)] <- 0

saveRDS(finalDataset, file = "wideFinalDataset_191348_17547.rds")
#finalDataset <- readRDS("wideFinalDataset_191348_7073.rds")



# --------------------------------------
counter <- 1
for (visit in visitNumbers[1:length(visitNumbers)]) {
    data <- fullDataset[fullDataset$VisitNumber == visit & fullDataset$ScanCount > 0, 'ScanCount']

    finalDataset[finalDataset$VisitNumber == visit, 'ScanCountPos'] <- sum(data[data > 0])
    finalDataset[finalDataset$VisitNumber == visit, 'ScanCountNeg'] <- -sum(abs(data[data < 0]))

    counter <- counter + 1
    if (counter %% 1000 == 0) {
        print(counter)
    }
}


# -----------------------------------------------------
final.train <- finalDataset[finalDataset$TripType != "-1", ]
final.test <- finalDataset[finalDataset$TripType == "-1", ]

