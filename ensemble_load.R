xg_17_00_16_e_1 <- read.csv("./results/xg_17_00_16_e_1.csv")
namesList <- names(xg_17_00_16_e_1[, 2:39])
xg_19_05_00pred_k_2 <- read.csv("./results/xg_19_05_00pred_k_2.csv")
xg_19_18_33pred_k_5 <- read.csv("./results/xg_19_18_33pred_k_5.csv")
xg_09_24_12d_ensemble <- read.csv("./results/xg_09_24_12d_ensemble.csv")
xg_09_29_05d_2_ensemble <- read.csv("./results/xg_09_29_05d_2_ensemble.csv")
xg_09_51_01d_2_ensemble <- read.csv("./results/xg_09_51_01d_2_ensemble.csv")
xg_10_24_19e_3_ensemble <- read.csv("./results/xg_10_24_19e_3_ensemble.csv")
xg_12_33_23d_5_ensemble_less_015 <- read.csv("./results/xg_12_33_23d_5_ensemble_less_015.csv")
xg_21_51_30j_6_ensemble <- read.csv("./results/xg_21_51_30j_6_ensemble.csv")
xg_11_23_54j_ensemble_total_1 <- read.csv("./results/xg_11_23_54j_ensemble_total_1.csv")
xg_12_00_53j_7_ensemble <- read.csv("./results/xg_12_00_53j_7_ensemble.csv")



View(round(xg_17_00_16_e_1[, 2:39], 2))
View(round(test_pred_k_1, 2))
View(round(test_pred_k_3, 2))
View(round(test_pred_k_3[test_pred_k_3$TripType_14 > 0.1,], 5))


colSums(round(xg_17_00_16_e_1[, 2:39], 2))
colSums(round(test_pred_k_1, 2))
colSums(round(test_pred_k_2, 2))
colSums(round(test_pred_k_3, 2))
colSums(round(train_pred_k_1, 2))
colSums(round(train_pred_k_2, 2))
colSums(round(train_pred_k_3, 2))
