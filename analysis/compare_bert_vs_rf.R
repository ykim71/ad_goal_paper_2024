library(data.table)
library(dplyr)
library(xtable)

rf_results <- fread("../performance/rf_2020_2022_feature_comparison_weighted.csv", data.table = F)
bert_results <- fread("../performance/bert_2020_2022_feature_comparison_weighted.csv", data.table = F)

results <- rbind(bert_results, rf_results[3,])
results <- results %>% select(-V1)
goal_names <- data.frame(wrong = c("DONATE", "CONTACT", "PURCHASE", "GOTV", "EVENT", "POLL", "GATHERINFO", "LEARNMORE", "PRIMARY_PERSUADE"),
                         correct = c("Donate", "Contact", "Purchase", "Vote", "Event", "Poll", "Acquisition", "Learn", "Persuade"))
rename_strings <- function(x) {
  ifelse(x %in% goal_names$wrong, goal_names$correct[match(x, goal_names$wrong)], x)
}
names(results) <- rename_strings(names(results))
results <- results[,order(colnames(results))]
rownames(results) <- c("DistilBERT", "Random Forest")

xt <- xtable(results, digits = 3, label = "model_performance", caption = "DistilBERT and Random Forest classifier performance given as weighted F1 scores. Generally, the DistilBERT classifiers outperformed the Random Forest classifiers reaching a mean F1 score of 0.95 across classifiers.")
print.xtable(xt, file = "tables/bert_vs_rf_performance.tex", comment = F)

mean(as.numeric(results[1,])) # average F1-score reported in the paper


#----
rf_results <- fread("../performance/rf_2020_2022_feature_comparison_class1.csv", data.table = F)
bert_results <- fread("../performance/bert_2020_2022_feature_comparison_class1.csv", data.table = F)

results <- rbind(bert_results, rf_results[3,])
results <- results %>% select(-V1)
goal_names <- data.frame(wrong = c("DONATE", "CONTACT", "PURCHASE", "GOTV", "EVENT", "POLL", "GATHERINFO", "LEARNMORE", "PRIMARY_PERSUADE"),
                         correct = c("Donate", "Contact", "Purchase", "Vote", "Event", "Poll", "Acquisition", "Learn", "Persuade"))
rename_strings <- function(x) {
  ifelse(x %in% goal_names$wrong, goal_names$correct[match(x, goal_names$wrong)], x)
}
names(results) <- rename_strings(names(results))
results <- results[,order(colnames(results))]
rownames(results) <- c("DistilBERT", "Random Forest")

xt <- xtable(results, digits = 3, label = "model_performance_positive", caption = "DistilBERT and Random Forest classifier performance given as F1 scores of the positive cases. Generally, the DistilBERT classifiers outperformed the Random Forest classifiers reaching a mean F1 score of 0.81 across classifiers.")
print.xtable(xt, file = "tables/bert_vs_rf_performance_positive.tex", comment = F)

mean(as.numeric(results[1,])) # average F1-score reported in the paper


#----
results <- fread("../performance/rf_2020_2022_feature_comparison_weighted.csv", data.table = F)
results <- results %>% select(-V1)
goal_names <- data.frame(wrong = c("DONATE", "CONTACT", "PURCHASE", "GOTV", "EVENT", "POLL", "GATHERINFO", "LEARNMORE", "PRIMARY_PERSUADE"),
                         correct = c("Donate", "Contact", "Purchase", "Vote", "Event", "Poll", "Acquisition", "Learn", "Persuade"))
rename_strings <- function(x) {
  ifelse(x %in% goal_names$wrong, goal_names$correct[match(x, goal_names$wrong)], x)
}
names(results) <- rename_strings(names(results))
results <- results[,order(colnames(results))]
rownames(results) <-  c("Ad creative body", "Ad creative body + ASR + OCR", "All fields")

xt <- xtable(results, digits = 3, label = "model_performance_feature_comparison", caption = "Random Forest classifier performance for different feature sets, given as weighted F1 scores.")
print.xtable(xt, file = "tables/rf_performance_features.tex", comment = F)

