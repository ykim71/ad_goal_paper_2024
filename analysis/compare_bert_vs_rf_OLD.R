library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(xtable)

paths <- list.files("../performance", pattern = ".csv", recursive = T)
dirs <- dirname(paths)
files <- basename(paths)
files <- str_remove(files, ".csv")

# Weighted F1-scores

results <- data.frame(matrix(nrow = length(paths)/2, ncol = 3))
names(results) <- c(unique(dirs), "labels")
rownames(results) <- unique(files)

for (f in 1:length(paths)){
  
  df <- fread(paste0("../performance/", paths[f]))
  overall_f1 <- ((df$V2[4]*df$V2[5]) + (df$V3[4]*df$V3[5])) / sum(df[5,2:3])
  results[which(rownames(results) == files[f]), dirs[f]] <- overall_f1
  results[which(rownames(results) == files[f]), 3] <- paste(df$V2[5], df$V3[5], sep = "|")
  
}

names(results) <- c("DistilBERT", "Random Forest", "Label freq (0|1)")
rownames(results) <- c("Contact", "Donate", "Event", "Acquisition", "Vote", "Learn", "Poll", "Persuade", "Purchase")
results[,1:2] <- round(results[,1:2], 3)
results <- t(results)
results <- results %>% as.data.frame() %>% select("Acquisition", "Contact", "Donate", "Event", "Learn", "Poll", "Persuade", "Purchase", "Vote")
results <- results[,order(colnames(results))]
mean(as.numeric(results[1,])) # average F1-score reported in the paper

xt <- xtable(results, digits = 3, label = "model_performance", caption = "Model performance (weighted F1 scores), DistilBERT and Random Forest. The third row indicates the frequencies of the 0/1 labels in the test set.")
print.xtable(xt, file = "tables/bert_vs_rf_performance.tex")

# Positive only

results <- data.frame(matrix(nrow = length(paths)/2, ncol = 2))
names(results) <- unique(dirs)
rownames(results) <- unique(files)

for (f in 1:length(paths)){
  
  df <- fread(paste0("../performance/", paths[f]))
  overall_f1 <- df$V3[4]
  results[which(rownames(results) == files[f]), dirs[f]] <- overall_f1
  
}

names(results) <- c("DistilBERT", "Random Forest")
rownames(results) <- c("Contact", "Donate", "Event", "Acquisition", "Vote", "Learn", "Poll", "Persuade", "Purchase")
results[,1:2] <- round(results[,1:2], 3)
results <- t(results)
results <- results %>% as.data.frame() %>% select("Acquisition", "Contact", "Donate", "Event", "Learn", "Poll", "Persuade", "Purchase", "Vote")
results <- results[,order(colnames(results))]
round(mean(as.numeric(results[1,])), 2) # average F1-score reported in the paper

xt <- xtable(results, digits = 3, label = "model_performance_positive", caption = "DistilBERT and Random Forest classifier performance given as F1 scores of the positive cases. Generally, the DistilBERT classifiers outperformed the Random Forest classifiers reaching a mean F1 score of 0.82 across classifiers.")
print.xtable(xt, file = "tables/bert_vs_rf_performance_positive.tex")

