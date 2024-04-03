library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(xtable)

paths <- list.files("../performance", pattern = ".csv", recursive = T)
dirs <- dirname(paths)
files <- basename(paths)
files <- str_remove(files, ".csv")

results <- data.frame(matrix(nrow = length(paths)/2, ncol = 2))
names(results) <- unique(dirs)
rownames(results) <- unique(files)

for (f in 1:length(paths)){
  
  df <- fread(paste0("../performance/", paths[f]))
  overall_f1 <- ((df$V2[4]*df$V2[5]) + (df$V3[4]*df$V3[5])) / sum(df[5,2:3])
  results[which(rownames(results) == files[f]), dirs[f]] <- overall_f1
  
}

names(results) <- c("BERT", "RF")
rownames(results) <- c("Contact", "Donate", "Event", "Acquisition", "Vote", "Learn", "Poll", "Persuade", "Purchase")
results <- round(results, 3)
results <- t(results)
results <- results %>% as.data.frame() %>% select("Acquisition", "Contact", "Donate", "Event", "Learn", "Poll", "Persuade", "Purchase", "Vote")

xt <- xtable(results, digits = 3, label = "model_performance", caption = "Model performance (weighted F1 scores), DistilBERT and Random Forest.")
print.xtable(xt, file = "tables/bert_vs_rf_performance.tex")

