library(ggcorrplot)
library(haven)
library(dplyr)
library(data.table)

df <- fread("../data/humancoded_2020_2022.csv.gz")
df <- df %>% select(DONATE:PRIMARY_PERSUADE)

goal_names <- data.frame(wrong = c("DONATE", "CONTACT", "PURCHASE", "GOTV", "EVENT", "POLL", "GATHERINFO", "LEARNMORE", "PRIMARY_PERSUADE"),
                         correct = c("Donate", "Contact", "Purchase", "Vote", "Event", "Poll", "Acquisition", "Learn", "Persuade"))
rename_strings <- function(x) {
  ifelse(x %in% goal_names$wrong, goal_names$correct[match(x, goal_names$wrong)], x)
}
names(df) <- rename_strings(names(df))

cor_matrix <- cor(df, use = "complete.obs")

#----
df2 <- fread("../data/fb2022_predicted_goals_bert_all.csv.gz")
df2 <- df2 %>% select(-ad_id)
names(df2) <- rename_strings(names(df))
cor_matrix2 <- cor(df2, use = "complete.obs")

# Combined matrix
mat_combined <- cor_matrix
mat_combined[lower.tri(mat_combined)] <- cor_matrix2[lower.tri(cor_matrix2)]
diag(mat_combined) <- NA
ggcorrplot(mat_combined, method = "circle")
ggsave("figures/goal_overlap.pdf", height = 6, width = 6)
