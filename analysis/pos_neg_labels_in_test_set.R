# Rows 3 and 4 of Table 2 in the paper

library(ggcorrplot)
library(haven)
library(dplyr)
library(data.table)

df <- fread("../data/test_2020_2022.csv")
df <- df %>% select(DONATE:PRIMARY_PERSUADE,year)

goal_names <- data.frame(wrong = c("DONATE", "CONTACT", "PURCHASE", "GOTV", "EVENT", "POLL", "GATHERINFO", "LEARNMORE", "PRIMARY_PERSUADE"),
                         correct = c("Donate", "Contact", "Purchase", "Vote", "Event", "Poll", "Acquisition", "Learn", "Persuade"))
rename_strings <- function(x) {
  ifelse(x %in% goal_names$wrong, goal_names$correct[match(x, goal_names$wrong)], x)
}
names(df) <- rename_strings(names(df))

# reorder columns except year alphabetically
df <- df %>% select(year, sort(names(df)[-10]))

pos_labels <- df %>% group_by(year) %>% summarize(across(Acquisition:Vote, sum))
neg_labels <- table(df$year)-pos_labels


