library(jsonlite)
library(parallel)
library(pbapply)
library(data.table)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)


# Combine relevant vars from multiple datasets
fb22_pred <- fread("../data/fb2022_predicted_goals_bert_all.csv.gz", data.table = F)

goal_names <- data.frame(wrong = c("DONATE", "CONTACT", "PURCHASE", "GOTV", "EVENT", "POLL", "GATHERINFO", "LEARNMORE", "PRIMARY_PERSUADE"),
                         correct = c("Donate", "Contact", "Purchase", "Vote", "Event", "Poll", "Acquisition", "Learn", "Persuade"))
rename_strings <- function(x) {
  ifelse(x %in% goal_names$wrong, goal_names$correct[match(x, goal_names$wrong)], x)
}

names(fb22_pred) <- rename_strings(names(fb22_pred))

fb22_vars <- fread("../data/fb_2022_adid_var1.csv.gz", data.table = F)
fb22_vars <- fb22_vars %>% select(ad_id, page_id, pd_id, spend)
fb22_vars$spend <- str_split_fixed(fb22_vars$spend, ",", 2) %>%
  apply(., 2, function(x){str_extract(x, "[0-9]+")}) %>%
  apply(., 1, function(x){mean(as.numeric(x))})

fb22_vars2 <- fread("../../fb_2022/fb_2022_adid_text.csv.gz", data.table = F)
fb22_vars2 <- fb22_vars2 %>% select(ad_id, page_name)

fb22 <- left_join(fb22_vars, fb22_vars2, by = "ad_id")
fb22 <- left_join(fb22, fb22_pred, by = "ad_id")

rm(list = ls()[ls() != "fb22"])

#----
# Extract age data from the json

extract_age <- function(x){
  
  json_str <- str_replace_all(x, "\"\"", "\"")
  df_demographic <- fromJSON(json_str)
  df_demographic$percentage <- as.numeric(df_demographic$percentage)
  df_demographic <- df_demographic %>% group_by(age) %>% summarize(percentage = sum(percentage))
  
  age_equivalents <- data.frame(
    age_group = c("13-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
    age = c(15, 21, 29.5, 39.5, 49.5, 59.5, 69.5)
  )
  
  df_demographic$age <- age_equivalents$age[match(df_demographic$age, age_equivalents$age_group)]
  ad_demographic <- sum(df_demographic$age*df_demographic$percentage)
  
  return(ad_demographic)
}

fb22_vars <- fread("../data/fb_2022_adid_var1.csv.gz", data.table = F)
test <- fb22_vars[fb22_vars$demographic_distribution != "",]

cl <- makeCluster(12L)
clusterExport(cl, c("str_replace_all", "fromJSON", "%>%", "summarize", "group_by"))
ads_age <- pblapply(cl = cl, test$demographic_distribution, extract_age)
stopCluster(cl)

test$age <- unlist(ads_age)
age <- test %>% select(ad_id, age)

fb22_age <- right_join(fb22, age, by = "ad_id")

#----
# Make the plot(s)

# These aren't actually used in the paper
ggplot(fb22_age, aes(age)) + geom_density()
ggplot(fb22_age[fb22_age$Donate == 1,], aes(age)) + geom_density()
ggplot(fb22_age[fb22_age$Contact == 1,], aes(age)) + geom_density()
ggplot(fb22_age[fb22_age$Purchase == 1,], aes(age)) + geom_density()
ggplot(fb22_age[fb22_age$Vote == 1,], aes(age)) + geom_density()
ggplot(fb22_age[fb22_age$Event == 1,], aes(age)) + geom_density()
ggplot(fb22_age[fb22_age$Poll == 1,], aes(age)) + geom_density()
ggplot(fb22_age[fb22_age$Info == 1,], aes(age)) + geom_density()
ggplot(fb22_age[fb22_age$Learn == 1,], aes(age)) + geom_density()
ggplot(fb22_age[fb22_age$Persuade == 1,], aes(age)) + geom_density()

mean(fb22_age$age[fb22_age$Donate == 1])
mean(fb22_age$age[fb22_age$Vote == 1])
mean(fb22_age$age[fb22_age$Learn == 1], na.rm = T)

fb22_age_df <- fb22_age %>% select(Donate:Persuade) %>% as.matrix(.) * fb22_age$age
fb22_age_df <- as.data.frame(fb22_age_df)
fb22_age_df$ad_id <- fb22_age$ad_id
fb22_age_df <- fb22_age_df %>% pivot_longer(-ad_id, names_to = "Goal", values_to = "Age")
fb22_age_df <- fb22_age_df[fb22_age_df$Age != 0,]
fb22_age_df <- fb22_age_df[is.na(fb22_age_df$Age) == F,]

# Figure 3
ggplot(fb22_age_df, aes(Age)) + 
  geom_density() + 
  stat_summary(aes(xintercept = ..x.., y = 0), fun = mean, geom = "vline", orientation = "y", color = "darkgray") +
  stat_summary(aes(label = ..x.., y = 0.09), fun = function(x){round(mean(x), 2)}, geom = "text", orientation = "y", color = "darkgray", hjust = 1.1) +
  facet_wrap(~Goal) +
  theme_bw() +
  labs(y = "")
ggsave("figures/age_by_goal.pdf", width = 5, height = 3.5)

