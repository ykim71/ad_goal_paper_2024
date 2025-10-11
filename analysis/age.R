library(jsonlite)
library(parallel)
library(pbapply)
library(data.table)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)


# Combine relevant vars from multiple datasets
# Predictions on inference set
fb22_pred <- fread("../data/fb2022_predicted_goals_bert_all.csv.gz", data.table = F)
# Main variables
load("../data/fb_2022_adid_var_clean.rdata")
fb22 <- fb22_vars %>% select(ad_id, pd_id, spend)
fb22 <- left_join(fb22, fb22_pred, by = "ad_id")
# Age
load("../data/ads_age.rdata")
fb22_age <- right_join(fb22, ads_age, by = "ad_id")
rm(list = ls()[ls() != "fb22_age"])

#----
# Make the plot
fb22_age_df <- fb22_age %>% select(Donate:Persuade) %>% as.matrix(.) * fb22_age$age
fb22_age_df <- as.data.frame(fb22_age_df)
fb22_age_df$ad_id <- fb22_age$ad_id
fb22_age_df <- fb22_age_df %>% pivot_longer(-ad_id, names_to = "Goal", values_to = "Age")
fb22_age_df <- fb22_age_df[fb22_age_df$Age != 0,]
fb22_age_df <- fb22_age_df[is.na(fb22_age_df$Age) == F,]
fb22_age_df <- left_join(fb22_age_df, fb22_age %>% select(ad_id, spend), by = "ad_id")
wm <- fb22_age_df %>%
  group_by(Goal) %>%
  summarize(wmean = weighted.mean(Age, spend, na.rm = TRUE))

# Figure 3
ggplot(fb22_age_df, aes(Age)) + 
  geom_density(aes(weight = spend), adjust = 2) + 
  geom_vline(data = wm, aes(xintercept = wmean), color = "darkgray") +
  geom_text(
    data = wm,
    aes(x = wmean, y = 0.08, label = round(wmean, 2)),
    color = "darkgray", hjust = 1.1
  ) +
  facet_wrap(~Goal) +
  theme_bw() +
  labs(y = "")
ggsave("figures/age_by_goal.pdf", width = 5, height = 3.5)
