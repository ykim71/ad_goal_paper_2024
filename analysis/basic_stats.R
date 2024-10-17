library(data.table)
library(stringr)
library(stringi)
library(dplyr)
library(tidyr)
library(xtable)
library(ggplot2)
library(scales)


fb22_pred <- fread("../data/fb2022_predicted_goals_bert_all.csv.gz", data.table = F)
goal_names <- data.frame(wrong = c("DONATE", "CONTACT", "PURCHASE", "GOTV", "EVENT", "POLL", "GATHERINFO", "LEARNMORE", "PRIMARY_PERSUADE"),
                         correct = c("Donate", "Contact", "Purchase", "Vote", "Event", "Poll", "Acquisition", "Learn", "Persuade"))
rename_strings <- function(x) {
  ifelse(x %in% goal_names$wrong, goal_names$correct[match(x, goal_names$wrong)], x)
}
names(fb22_pred) <- rename_strings(names(fb22_pred))
fb22_pred <- fb22_pred %>% select("ad_id", "Acquisition", "Contact", "Donate", "Event", "Learn", "Persuade", "Poll", "Purchase", "Vote")

fb22_pred$`No goals` <- fb22_pred %>% select(-ad_id) %>% apply(., 1, function(x){all(x == 0)}) %>% as.numeric()


# Goal count
# Ignore warning
goal_count = fb22_pred %>%
  summarise(across(c(Acquisition:`No goals`), list(mean = mean, sum = sum))) %>%
  pivot_longer(cols = everything(), names_to = c(".value", "variable"), names_sep = "_") %>%
  t() %>%
  as.data.frame()
names(goal_count) <- c("Proportion", "Sum")
goal_count <- goal_count[-1,]
goal_count$Proportion <- round(as.numeric(goal_count$Proportion), 2)
goal_count$Sum <- as.numeric(goal_count$Sum)


# Goal spend
fb22_vars <- fread("../data/fb_2022_adid_var1.csv.gz", data.table = F)
fb22_vars <- fb22_vars %>% select(ad_id, page_id, pd_id, spend)
fb22_vars$spend <- str_split_fixed(fb22_vars$spend, ",", 2) %>%
  apply(., 2, function(x){str_extract(x, "[0-9]+")}) %>%
  apply(., 1, function(x){mean(as.numeric(x))})
fb22 <- left_join(fb22_pred, fb22_vars, by = "ad_id")

ngoals <- fb22 %>% select(Acquisition:`No goals`) %>% apply(., 1, sum)
spenddistr <- fb22$spend/ngoals
spend_by_goal <- fb22 %>% select(Acquisition:`No goals`)
spend_by_goal <- spend_by_goal * spenddistr
spend_by_goal[is.na(spend_by_goal)] <- 0

goal_total_spend <- apply(spend_by_goal, 2, sum)
goal_total_spend = spend_by_goal %>%
  summarise(across(c(Acquisition:`No goals`), list(sum = sum)))

goal_total_spend <- t(goal_total_spend) %>% as.data.frame(.)
names(goal_total_spend) <- "Spend"
goal_total_spend$Proportion <- round(goal_total_spend$Spend/sum(goal_total_spend), 2)


# Combine everything and save
final_table <- cbind(goal_count, goal_total_spend)
names(final_table) <- c("Prop. Ads", "Ad Count", "Spend", "Prop. Spend")
final_table <- final_table %>% select(`Ad Count`, `Prop. Ads`, Spend, `Prop. Spend`)
final_table$`Prop. Ads` <- as.character(final_table$`Prop. Ads`)
final_table$`Prop. Spend` <- as.character(final_table$`Prop. Spend`)
rownames(final_table)[rownames(final_table) == "Nogoals"] <- "No goals"
xt <- xtable(final_table, 
             caption = "Ad count and spend, by goal.",
             label = "tab:basic_stats", 
             digits = 0)
print(xt,
      "latex",
      "tables/basic_stats.tex",
      include.rownames = T,
      format.args = list(big.mark = ",", decimal.mark = "."))

# Candidates vs. outside groups

wmpent <- fread("../data/wmp_fb_2022_entities_v120122.csv", data.table = F)
wmpent <- wmpent %>% select(pd_id, wmp_spontype)

fb22 <- left_join(fb22, wmpent, by = "pd_id")

spend_by_goal$wmp_spontype <- fb22$wmp_spontype

table(spend_by_goal$wmp_spontype)
sum(table(spend_by_goal$wmp_spontype))

spend_by_goal <- spend_by_goal %>% filter(wmp_spontype %in% c('campaign', 'group', 'party', 'party national'))
spend_by_goal$wmp_spontype[spend_by_goal$wmp_spontype == "party national"] <- "party"
spontype_goals <- spend_by_goal %>% group_by(wmp_spontype) %>% summarise(across(.cols = Acquisition:`No goals`, .fns = sum, na.rm = TRUE))
# Omit no goals since we don't discuss it in the paper
spontype_goals <- spontype_goals %>% select(-`No goals`)
spontype_goals <- pivot_longer(spontype_goals, -wmp_spontype)

names(spontype_goals) <- c("Sponsor type", "Goal", "Spend")
spontype_goals$`Sponsor type` <- spontype_goals$`Sponsor type` %>% case_match("campaign" ~ "Campaign", "group" ~ "Group", "party" ~ "Party")
spontype_goals$Goal <- factor(spontype_goals$Goal, levels = c(names(fb22_pred)[-1]))

# Create a bar chart
ggplot(spontype_goals, aes(x = Goal, y = Spend, fill = `Sponsor type`)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  xlab(NULL) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
    legend.position = "bottom",
    legend.margin = margin(t = -10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.key.size = unit(0.3, "cm")
  )
ggsave("figures/goal_by_spontype.pdf", width = 6, height = 2.5)


# FB vs Insta
fb22_vars <- fread("../data/fb_2022_adid_var1.csv.gz", data.table = F)
fb22_vars <- fb22_vars %>% select(ad_id, page_id, pd_id, spend, publisher_platforms)
fb22_vars$spend <- str_split_fixed(fb22_vars$spend, ",", 2) %>%
  apply(., 2, function(x){str_extract(x, "[0-9]+")}) %>%
  apply(., 1, function(x){mean(as.numeric(x))})
fb22 <- left_join(fb22_pred, fb22_vars, by = "ad_id")

ngoals <- fb22 %>% select(Acquisition:`No goals`) %>% apply(., 1, sum)
spenddistr <- fb22$spend/ngoals
spend_by_goal <- fb22 %>% select(Acquisition:`No goals`)
spend_by_goal <- spend_by_goal * spenddistr
spend_by_goal[is.na(spend_by_goal)] <- 0
spend_by_goal$publisher_platforms <- fb22$publisher_platforms
spend_by_goal <- spend_by_goal %>% filter(publisher_platforms %in% c('facebook', 'instagram'))
platform_goals <- spend_by_goal %>% group_by(publisher_platforms) %>% summarise(across(.cols = Acquisition:`No goals`, .fns = sum, na.rm = TRUE))
# Omit no goals since we don't discuss it in the paper
platform_goals <- platform_goals %>% select(-`No goals`)
platform_goals <- pivot_longer(platform_goals, -publisher_platforms)

names(platform_goals) <- c("Platform", "Goal", "Spend")
platform_goals$Platform <- platform_goals$Platform %>% case_match("facebook" ~ "Facebook", "instagram" ~ "Instagram")
platform_goals$Goal <- factor(platform_goals$Goal, levels = c(names(fb22_pred)[-1]))

# Create a bar chart
ggplot(platform_goals, aes(x = Goal, y = Spend, fill = Platform)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  xlab(NULL) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
    legend.position = "bottom",
    legend.margin = margin(t = -10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.key.size = unit(0.3, "cm")
  )
ggsave("figures/goal_by_platform.pdf", width = 6, height = 2.5)

