library(jsonlite)
library(parallel)
library(pbapply)
library(data.table)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)

fb22_pred <- fread("../data/fb2022_predicted_goals_bert_all.csv.gz", data.table = F)

goal_names <- data.frame(wrong = c("DONATE", "CONTACT", "PURCHASE", "GOTV", "EVENT", "POLL", "GATHERINFO", "LEARNMORE", "PRIMARY_PERSUADE"),
                         correct = c("Donate", "Contact", "Purchase", "Vote", "Event", "Poll", "Acquisition", "Learn", "Persuade"))
rename_strings <- function(x) {
  ifelse(x %in% goal_names$wrong, goal_names$correct[match(x, goal_names$wrong)], x)
}

names(fb22_pred) <- rename_strings(names(fb22_pred))


# Combine relevant vars from multiple datasets
fb22_vars <- fread("../data/fb_2022_adid_var1.csv.gz", data.table = F)
# Only consider ads with start dates from Sep. 5 onwards
fb22_vars <- fb22_vars[as.Date(fb22_vars$ad_delivery_start_time) >= as.Date("2022-09-05"),]
fb22_vars <- fb22_vars %>% select(ad_id, page_id, pd_id, spend, ad_delivery_start_time)
fb22_vars$spend <- str_split_fixed(fb22_vars$spend, ",", 2) %>%
  apply(., 2, function(x){str_extract(x, "[0-9]+")}) %>%
  apply(., 1, function(x){mean(as.numeric(x))})

fb22_vars2 <- fread("../../fb_2022/fb_2022_adid_text.csv.gz", data.table = F)
fb22_vars2 <- fb22_vars2 %>% select(ad_id, page_name)

fb22 <- left_join(fb22_vars, fb22_vars2, by = "ad_id")
fb22 <- left_join(fb22, fb22_pred, by = "ad_id")

rm(list = ls()[ls() != "fb22"])

fb22$spenddistr <- fb22$spend / apply(fb22 %>% select(Donate:Persuade), 1, sum)
fb22 <- fb22[is.infinite(fb22$spenddistr) == F,]
fb22[names(fb22) %in% names(fb22 %>% select(Donate:Persuade))] <- fb22 %>% select(Donate:Persuade)*fb22$spenddistr

fb22 <- fb22 %>% 
  group_by(ad_delivery_start_time) %>% 
  summarise(across(Donate:Persuade, \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(-ad_delivery_start_time)


# Colorblind-friendly colors
color_palette <- c("#882255","#AA4398","#CC6577","#DDCC77","#88CBED","#45AB99","#107633","#322288","#13283E")

# Goals over time
ggplot(fb22, aes(ad_delivery_start_time, value, color = name)) + 
  geom_line() +
  theme_bw() +
  scale_color_manual(values = color_palette, name = "Goal") +
  labs(x = "Ad delivery start time", y = "Daily ad spend on goal (as a proportion)")
ggsave("figures/goals_over_time.pdf", width = 7, height = 4.5)


# 7-day running average
fb22 <- fb22 %>%
  arrange(ad_delivery_start_time) %>%
  group_by(name) %>%
  mutate(rolling_average = rollapply(value, width = 7, FUN = mean, align = "right", fill = NA))

ggplot(fb22, aes(ad_delivery_start_time, rolling_average, color = name)) + 
  geom_line() +
  theme_bw() +
  scale_color_manual(values = color_palette, name = "Goal") +
  labs(x = "Ad delivery start time", y = "Daily ad spend on goal (as a proportion)")
ggsave("figures/goals_over_time_running_avg.pdf", width = 7, height = 4.5)

# Goals over time normalized and running average
fb22 <- fb22[order(fb22$ad_delivery_start_time, fb22$name),]
goal_total <- fb22 %>% group_by(name) %>% summarise(total = sum(value))
fb22$value_norm <- fb22$value/goal_total$total

fb22 <- fb22 %>%
  arrange(ad_delivery_start_time) %>%
  group_by(name) %>%
  mutate(rolling_average_norm = rollapply(value_norm, width = 7, FUN = mean, align = "right", fill = NA))
ggplot(fb22, aes(ad_delivery_start_time, rolling_average_norm, color = name)) + 
  geom_line() +
  theme_bw() +
  scale_color_manual(values = color_palette, name = "Goal") +
  labs(x = "Ad delivery start time", y = "Daily ad spend on goal (as a proportion)")
ggsave("figures/goals_over_time_norm_running_avg.pdf", width = 7, height = 4.5)

