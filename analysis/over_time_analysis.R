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

# Combine relevant vars from multiple datasets
load("../data/fb_2022_adid_var_clean.rdata")
# Only consider ads with start dates from Sep. 5 onwards
fb22_vars <- fb22_vars[as.Date(fb22_vars$ad_delivery_start_time) >= as.Date("2022-09-05"),]
fb22_vars <- fb22_vars %>% select(ad_id, pd_id, page_name, spend, ad_delivery_start_time)

fb22 <- left_join(fb22_vars, fb22_pred, by = "ad_id")

rm(list = ls()[ls() != "fb22"])

fb22$spenddistr <- fb22$spend / apply(fb22 %>% select(Donate:Persuade), 1, sum)
fb22 <- fb22[is.infinite(fb22$spenddistr) == F,]
fb22[names(fb22) %in% names(fb22 %>% select(Donate:Persuade))] <- fb22 %>% select(Donate:Persuade)*fb22$spenddistr

fb22 <- fb22 %>% 
  group_by(ad_delivery_start_time) %>% 
  summarise(across(Donate:Persuade, \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(-ad_delivery_start_time)

# Remove ads from 11-01 onwards, since there was a moratorium on new ads
fb22 <- fb22[fb22$ad_delivery_start_time < "2022-11-01",]

# Colorblind-friendly colors
color_palette <- c("#882255","#AA4398","#CC6577","#DDCC77","#88CBED","#45AB99","#107633","#322288","#13283E")

# Goals over time
ggplot(fb22, aes(ad_delivery_start_time, value, color = name)) + 
  geom_line() +
  theme_bw() +
  scale_color_manual(values = color_palette, name = "Goal") +
  labs(x = "Ad delivery start time", y = "Daily ad spend on goal (as a proportion)") +
  theme(legend.position = "bottom")
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
  labs(x = "Ad delivery start time", y = "Daily ad spend on goal (as a proportion)") +
  theme(legend.position = "bottom")
#ggsave("figures/goals_over_time_running_avg.pdf", width = 7, height = 4.5)

# Goals on a given day as a proportion of each goal's total
fb22 <- fb22[order(fb22$ad_delivery_start_time, fb22$name),]
goal_total <- fb22 %>% group_by(name) %>% summarise(total = sum(value))
fb22$value_norm <- fb22$value/goal_total$total
ggplot(fb22, aes(ad_delivery_start_time, value_norm, color = name)) + 
  geom_line() +
  theme_bw() +
  scale_color_manual(values = color_palette, name = "Goal") +
  labs(x = "Ad delivery start time", y = "Daily ad spend on goal (as a proportion)") +
  theme(legend.position = "bottom")
ggsave("figures/goals_over_time_norm.pdf", width = 7, height = 4.5)

# 7-day running average of that
fb22 <- fb22 %>%
  arrange(ad_delivery_start_time) %>%
  group_by(name) %>%
  mutate(rolling_average_norm = rollapply(value_norm, width = 7, FUN = mean, align = "right", fill = NA))
ggplot(fb22, aes(ad_delivery_start_time, rolling_average_norm, color = name)) + 
  geom_line() +
  theme_bw() +
  scale_color_manual(values = color_palette, name = "Goal") +
  labs(x = "Ad delivery start time", y = "Daily ad spend on goal (as a proportion)") +
  theme(legend.position = "bottom")
#ggsave("figures/goals_over_time_norm_running_avg.pdf", width = 7, height = 4.5)

# Goals on a given day as a proportion of each goal's total
# With spline smoothing

smooth_spline <- function(df, spar_val = 0.8) {
  # Ensure date is in numeric format for spline fitting
  dates_numeric <- as.numeric(df$ad_delivery_start_time)
  # Fit a smoothing spline
  spline_fit <- smooth.spline(x = dates_numeric, y = df$value_norm, spar = spar_val)  # spar controls smoothness
  # Use original numeric dates for interpolation
  smoothed_values <- predict(spline_fit, x = dates_numeric)$y
  # Return a data frame with the original dates and smoothed values
  return(data.frame(ad_delivery_start_time = df$ad_delivery_start_time, smoothed_values = smoothed_values))
}

fb22_2 <- fb22 %>%
  arrange(ad_delivery_start_time) %>%
  group_by(name) %>%
  group_modify(~smooth_spline(.x, .9)) %>%
  ungroup()

ggplot(fb22_2, aes(ad_delivery_start_time, smoothed_values, color = name)) + 
  geom_line() +
  theme_bw() +
  scale_color_manual(values = color_palette, name = "Goal") +
  labs(x = "Ad delivery start time", y = "Daily ad spend on goal (as a proportion)") +
  theme(legend.position = "bottom") +
  # Manually add labels
  geom_point(aes(x = as.Date("2022-10-25"), y = fb22_2$smoothed_values[fb22_2$name == "Vote" & fb22_2$ad_delivery_start_time == "2022-10-25"]), shape = "x", size = 3, color = "red", show.legend = FALSE) +
  geom_point(aes(x = as.Date("2022-09-05"), y = fb22_2$smoothed_values[fb22_2$name == "Contact" & fb22_2$ad_delivery_start_time == "2022-09-05"]), shape = "x", size = 3, color = "red", show.legend = FALSE) +
  geom_point(aes(x = as.Date("2022-10-10"), y = fb22_2$smoothed_values[fb22_2$name == "Poll" & fb22_2$ad_delivery_start_time == "2022-10-10"]), shape = "x", size = 3, color = "red", show.legend = FALSE) +
  geom_point(aes(x = as.Date("2022-09-16"), y = fb22_2$smoothed_values[fb22_2$name == "Acquisition" & fb22_2$ad_delivery_start_time == "2022-09-16"]), shape = "x", size = 3, color = "red", show.legend = FALSE) +
  geom_text(aes(x = as.Date("2022-10-25"), y = fb22_2$smoothed_values[fb22_2$name == "Vote" & fb22_2$ad_delivery_start_time == "2022-10-25"], label = "Vote"), hjust = -0.2, size = 3, show.legend = FALSE) +
  geom_text(aes(x = as.Date("2022-09-05"), y = fb22_2$smoothed_values[fb22_2$name == "Contact" & fb22_2$ad_delivery_start_time == "2022-09-05"], label = "Contact"), hjust = -0.2, size = 3, show.legend = FALSE) +
  geom_text(aes(x = as.Date("2022-10-10"), y = fb22_2$smoothed_values[fb22_2$name == "Contact" & fb22_2$ad_delivery_start_time == "2022-10-10"], label = "Poll"), vjust = -3, size = 3, show.legend = FALSE) +
  geom_text(aes(x = as.Date("2022-09-16"), y = fb22_2$smoothed_values[fb22_2$name == "Acquisition" & fb22_2$ad_delivery_start_time == "2022-09-16"], label = "Acquisition"), vjust = -.6, hjust = -0.025, size = 3, show.legend = FALSE)
ggsave("figures/goals_over_time_spline_smoothing.pdf", width = 5.5, height = 5)
