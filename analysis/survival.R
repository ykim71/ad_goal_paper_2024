library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(survival)
library(ggplot2)
library(RColorBrewer)

var1 <- fread("../data/fb_2022_adid_var1.csv.gz", data.table = F)

# Count number of ads running from before 2022-09-05 (mentioned in footnote)
length(which(as.Date(var1$ad_delivery_start_time) < as.Date("2022-09-05")))

# Treat ads that end after 2022-11-08, or for which we don't know the stop time as right-censored on that day
var1$ad_delivery_stop_time[as.Date(var1$ad_delivery_stop_time) > as.Date("2022-11-08")] <- "2022-11-08"
var1$ad_delivery_stop_time[is.na(var1$ad_delivery_stop_time)] <- "2022-11-08"


bert <- fread("../data/fb2022_predicted_goals_bert_all.csv.gz", data.table = F)
goal_names <- data.frame(wrong = c("DONATE", "CONTACT", "PURCHASE", "GOTV", "EVENT", "POLL", "GATHERINFO", "LEARNMORE", "PRIMARY_PERSUADE"),
                         correct = c("Donate", "Contact", "Purchase", "Vote", "Event", "Poll", "Acquisition", "Learn", "Persuade"))
rename_strings <- function(x) {
  ifelse(x %in% goal_names$wrong, goal_names$correct[match(x, goal_names$wrong)], x)
}
names(bert) <- rename_strings(names(bert))

var1$duration <- (var1$ad_delivery_stop_time - var1$ad_delivery_start_time)+1
bert <- bert[is.na(var1$duration) == F,]
var1 <- var1[is.na(var1$duration) == F,]


goals <- names(bert)[2:ncol(bert)]
dfs <- list()
for(goal in goals){
  
  goal_duration <- var1$duration[bert[goal] == 1]
  goal_surv <- Surv(goal_duration)
  goal_survfit <- survfit(goal_surv ~ 1)
  goal_df <- data.frame(time = goal_survfit$time, surv = goal_survfit$surv)
  goal_df_t <- data.frame(time = 1:1000)
  goal_df_t <- left_join(goal_df_t, goal_df, by = "time")
  goal_df_t <- goal_df_t[1:which.min(goal_df_t$surv),]
  goal_df_t$surv <- zoo::na.locf(goal_df_t$surv) # fill with value above
  goal_df_t$goal <- goal
  dfs <- c(dfs, list(goal_df_t))
  
}

df_goals <- rbindlist(dfs)
names(df_goals)[3] <- "Goal"

# Generate a colorblind-friendly color palette with 9 colors
# display.brewer.all(n=9, type="qual", exact.n=TRUE, colorblindFriendly=T)
# color_palette <- brewer.pal(9, "Paired")
color_palette <- c("#882255","#AA4398","#CC6577","#DDCC77","#88CBED","#45AB99","#107633","#322288","#13283E")

ggplot(df_goals, aes(time, surv, color = Goal)) + 
  geom_step() + 
  ylim(0,1) + 
  xlim(0,200) + # cut off after 200 days
  scale_x_continuous(trans='log10', limits = c(1,200), breaks = c(1:7, 14, 21, 28, 100, 200)) +
  #geom_vline(xintercept = 7, lty = 2, alpha = 0.5) + # dashed line at 1 week
  #geom_vline(xintercept = 30, lty = 2, alpha = 0.5) + # dashed line at 1 month
  theme_bw() +
  labs(x = "Ad runtime (days)", y = "Probability that ad is still running") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = color_palette) +
  geom_point(aes(x = 7, y = df_goals$surv[df_goals$Goal == "Event" & df_goals$time == 7]), shape = "x", size = 3, color = "red", show.legend = FALSE) +
  geom_point(aes(x = 100, y = df_goals$surv[df_goals$Goal == "Purchase" & df_goals$time == 100]), shape = "x", size = 3, color = "red", show.legend = FALSE) +
  geom_text(aes(x = 7, y = df_goals$surv[df_goals$Goal == "Event" & df_goals$time == 7], label = "Event"), vjust = 2, size = 3, color = "black", show.legend = FALSE) +
  geom_text(aes(x = 100, y = df_goals$surv[df_goals$Goal == "Purchase" & df_goals$time == 100], label = "Purchase"), vjust = -1, size = 3, color = "black", show.legend = FALSE)
ggsave("figures/goal_survival.pdf", width = 5.5, height = 5)


# Survival probabilities mentioned in the paper
df_goals[df_goals$time == 7,]
df_goals[df_goals$time == 30,]
df_goals[df_goals$time == 200,]
