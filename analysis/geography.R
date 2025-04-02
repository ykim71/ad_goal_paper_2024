library(jsonlite)
library(parallel)
library(pbapply)
library(data.table)
library(stringr)
library(dplyr)
library(scales)
library(ggplot2)
library(RColorBrewer)

# Combine relevant vars from multiple datasets
fb22_pred <- fread("../data/fb2022_predicted_goals_bert_all.csv.gz", data.table = F)
load("../data/fb_2022_adid_var_clean.rdata")
fb22 <- left_join(fb22_vars, fb22_pred, by = "ad_id")
rm(list = ls()[ls() != "fb22"])

load("../data/ads_region.rdata")

#----
# Merge into ad-level
fb22_geo <- right_join(fb22, ads_region, by = "ad_id")
fb22_geo$geo_spend <- fb22_geo$spend*fb22_geo$percentage
fb22_geo$ngoals <- fb22_geo %>% select(Donate:Persuade) %>% apply(., 1, sum)
geo_spenddistr <- fb22_geo$geo_spend/fb22_geo$ngoals

spend_by_goal_geo <- fb22_geo %>% select(Donate:Persuade)
spend_by_goal_geo <- spend_by_goal_geo * geo_spenddistr
spend_by_goal_geo[is.na(spend_by_goal_geo)] <- 0
spend_by_goal_geo$state <- fb22_geo$region

goal_by_state <- spend_by_goal_geo %>%
  group_by(state) %>%
  summarise(across(c(Donate:Persuade), sum))

goal_by_state <- data.frame(goal_by_state)
rownames(goal_by_state) <- goal_by_state$state
goal_by_state <- goal_by_state %>% select(-state)
state_totals <- apply(goal_by_state, 1, sum)
state_props <- state_totals/sum(state_totals)
goal_totals <- apply(goal_by_state, 2, sum)


# actual spend, divided by expected spend
goal_by_state2 <- goal_by_state
for(j in 1:ncol(goal_by_state)){
  goal_by_state2[,j] <- goal_by_state[,j]/(state_props*goal_totals[j])
}

state_surprising_goal <- data.frame(state = rownames(goal_by_state2),
                                    goal = names(goal_by_state2)[apply(goal_by_state2, 1, which.max)])

state_surprising_goal$state <- tolower(state_surprising_goal$state)

# Get the U.S. map data
us_map <- map_data("state")

# Merge the state-color dataframe with the map data
us_map_colors <- merge(us_map, state_surprising_goal, by.x = "region", by.y = "state", all.x = TRUE)
us_map_colors <- us_map_colors[!is.na(us_map_colors$goal),]

unique_labels <- unique(us_map_colors$goal)

us_map_colors <- us_map_colors[order(us_map_colors$order),]

# Generate a colorblind-friendly color palette with 9 colors
# display.brewer.all(n=9, type="qual", exact.n=TRUE, colorblindFriendly=T)
# color_palette <- brewer.pal(9, "Paired")
color_palette <- c("#882255","#AA4398","#CC6577","#DDCC77","#88CBED","#45AB99","#107633","#322288","#13283E")

# Create the map
ggplot() +
  geom_polygon(data = us_map_colors, aes(x = long, y = lat, group = group, fill = factor(goal)), color = "white") +
  scale_fill_manual(values = color_palette, name = "Goal") +
  theme_void() +
  theme(legend.position = "bottom")
ggsave("figures/map.pdf", width = 7, height = 4.5)


# One map for each state

# Before that, add Learn which got left out above
unique_labels <- sort(colnames(goal_by_state))

# Account for state population
turnout <- fread("../data/Turnout_1980_2022_v1.0.csv")
turnout <- turnout %>% 
  filter(YEAR == 2022, !STATE %in% c("United States", "District of Columbia")) %>% 
  select(STATE, TOTAL_BALLOTS_COUNTED) %>%
  mutate(TOTAL_BALLOTS_COUNTED = as.numeric(str_remove_all(TOTAL_BALLOTS_COUNTED, ","))) %>%
  mutate(STATE = tolower(STATE)) %>%
  arrange(STATE)

goal_by_state <- goal_by_state[rownames(goal_by_state) != "Other",]
goal_by_state <- goal_by_state/turnout$TOTAL_BALLOTS_COUNTED

# goal_by_state_prop <- goal_by_state/apply(goal_by_state, 1, sum)
# goal_by_state_prop$state <- tolower(rownames(goal_by_state_prop))

goal_by_state$state <- tolower(rownames(goal_by_state))

us_map_colors1 <- merge(us_map, goal_by_state, by.x = "region", by.y = "state", all.x = TRUE)
us_map_colors1 <- us_map_colors1[order(us_map_colors1$order),]

# By absolute spend
for(g in 1:length(unique_labels)) {
  max_val <- max(us_map_colors1[unique_labels[g]], na.rm = T)
  ggplot() +
    geom_polygon(data = us_map_colors1, aes_string(x = "long", y = "lat", group = "group", fill = unique_labels[g]), color = "black") +
    scale_fill_gradient(low = "white", high = color_palette[g], name = unique_labels[g],
                        breaks = c(0, max_val/2, max_val),
                        labels = label_number(scale = 1, accuracy = 0.01)) +
    theme_void() +
    theme(legend.position = "bottom")
  ggsave(paste0("figures/map_", unique_labels[g], ".pdf"), width = 7, height = 4.5)
}

# us_map_colors2 <- merge(us_map, goal_by_state_prop, by.x = "region", by.y = "state", all.x = TRUE)
# us_map_colors2 <- us_map_colors2[order(us_map_colors2$order),]
# 
# # Prop of ad spending to a state by a given goal
# for(g in 1:length(unique_labels)) {
#   ggplot() +
#     geom_polygon(data = us_map_colors2, aes_string(x = "long", y = "lat", group = "group", fill = unique_labels[g]), color = "black") +
#     scale_fill_gradient(low = "white", high = color_palette[g], name = unique_labels[g]) +
#     theme_void() +
#     theme(legend.position = "bottom")
#   ggsave(paste0("figures/map_", unique_labels[g], "_prop.pdf"), width = 7, height = 4.5)
# }

