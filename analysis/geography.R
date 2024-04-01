library(jsonlite)
library(parallel)
library(pbapply)
library(data.table)
library(stringr)
library(dplyr)
#library(maps)
library(ggplot2)
library(RColorBrewer)

fb22_pred <- fread("../data/fb2022_predicted_goals_bert_all.csv.gz", data.table = F)

goal_names <- data.frame(wrong = c("DONATE", "CONTACT", "PURCHASE", "GOTV", "EVENT", "POLL", "GATHERINFO", "LEARNMORE", "PRIMARY_PERSUADE"),
                         correct = c("Donate", "Contact", "Purchase", "Vote", "Event", "Poll", "Acquisition", "Learn", "Persuade"))
rename_strings <- function(x) {
  ifelse(x %in% goal_names$wrong, goal_names$correct[match(x, goal_names$wrong)], x)
}

names(fb22_pred) <- rename_strings(names(fb22_pred))


# Combine relevant vars from multiple datasets
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

extract_region <- function(x){
  
  json_str <- str_replace_all(x, "\"\"", "\"")
  df_region <- fromJSON(json_str)
  df_region$percentage <- as.numeric(df_region$percentage)
  df_region$region[!df_region$region %in% state.name] <- "Other"
  df_region <- df_region %>% group_by(region) %>% summarize(percentage = sum(percentage))
  
  return(df_region)
}

# Ad-level metadata
fb22_vars <- fread("../data/fb_2022_adid_var1.csv.gz", data.table = F)
fb22_vars <- fb22_vars[fb22_vars$region_distribution != "",]
ads_region <- pblapply(fb22_vars$region_distribution, extract_region)

add_new_column <- function(df, value) {
  df$ad_id <- value
  return(df)
}

# Long dataframe, % of each region per ad
ads_region <- mapply(add_new_column, ads_region, fb22_vars$ad_id, SIMPLIFY = F)
ads_region <- rbindlist(ads_region)

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
color_palette <- brewer.pal(9, "Paired")

# Create the map
ggplot() +
  geom_polygon(data = us_map_colors, aes(x = long, y = lat, group = group, fill = factor(goal)), color = "white") +
  scale_fill_manual(values = color_palette, name = "Goal") +
  theme_void() +
  theme(legend.position = "bottom")
ggsave("figures/map.pdf", width = 7, height = 4.5)


# One map for each state
# Prop of ad spending to a state by a given goal
goal_by_state_prop <- goal_by_state/apply(goal_by_state, 1, sum)
goal_by_state_prop$state <- tolower(rownames(goal_by_state_prop))

us_map_colors2 <- merge(us_map, goal_by_state_prop, by.x = "region", by.y = "state", all.x = TRUE)
us_map_colors2 <- us_map_colors2[order(us_map_colors2$order),]

for(g in unique_labels) {
  ggplot() +
    geom_polygon(data = us_map_colors2, aes_string(x = "long", y = "lat", group = "group", fill = g), color = "white") +
    scale_fill_gradient(name = g) +
    theme_void() +
    theme(legend.position = "bottom")
  ggsave(paste0("figures/map_", g, ".pdf"), width = 7, height = 4.5)
}

