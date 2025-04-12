library(data.table)
library(stringr)
library(stringi)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(xtable)
library(maps)
library(RColorBrewer)


fb22_pred <- fread("../data/fb2022_predicted_goals_bert_all.csv.gz", data.table = F)

# Combine relevant vars from multiple datasets
load("../data/fb_2022_adid_var_clean.rdata")
fb22_vars <- fb22_vars %>% select(ad_id, pd_id, page_name, spend)

fb22 <- left_join(fb22_vars, fb22_pred, by = "ad_id")

# Add party & incumbency
load("../data/wmpentities_2022.rdata")
wmpent <- wmpent %>% 
  filter((wmp_spontype == "campaign") & (wmp_office %in% c("us house", "us senate"))) %>% 
  select(pd_id, wmp_office, party_all, incumbency)
wmpent <- wmpent %>% filter(incumbency != "")
fb22 <- left_join(fb22, wmpent, by = "pd_id")

rm(list = ls()[ls() != "fb22"])

#----

# Sponsors who run Vote ads without calling for the election of a specific candidates
# Mostly seem to be nonpartisan groups, but a few Dem ads are in there
non_persuade_sponsors <- fb22$page_name[(fb22$Vote == 1 & fb22$Persuade == 0)]
non_persuade_sponsors <- data.frame(table(non_persuade_sponsors))
names(non_persuade_sponsors) <- c("Sponsor", "Ads")
non_persuade_sponsors <- non_persuade_sponsors[order(non_persuade_sponsors$Ads, decreasing = T),]
non_persuade_sponsors <- non_persuade_sponsors[1:10,]
print(xtable(non_persuade_sponsors, 
             caption = "Sponsors who run ads that are Vote without also being Persuade.",
             label = "tab:non_persuade_vote_sponsors"),
      "latex",
      "tables/non_persuade_vote_sponsors.tex",
      include.rownames = F,
      comment = FALSE)

# sponsor % of spend by goal
# purchase
fb22_purchase <- fb22[fb22$Purchase == 1,]
purchase_spenders <- aggregate(fb22_purchase$spend, by = list(fb22_purchase$page_name), sum)
names(purchase_spenders) <- c("page_id", "spend")
purchase_spenders$percent <- purchase_spenders$spend/sum(purchase_spenders$spend)
purchase_spenders <- purchase_spenders[order(purchase_spenders$percent, decreasing = T),]
purchase_spenders$percent <- round(purchase_spenders$percent, 2)
names(purchase_spenders) <- c("Page", "Spend", "Prop. Spend")
purchase_spenders <- purchase_spenders[1:10,]
purchase_spenders$Spend <- as.character(round(purchase_spenders$Spend))
print(xtable(purchase_spenders, 
             caption = "Purchase ad sponsors, sorted by their spend.",
             label = "tab:purchase_sponsors"),
      "latex",
      "tables/purchase_sponsors.tex",
      include.rownames = F,
      comment = FALSE)

# % of sponsor's spend by goal
# If an ad has multiple goals, spread the spend between them equally
ngoals <- fb22 %>% select(Donate:Persuade) %>% apply(., 1, sum)
spenddistr <- fb22$spend/ngoals
spend_by_goal <- fb22 %>% select(Donate:Persuade)
spend_by_goal <- spend_by_goal * spenddistr
spend_by_goal[is.na(spend_by_goal)] <- 0

fb22_spend_by_goal <- fb22
fb22_spend_by_goal[,names(fb22_spend_by_goal %>% select(Donate:Persuade))] <- spend_by_goal
goal_by_sponsor <- fb22_spend_by_goal %>%
  group_by(page_name) %>%
  summarise(across(c(Donate:Persuade), sum))

topspenders <- data.frame(spend = goal_by_sponsor %>% select(Donate:Persuade) %>% apply(., 1, sum),
                          sponsor = goal_by_sponsor$page_name)
topspenders <- topspenders[order(topspenders$spend, decreasing = T),]
goal_by_sponsor <- goal_by_sponsor %>% pivot_longer(-page_name)
topsponsors <- topspenders$sponsor[1:15]
goal_by_topsponsor <- goal_by_sponsor[goal_by_sponsor$page_name %in% topsponsors,]
names(goal_by_topsponsor) <- c("Page Name", "Goal", "Spend")
goal_by_topsponsor$`Page Name` <- factor(goal_by_topsponsor$`Page Name`, rev(topsponsors))
levels(goal_by_topsponsor$`Page Name`)[levels(goal_by_topsponsor$`Page Name`) == "NO on Prop 29 - Stop Yet Another Dangerous Dialysis Proposition"] <- "NO on Prop 29"

# Generate a colorblind-friendly color palette with 9 colors
# display.brewer.all(n=9, type="qual", exact.n=TRUE, colorblindFriendly=T)
# color_palette <- brewer.pal(9, "Paired")
color_palette <- c("#882255","#AA4398","#CC6577","#DDCC77","#88CBED","#45AB99","#107633","#322288","#13283E")

ggplot(goal_by_topsponsor, aes(Spend, `Page Name`)) + geom_col(aes(fill = Goal)) + theme_bw() + theme(legend.position = "bottom") + ylab("") + scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) + scale_fill_manual(values = color_palette)
ggsave("figures/goal_by_sponsor.pdf", height = 4, width = 7)

# Top sponsors by goal
goals <- unique(goal_by_sponsor$name)
for(i in 1:length(goals)){
  top_sponsors_by_goal <- goal_by_sponsor[goal_by_sponsor$name == goals[i],]
  top_sponsors_by_goal <- top_sponsors_by_goal[order(top_sponsors_by_goal$value, decreasing = T),]
  top_sponsors_by_goal <- top_sponsors_by_goal[1:15,]
  top_sponsors_by_goal <- top_sponsors_by_goal[top_sponsors_by_goal$value>0,]
  names(top_sponsors_by_goal) <- c("Page Name", "Goal", "Spend")
  top_sponsors_by_goal$`Page Name` <- factor(top_sponsors_by_goal$`Page Name`, rev(top_sponsors_by_goal$`Page Name`))
  ggplot(top_sponsors_by_goal, aes(Spend, `Page Name`)) + geom_col() + theme(legend.position = "bottom") + labs(title = goals[i], y = "") + scale_x_continuous(labels = label_number(scale_cut = cut_short_scale()))
  ggsave(paste0("figures/top_sponsors_by_goal_", goals[i],".pdf"), height = 5, width = 7)
}


# Do any of the top 100 spenders spread their spend more evenly between goals?
# Not really, there are none with low gini or low variance within the sponsor
top100sponsors <- topspenders$sponsor[1:100]
goal_by_top100sponsor <- goal_by_sponsor[goal_by_sponsor$page_name %in% top100sponsors,]
goal_gini <- aggregate(goal_by_top100sponsor$value, by = list(goal_by_top100sponsor$page_name), DescTools::Gini)
goal_var <- aggregate(goal_by_top100sponsor$value, by = list(goal_by_top100sponsor$page_name), var)

#----
# The reason the following doesn't match topspenders
# is because some ads have no goals, and so they get excluded from topspenders
# topspenders2 <- aggregate(fb22$spend, by = list(fb22$page_name), sum)

#----
# By candidate

fb22_spend_by_goal_cand <- fb22_spend_by_goal %>% filter(is.na(wmp_office) == F)

# Number of candidates, mentioned in paper
length(unique(fb22_spend_by_goal_cand$page_name))

goal_by_party <- fb22_spend_by_goal_cand %>%
  group_by(party_all) %>%
  summarise(across(c(Donate:Persuade), sum)) %>%
  filter(party_all != "OTHER")

goal_by_party <- pivot_longer(goal_by_party, -party_all)

names(goal_by_party) <- c("Party", "Goal", "Spend")
goal_by_party$Party <- goal_by_party$Party %>% case_match("DEM" ~ "Democrat", "REP" ~ "Republican")
goal_by_party$Goal <- factor(goal_by_party$Goal, levels = sort(unique(goal_by_party$Goal)))

# Create a bar chart
ggplot(goal_by_party, aes(x = Goal, y = Spend, fill = Party)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  scale_fill_manual(values = c("Democrat" = "#377eb8", "Republican" = "#e41a1c"))
ggsave("figures/goal_by_party.pdf", width = 6, height = 4)

# As a proportion, to account for the Democratic advantage
goal_by_party_prop <- fb22_spend_by_goal_cand %>%
  group_by(party_all) %>%
  summarise(across(c(Donate:Persuade), sum)) %>%
  filter(party_all != "OTHER") %>%
  mutate(Total = rowSums(across(where(is.numeric))))
goal_by_party_prop <- goal_by_party_prop %>%
  mutate(across(where(is.numeric), ~ .x / Total)) %>%
  select(-Total)

goal_by_party_prop <- pivot_longer(goal_by_party_prop, -party_all)

names(goal_by_party_prop) <- c("Party", "Goal", "Spend")
goal_by_party_prop$Party <- goal_by_party_prop$Party %>% case_match("DEM" ~ "Democrat", "REP" ~ "Republican")
goal_by_party_prop$Goal <- factor(goal_by_party_prop$Goal, levels = sort(unique(goal_by_party_prop$Goal)))

# Create a bar chart
ggplot(goal_by_party_prop, aes(x = Goal, y = Spend, fill = Party)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_fill_manual(values = c("Democrat" = "#377eb8", "Republican" = "#e41a1c")) +
  xlab(NULL) +
  ylab("Spend (proportion of total party spend)") +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
    axis.title.y = element_text(size = 8),
    legend.position = "bottom",
    legend.margin = margin(t = -10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.key.size = unit(0.3, "cm")
  )
ggsave("figures/goal_by_party_prop.pdf", width = 6, height = 2.5)

#----
# By incumbency

goal_by_incumbency <- fb22_spend_by_goal_cand %>%
  group_by(incumbency) %>%
  summarise(across(c(Donate:Persuade), sum))

goal_by_incumbency <- pivot_longer(goal_by_incumbency, -incumbency)

names(goal_by_incumbency) <- c("Incumbency", "Goal", "Spend")
goal_by_incumbency$Incumbency <- str_to_title(goal_by_incumbency$Incumbency)
goal_by_incumbency$Goal <- factor(goal_by_incumbency$Goal, levels = sort(unique(goal_by_incumbency$Goal)))

# Create a bar chart
ggplot(goal_by_incumbency, aes(x = Goal, y = Spend, fill = Incumbency)) +
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
ggsave("figures/goal_by_incumbency_mean.pdf", width = 6, height = 2.5)

# As a proportion of candidate ad budget

goal_by_incumbency_prop <- fb22_spend_by_goal_cand %>%
  group_by(pd_id) %>%
  summarise(across(c(Donate:Persuade), sum))
goal_by_incumbency_prop[,2:ncol(goal_by_incumbency_prop)] <- goal_by_incumbency_prop[,2:ncol(goal_by_incumbency_prop)]/apply(goal_by_incumbency_prop[,2:ncol(goal_by_incumbency_prop)], 1, sum)
goal_by_incumbency_prop <- goal_by_incumbency_prop[is.na(goal_by_incumbency_prop$Donate) == F,]
goal_by_incumbency_prop <- left_join(goal_by_incumbency_prop, fb22_spend_by_goal_cand %>% select(pd_id, incumbency))
goal_by_incumbency_prop <- 
  goal_by_incumbency_prop %>%
  group_by(incumbency) %>%
  summarise(across(c(Donate:Persuade), mean))

goal_by_incumbency_prop <- pivot_longer(goal_by_incumbency_prop, -incumbency)

names(goal_by_incumbency_prop) <- c("Incumbency", "Goal", "Spend")
goal_by_incumbency_prop$Incumbency <- str_to_title(goal_by_incumbency_prop$Incumbency)
goal_by_incumbency_prop$Goal <- factor(goal_by_incumbency_prop$Goal, levels = sort(unique(goal_by_incumbency_prop$Goal)))

# Create a bar chart
ggplot(goal_by_incumbency_prop, aes(x = Goal, y = Spend, fill = Incumbency)) +
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
ggsave("figures/goal_by_incumbency_prop.pdf", width = 6, height = 2.5)
