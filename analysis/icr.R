library(haven)
library(dplyr)
library(irr)
library(tidyr)

#---
# Alphas for 2020
# Load 2020 data and rename columns
df <- read_dta("../data/FBEL_1.0_2020_doublecoded_040225.dta")
df <- df %>% select(ad_id,coder,DONATE:PRIMARY_PERSUADE)
goal_names <- tibble(wrong = c("DONATE", "CONTACT", "PURCHASE", "GOTV", "EVENT", "POLL", "GATHERINFO", "LEARNMORE", "PRIMARY_PERSUADE"),
                     correct = c("Donate", "Contact", "Purchase", "Vote", "Event", "Poll", "Acquisition", "Learn", "Persuade"))
df <- df %>%  rename_with(.fn = ~ goal_names$correct[match(.x, goal_names$wrong)],
                          .cols = any_of(goal_names$wrong))

# Shape into format required by irr package
nmm <- df %>% pivot_wider(id_cols = coder, names_from = ad_id, values_from = Donate:Persuade)

alphas <- data.frame(Goals = goal_names$correct, Alpha = NA)
for(i in 1:nrow(goal_names)){
  mat_kripp <- nmm %>% select(starts_with(goal_names$correct[i])) %>% as.matrix()
  alphas$Alpha[i] <- round(kripp.alpha(mat_kripp)$value, 2)
}
alphas2020 <- alphas[order(alphas$Goals),]
alphas2020


#---
# Alphas for 2022
# Load 2022 data and rename columns
df <- read_dta("../data/FBEL_doublecoded2_040225.dta")
df <- df %>% select(ad_id,coder,DONATE:PRIMARY_PERSUADE)
df <- df %>%  rename_with(.fn = ~ goal_names$correct[match(.x, goal_names$wrong)],
                          .cols = any_of(goal_names$wrong))

# Shape into format required by irr package
nmm <- df %>% pivot_wider(id_cols = coder, names_from = ad_id, values_from = Donate:Persuade)

alphas <- data.frame(Goals = goal_names$correct, Alpha = NA)
for(i in 1:nrow(goal_names)){
  mat_kripp <- nmm %>% select(starts_with(goal_names$correct[i])) %>% as.matrix()
  alphas$Alpha[i] <- round(kripp.alpha(mat_kripp)$value, 2)
}
alphas2022 <- alphas[order(alphas$Goals),]
alphas2022
