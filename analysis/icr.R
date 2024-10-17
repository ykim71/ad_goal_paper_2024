library(haven)
library(dplyr)
library(irr)
library(tidyr)

df <- read_dta("../data/FBEL_wICR_092924.dta")
double_coded <- df$adid[duplicated(df$adid)]
df <- df[df$adid %in% double_coded,]
df <- df %>% select(adid,coder,DONATE:PRIMARY_PERSUADE)
df$PRIMARY_PERSUADE <- abs(df$PRIMARY_PERSUADE-2)
df[is.na(df)] <- 0

goal_names <- data.frame(wrong = c("DONATE", "CONTACT", "PURCHASE", "GOTV", "EVENT", "POLL", "GATHERINFO", "LEARNMORE", "PRIMARY_PERSUADE"),
                         correct = c("Donate", "Contact", "Purchase", "Vote", "Event", "Poll", "Acquisition", "Learn", "Persuade"))
rename_strings <- function(x) {
  ifelse(x %in% goal_names$wrong, goal_names$correct[match(x, goal_names$wrong)], x)
}
names(df) <- rename_strings(names(df))
nmm <- df %>% pivot_wider(id_cols = coder, names_from = adid, values_from = Donate:Persuade)

alphas <- data.frame(Goals = goal_names$correct, Alpha = NA)
for(i in 1:nrow(goal_names)){
  mat_kripp <- nmm %>% select(starts_with(goal_names$correct[i])) %>% as.matrix()
  alphas$Alpha[i] <- round(kripp.alpha(mat_kripp)$value, 2)
}
alphas <- alphas[order(alphas$Goals),]

