# Alphas for 2022

library(haven)
library(dplyr)
library(irr)
library(tidyr)

df <- read_dta("../data/FBEL_wICR_092924.dta")

# Drop problem ads
df <- df %>%
  filter(
    NOCODE != 1 | is.na(NOCODE),
    PROB_SPANISH != 1 | is.na(PROB_SPANISH),
    PROB_VID_NOCODE != 1 | is.na(PROB_VID_NOCODE),
    PROB_VID_PARTIAL != 1 | is.na(PROB_VID_PARTIAL),
    PROB_DONTUSE != 1 | is.na(PROB_DONTUSE),
    PROB_OTH != 1 | is.na(PROB_OTH)
  )

double_vars <- sapply(df, is.double)

df[ , double_vars] <- lapply(df[ , double_vars], function(x) ifelse(is.na(x), 0, x))

df <- df %>%
  rename(ad_id = adid)

df <- df %>%
  filter(CAND1 != "")

df <- df %>%
  arrange(ad_id), desc(RecordedDate), row_number()) %>%
  group_by(ad_id) %>%
  mutate(dup = if (n() == 1) 0 else row_number()) %>%
  ungroup() %>%
  filter(dup == 1 | dup == 2)



df <- df %>% select(adid,coder,DONATE:PRIMARY_PERSUADE)
df$PRIMARY_PERSUADE <- abs(df$PRIMARY_PERSUADE-2)
# Only keep ads that are double-coded
double_coded <- df$adid[duplicated(df$adid)]
df <- df[df$adid %in% double_coded,]
# Drop ads that weren't actually coded
# df <- drop_na(df)
# Recode NAs to 0 (NA usually means primary persuade = 1)
df[is.na(df)] <- 0

df$coder[duplicated(df$adid)] <- 100
df$coder[df$coder != 100] <- 200

# Rename goal names
goal_names <- data.frame(wrong = c("DONATE", "CONTACT", "PURCHASE", "GOTV", "EVENT", "POLL", "GATHERINFO", "LEARNMORE", "PRIMARY_PERSUADE"),
                         correct = c("Donate", "Contact", "Purchase", "Vote", "Event", "Poll", "Acquisition", "Learn", "Persuade"))
rename_strings <- function(x) {
  ifelse(x %in% goal_names$wrong, goal_names$correct[match(x, goal_names$wrong)], x)
}
names(df) <- rename_strings(names(df))


df <- read_dta("../data/FBEL_onlydoublecoded_040225.dta")

# Shape into format required by irr package
nmm <- df %>% pivot_wider(id_cols = coder, names_from = ad_id, values_from = Donate:Persuade)

alphas <- data.frame(Goals = goal_names$correct, Alpha = NA)
for(i in 1:nrow(goal_names)){
  mat_kripp <- nmm %>% select(starts_with(goal_names$correct[i])) %>% as.matrix()
  alphas$Alpha[i] <- round(kripp.alpha(mat_kripp)$value, 2)
}
alphas <- alphas[order(alphas$Goals),]
alphas
