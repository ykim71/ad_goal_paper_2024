#   party_all    spend
#   <chr>        <dbl>
# 1 DEM       25496238
# 2 OTHER       290401
# 3 REP        6803407

library(dplyr)
library(stringr)

load("../data/fb_2022_adid_var_clean.rdata")
fb22_vars <- fb22_vars %>% select(ad_id, page_id, pd_id, spend)

load("../data/wmpentities_2022.rdata")
wmpent <- wmpent %>% 
  filter((wmp_spontype == "campaign") & (wmp_office %in% c("us house", "us senate"))) %>% 
  select(pd_id, party_all)

fb22 <- right_join(fb22_vars, wmpent, by = "pd_id")

spend_by_party <- fb22 %>%
  group_by(party_all) %>%
  summarise(spend = sum(spend))
spend_by_party
