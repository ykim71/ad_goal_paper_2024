# Democratic candidates spent 25,344,037 on FB ads in 2022
# compared to only 6,756,382 for Republican candidates

# Required files:
# fb_2022_adid_var1.csv.gz -- download from here: https://drive.google.com/file/d/1y1iiEbijdAMl73oWnU7lzf3-LjZ2SAto/view?usp=drive_link
# wmp_fb_2022_entities_v120122.csv -- download from here: https://github.com/Wesleyan-Media-Project/datasets/raw/main/wmp_entity_files/Facebook/2022/wmp_fb_2022_entities_v120122.csv

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
