# Democratic candidates spent 25,344,037 on FB ads in 2022
# compared to only 6,756,382 for Republican candidates

# Required files:
# fb_2022_adid_var1.csv.gz -- download from here: https://drive.google.com/file/d/1y1iiEbijdAMl73oWnU7lzf3-LjZ2SAto/view?usp=drive_link
# wmp_fb_2022_entities_v120122.csv -- download from here: https://github.com/Wesleyan-Media-Project/datasets/raw/main/wmp_entity_files/Facebook/2022/wmp_fb_2022_entities_v120122.csv

library(data.table)
library(dplyr)
library(stringr)

fb22_vars <- fread("../data/fb_2022_adid_var1.csv.gz", data.table = F)
fb22_vars <- fb22_vars %>% select(ad_id, page_id, pd_id, spend)
fb22_vars$spend <- str_split_fixed(fb22_vars$spend, ",", 2) %>%
  apply(., 2, function(x){str_extract(x, "[0-9]+")}) %>%
  apply(., 1, function(x){mean(as.numeric(x))})

wmpent <- fread("../data/wmp_fb_2022_entities_v120122.csv")
wmpent <- wmpent %>% 
  filter((wmp_spontype == "campaign") & (wmp_office %in% c("us house", "us senate"))) %>% 
  select(pd_id, party_all)

fb22 <- left_join(fb22_vars, wmpent, by = "pd_id")

spend_by_party <- fb22 %>%
  group_by(party_all) %>%
  summarise(spend = sum(spend))
spend_by_party
