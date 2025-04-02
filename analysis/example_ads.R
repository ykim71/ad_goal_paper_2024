# Unfortunately this doesn't replicate - it now produces partially different
# ads to when we ran it originally

library(jsonlite)
library(parallel)
library(pbapply)
library(data.table)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xtable)


# Goal clf predictions
fb22_pred <- fread("../data/fb2022_predicted_goals_bert_all.csv.gz", data.table = F)

# Merge in text
fb22_text <- fread("../data/fb2022_inference_separate_fields.csv.gz", data.table = F)
fb22_text <- fb22_text %>% select(ad_id, page_name, ad_creative_body)

fb22_text <- left_join(fb22_pred, fb22_text, by = "ad_id")
fb22_text <- fb22_text[fb22_text$ad_creative_body != "",]
fb22_text <- fb22_text[!str_detect(fb22_text$ad_creative_body, "Please read this"),]

# Merge in pdid and spend
load("../data/fb_2022_adid_var_clean.rdata")

# Merge in and restrict to wmp spontype campaign
load("../data/wmpentities_2022.rdata")
wmpent <- wmpent %>% 
  filter((wmp_spontype == "campaign") & (wmp_office %in% c("us house", "us senate"))) %>%
  select(pd_id)

fb22 <- inner_join(fb22_vars, wmpent, by = "pd_id") %>% select(-page_name)
fb22_text <- inner_join(fb22_text, fb22, by = "ad_id")

# Restrict to ads that fit in the paper
fb22_text2 <- fb22_text[nchar(fb22_text$ad_creative_body) < 200,]
# Kick out ads whose ad creative body alone doesn't make much sense
fb22_text2 <- fb22_text2[nchar(fb22_text2$ad_creative_body) >= 50,]

#----
# Sample ads for each goal and put them into a latex table
goal_names <- c("Donate", "Contact", "Purchase", "Vote", "Event", "Poll", "Acquisition", "Learn", "Persuade")

sampled_ads <- list()
set.seed(123)
for(i in 1:length(goal_names)){
  current_goal <- goal_names[i]
  current_df <- fb22_text2[fb22_text2[,current_goal] == 1,]
  sampled_ad <- sample(1:nrow(current_df), size = 1, prob = current_df$spend)
  sampled_ads[[i]] <- data.frame(sampled_ad_goal = current_goal, sampled_ad_adid = current_df$ad_id[sampled_ad], sampled_ad_page_name = current_df$page_name[sampled_ad], sampled_ad_ad_creative_body = current_df$ad_creative_body[sampled_ad])
}

sampled_ads_df <- do.call(rbind, sampled_ads)
sampled_ads_df <- sampled_ads_df %>% select(-sampled_ad_adid)
names(sampled_ads_df) <- c("Goal", "Page Name", "Ad")

xt <- xtable(sampled_ads_df, caption = "Example ads for each predicted goal. The ads were randomly sampled from candidate ads whose ad creative body was between 50 and 200 characters (to fit the page), weighted by ad spend.", label = "tab:example_ads")
align(xt) <- c("l", "l", "l", "p{10cm}")
print(xt, include.rownames = F, , sanitize.text.function = function(x) { x })
