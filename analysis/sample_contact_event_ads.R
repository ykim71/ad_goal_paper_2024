library(data.table)
library(dplyr)
library(tidyr)
library(stringr)

bert <- fread("../data/fb2022_predicted_goals_bert_all.csv.gz", data.table = F)
fb22_text <- fread("../data/fb2022_inference.csv.gz", data.table = F)

bert <- bert %>% select(ad_id, CONTACT, EVENT)
fb22_text <- left_join(fb22_text, bert)
contact_ads <- fb22_text %>% filter(CONTACT == 1)
event_ads <- fb22_text %>% filter(EVENT == 1)

set.seed(123)
contact_sample_ids <- sample(1:nrow(contact_ads), 10)
contact_sample <- contact_ads[contact_sample_ids, c("ad_id", "text")]
fwrite(contact_sample, "../data/contact_ads_sample.csv")

set.seed(123)
event_sample_ids <- sample(1:nrow(event_ads), 10)
event_sample <- event_ads[event_sample_ids, c("ad_id", "text")]
fwrite(event_sample, "../data/event_ads_sample.csv")
