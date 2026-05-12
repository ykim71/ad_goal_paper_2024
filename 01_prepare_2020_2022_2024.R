# Prepares 2020, 2022, 2024 handcoding AND inference ads
# Then combines handcoding datasets
setwd("~/Documents/GitHub/ad_goal_paper_2024")
library(data.table)
library(stringr)
library(stringi)
library(dplyr)
library(tidyr)
library(haven)


#-------------------------------------------------------------------------------
# Skip 2020 & 2022 
# Data ready from previous repo
# path_output_data_2020 <- "data/handcoded_2020.csv"
# path_output_data_2022 <- "data/handcoded_2022.csv"

#-------------------------------------------------------------------------------
# 2024

# Prepare the FB and GG 2024 data so it is in the same shape as the training data
# Part 1 prepares the ads themselves
# Part 2 cleans up the human coding and merges the respective ads into it

# Input data
path_input_data_fb <- "data/facebook_2024_manual_coding_unique_cids.csv"
path_input_data_gg <- "data/google_2024_manual_coding_unique_cids.csv"

path_humancoded_input_data_fb <- "data/FB_Goal_noICR_050626.dta"
path_humancoded_input_data_gg <- "data/Google_Goal_noICR_050526.dta"

# Output data
path_output_data_fb <- "data/fb2024_inference.csv.gz"
path_output_data_gg <- "data/gg2024_inference.csv.gz"

path_humancoded_output_data_fb <- "data/handcoded_2024_fb.csv"
path_humancoded_output_data_gg <- "data/handcoded_2024_gg.csv"

#----
# Part 1

# Load data
df_fb <- fread(path_input_data_fb, encoding = 'UTF-8')
df_gg <- fread(path_input_data_gg, encoding = 'UTF-8')

df_fb <- df_fb %>% select(-c(cid, race, ad_snapshot_url, checksum, total_spend, sponsor_type))
df_gg <- df_gg %>% select(-c(cid, race, ad_url, checksum, total_spend, sponsor_type))

# Clean text fields
df_fb <- df_fb %>%
  mutate(across(c(ad_creative_body, asr_text, ocr_text, 
                  page_name, funding_entity,
                  ad_creative_link_caption, ad_creative_link_title, 
                  ad_creative_link_description),
                ~str_remove_all(., "\\[|\\]") %>% # Replace brackets 
                  str_squish() %>%                # Remove extra spaces
                  na_if("")))                     # Replace empty strings with NA

df_gg <- df_gg %>%
  mutate(across(c(ad_text, asr_text, ocr_text, 
                  advertiser_name),
                ~str_replace_all(., "\\\\n", " ") %>%  # Replace brackets 
                  str_squish() %>%                # Remove extra spaces
                  na_if("")))                    # Replace empty strings with NA

# Concatenate and save again
df_fb2 <- df_fb %>% unite(text, ad_creative_body, asr_text, ocr_text, page_name, funding_entity, ad_creative_link_caption, ad_creative_link_title, ad_creative_link_description, sep = " ", na.rm = T)
fwrite(df_fb2, path_output_data_fb)

df_gg2 <- df_gg %>% unite(text, ad_text, asr_text, ocr_text, advertiser_name, sep = " ", na.rm = T)
fwrite(df_gg2, path_output_data_gg)

#----
# Part 2

# For the ads that have human-coding, clean up the data and merge with the text
df_hc_fb <- read_dta(path_humancoded_input_data_fb)
df_hc_gg <- read_dta(path_humancoded_input_data_gg)

df_hc_fb <- df_hc_fb %>% select(adid, DONATE:PRIMARY_PERSUADE)
df_hc_gg <- df_hc_gg %>% select(adid, DONATE:PRIMARY_PERSUADE)

# Facebook
df_hc_fb$PRIMARY_PERSUADE <- abs(df_hc_fb$PRIMARY_PERSUADE-2)
df_hc_fb[is.na(df_hc_fb)] <- 0
# Merge with text data
df_hc_fb <- left_join(df_hc_fb, df_fb, by = c("adid" = "ad_id"))
df_hc_fb <- df_hc_fb %>% rename(ad_id = adid)

fwrite(df_hc_fb, path_humancoded_output_data_fb)

# Google
df_hc_gg$PRIMARY_PERSUADE <- abs(df_hc_gg$PRIMARY_PERSUADE-2)
df_hc_gg[is.na(df_hc_gg)] <- 0
# Merge with text data
df_hc_gg <- left_join(df_hc_gg, df_gg, by = c("adid" = "ad_id"))
df_hc_gg <- df_hc_gg %>% rename(ad_id = adid)

fwrite(df_hc_gg, path_humancoded_output_data_gg)


#-------------------------------------------------------------------------------
# Combine 2020, 2022, 2024

# Input data
path_2020_humancoded_output_data <- "data/handcoded_2020.csv"
path_2022_humancoded_output_data <- "data/handcoded_2022.csv"
path_2024_humancoded_output_data_fb <- "data/handcoded_2024_fb.csv"
path_2024_humancoded_output_data_gg <- "data/handcoded_2024_gg.csv"
# Output data
path_output_data <- "data/humancoded_2020_2022_2024.csv.gz"

df_hc_2020 <- fread(path_2020_humancoded_output_data)
df_hc_2020$year <- 2020
df_hc_2020$platform <- "Facebook"

df_hc_2022 <- fread(path_2022_humancoded_output_data)
df_hc_2022 <- df_hc_2022 %>% rename(ocr = aws_ocr_text_img,
                                    ocr_vid = aws_ocr_text_vid,
                                    asr = google_asr_text)
df_hc_2022$year <- 2022
df_hc_2022$platform <- "Facebook"

df_hc <- bind_rows(df_hc_2020, df_hc_2022)
df_hc <- df_hc %>% relocate(ocr_vid, .after = ocr)

# add 2024
df_hc_2024_fb <- fread(path_2024_humancoded_output_data_fb)
df_hc_2024_fb$year <- 2024
df_hc_2024_fb$platform <- "Facebook"

df_hc_2024_gg <- fread(path_2024_humancoded_output_data_gg)
df_hc_2024_gg$year <- 2024
df_hc_2024_gg$platform <- "Google"

df_hc_2024 <- bind_rows(df_hc_2024_fb, df_hc_2024_gg)

df_hc_all <- bind_rows(df_hc, df_hc_2024)

# Report number of ads per cycle in the paper
table(df_hc_all$year)

fwrite(df_hc_all, path_output_data)

