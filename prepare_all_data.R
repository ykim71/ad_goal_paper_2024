# Prepare the FB 2022 data so it is in the same shape as the training data
# Part 1 prepares the ads themselves
# Part 2 cleans up the human coding and merges the respective ads into it

library(data.table)
library(stringr)
library(stringi)
library(dplyr)
library(tidyr)

# Input data
path_input_data <- "data/fb_2022_adid_text_clean.csv.gz"
path_humancoded_input_data <- "data/fb2022_082224_partial.csv"
# Output data
path_output_data <- "data/fb2022_prepared_separate_fields.csv.gz"
path_humancoded_output_data <- "data/fb2022_coded_separate_fields.csv.gz"

#----
# Part 1

# Load data
df <- fread(path_input_data, encoding = 'UTF-8')

# Example cleaning for multiple text columns (replace 'text' with your column names)
df <- df %>%
  mutate(across(c(ad_creative_body, google_asr_text, aws_ocr_text_img, aws_ocr_text_vid, page_name, disclaimer,
                  ad_creative_link_caption, ad_creative_link_title, 
                  ad_creative_link_description),
                ~str_replace_all(., "\\\\n", " ") %>% # Replace newlines with a space
                  str_squish() %>%                                         # Remove extra spaces
                  na_if("")))                                              # Replace empty strings with NA
df$year <- 2022
fwrite(df, path_output_data)

#----
# Part 2

# For the ads that have human-coding, clean up the data and merge with the text
df_hc <- fread(path_humancoded_input_data, encoding = 'UTF-8')
df_hc <- df_hc %>% select(adid, DONATE:PRIMARY_PERSUADE)
df_hc$adid <- str_remove(df_hc$adid, "\\\\")
df_hc$adid <- str_remove(df_hc$adid, "\\\"\\\"\\)")
df_hc[df_hc==""] <- 0
df_hc$PRIMARY_PERSUADE[df_hc$PRIMARY_PERSUADE == "No, primary goal is something else"] <- 0
df_hc <- df_hc %>% mutate(across(DONATE:PRIMARY_PERSUADE, ~ ifelse(. != 0, 1, 0)))
df_hc <- left_join(df_hc, df, by = c("adid" = "ad_id"))
df_hc <- df_hc %>% rename(ad_id = adid)

fwrite(df_hc, path_humancoded_output_data)
