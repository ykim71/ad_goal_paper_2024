# Prepares 2020 handcoding, as well as 2022 handcoding AND inference ads
# Then combines 2020 handcoding with 2022 handcoding

library(data.table)
library(stringr)
library(stringi)
library(dplyr)
library(tidyr)
library(haven)


#-------------------------------------------------------------------------------
# 2020

# Input data
path_input_data <- "data/fbel_w_train.csv"
# Output data
path_output_data <- "data/handcoded_2020.csv"


df <- fread(path_input_data, encoding = 'UTF-8', data.table = F)

# Clean up unicode
fix_unicode <- function(text){
  
  Encoding(text) <- "UTF-8"
  # remove trailing backslashes
  # this is necessary, otherwise the unicode unescaping will break some texts
  text <- str_remove(text, "[\\\\]+$")
  # unescaping has to be done twice
  text <- stri_unescape_unicode(text)
  text <- stri_unescape_unicode(text)
  # Convert unicode-based characters
  # this takes care of things like boldface characters, etc.
  text <- stri_trans_nfkd(text)
  
  return(text)
}
# Apply the function to all text columns
df <- df %>%
  mutate(across(c(ad_creative_body, ocr, asr, page_name, disclaimer, 
                  ad_creative_link_caption, ad_creative_link_title, 
                  ad_creative_link_description),
                fix_unicode))

# Clean up brackets
clean_brackets <- function(x){
  x <- str_remove(x,  '\\[\\"\\"')
  x <- str_remove(x,  '\\"\\"\\]')
  x <- str_replace_all(x,  '\\"\\"', '\\"')
}
# Apply the function to all text columns
df <- df %>%
  mutate(across(c(ad_creative_body, ocr, asr, page_name, disclaimer, 
                  ad_creative_link_caption, ad_creative_link_title, 
                  ad_creative_link_description),
                clean_brackets))

# Clean up semicolons in ocr
df$ocr <- str_replace_all(df$ocr, ';', ' ')

# Kick out irrelevant variables
df <- df %>% select(ad_id, 
                    ad_creative_body, ocr, asr, page_name, disclaimer, ad_creative_link_caption, ad_creative_link_title, ad_creative_link_description,
                    DONATE, CONTACT, PURCHASE, GOTV, EVENT, POLL, GATHERINFO, LEARNMORE, PRIMARY_PERSUADE)

# Exclude ads that having nothing for PRIMARY_PERSUADE
# Because those ads also weren't coded for DONATE etc. 
# and would be incorrectly assigned a 0 (as in, not DONATE etc. when they might be)
df <- df[df$PRIMARY_PERSUADE != "",]
df$PRIMARY_PERSUADE[df$PRIMARY_PERSUADE == "No, primary goal is something else"] <- ""

# If the variable is empty, code as 0, otherwise 1
df <- df %>%
  mutate(across(c(DONATE, CONTACT, PURCHASE, GOTV, EVENT, POLL, GATHERINFO, LEARNMORE, PRIMARY_PERSUADE),
                function(x){ifelse(x == "", 0, 1)}))

df <- df %>%
  mutate(across(c(ad_creative_body, ocr, asr, page_name, disclaimer, 
                  ad_creative_link_caption, ad_creative_link_title, 
                  ad_creative_link_description),
                \(x) str_replace_all(x, "\\\n", " ")))


fwrite(df, path_output_data)

rm(list = ls())


#-------------------------------------------------------------------------------
# 2022

# Prepare the FB 2022 data so it is in the same shape as the training data
# Part 1 prepares the ads themselves
# Part 2 cleans up the human coding and merges the respective ads into it

# Input data
path_input_data <- "data/fb_2022_adid_text_clean.csv.gz"
path_humancoded_input_data <- "data/fb2022_082224_partial.csv"
path_humancoded_input_data <- "data/FBEL_092924.dta"
# Output data
path_output_data <- "data/fb2022_inference.csv.gz"
path_humancoded_output_data <- "data/handcoded_2022.csv"

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

fwrite(df, path_output_data)

#----
# Part 2

# For the ads that have human-coding, clean up the data and merge with the text
df_hc <- read_dta(path_humancoded_input_data)
# Noncandidate ads weren't coded for goal, remove them
df_hc <- df_hc %>% filter(CAND1 != "")
df_hc <- df_hc %>% select(adid, DONATE:PRIMARY_PERSUADE)
df_hc$PRIMARY_PERSUADE <- abs(df_hc$PRIMARY_PERSUADE-2)
df_hc[is.na(df_hc)] <- 0
# Merge with text data
df_hc <- left_join(df_hc, df, by = c("adid" = "ad_id"))
df_hc <- df_hc %>% rename(ad_id = adid)

fwrite(df_hc, path_humancoded_output_data)

rm(list = ls())


#-------------------------------------------------------------------------------
# Combine 2020 and 2022

# Input data
path_2020_humancoded_output_data <- "data/handcoded_2020.csv"
path_2022_humancoded_output_data <- "data/handcoded_2022.csv"
# Output data
path_output_data <- "data/humancoded_2020_2022.csv.gz"


df_hc_2020 <- fread(path_2020_humancoded_output_data)
df_hc_2020$year <- 2020

df_hc_2022 <- fread(path_2022_humancoded_output_data)
df_hc_2022 <- df_hc_2022 %>% rename(ocr = aws_ocr_text_img,
                                    ocr_vid = aws_ocr_text_vid,
                                    asr = google_asr_text)
df_hc_2022$year <- 2022

df_hc <- bind_rows(df_hc_2020, df_hc_2022)
df_hc <- df_hc %>% relocate(ocr_vid, .after = ocr)

fwrite(df_hc, path_output_data)

