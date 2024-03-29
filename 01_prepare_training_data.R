library(data.table)
library(stringr)
library(stringi)
library(dplyr)
library(tidyr)

# Input data
path_input_data <- "data/fbel_w_train.csv"
# Output data
path_output_data <- "data/fbel_prepared.csv"


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

# Text order is ad creative body, OCR and ASR first
# The reason is that the DistilBert will cut off texts at 128 tokens
# So the more important ones are less likely to get cut off
df <- df %>% unite(col = "text", 
                     c(ad_creative_body, ocr, asr, page_name, disclaimer, 
                       ad_creative_link_caption, ad_creative_link_title, 
                       ad_creative_link_description), 
                     sep = " ")

# Kick out empty ads
df <- df[df$text != "",]

# Replace newlines with spaces
df$text <- str_replace_all(df$text, "\\\n", " ")

fwrite(df, path_output_data)
