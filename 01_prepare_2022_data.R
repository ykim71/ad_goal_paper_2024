# Prepare the FB 2022 data so it is in the same shape as the training data

library(data.table)
library(stringr)
library(stringi)
library(dplyr)
library(tidyr)

# Input data
path_input_data <- "data/fb_2022_adid_text_clean.csv.gz"
path_humancoded_input_data <- "data/fb2022_082224_partial.csv"
# Output data
path_output_data <- "data/fb2022_prepared.csv.gz"
path_humancoded_output_data <- "data/fb2022_coded.csv.gz"

# Load data
df <- fread(path_input_data, encoding = 'UTF-8')

# Concatenate them all together
df <- df %>% unite(col = "text", 
                   c(ad_creative_body, google_asr_text, aws_ocr_text_img, aws_ocr_text_vid, page_name, disclaimer,
                     ad_creative_link_caption, ad_creative_link_title, 
                     ad_creative_link_description), 
                   sep = " ", na.rm = T)

# Kick out empty ads
df <- df[df$text != "",]
df <- df[is.na(df$text) == F,]

# Replace newlines with spaces
df$text <- str_replace_all(df$text, "\\\\n", " ")

# Clean up extraneous spaces
df$text <- str_squish(df$text)

fwrite(df, path_output_data)

#----

# For the ads that have human-coding, clean up the data and merge with the text
df_hc <- fread(path_humancoded_input_data, encoding = 'UTF-8')
df_hc <- df_hc %>% select(adid, DONATE:PRIMARY_PERSUADE)
df_hc$adid <- str_remove(df_hc$adid, "\\\\")
df_hc$adid <- str_remove(df_hc$adid, "\\\"\\\"\\)")
df_hc[df_hc==""] <- 0
df_hc$PRIMARY_PERSUADE[df_hc$PRIMARY_PERSUADE == "No, primary goal is something else"] <- 0
df_hc <- df_hc %>% mutate(across(DONATE:PRIMARY_PERSUADE, ~ ifelse(. != 0, 1, 0)))
df_hc$text <- df$text[match(df_hc$adid, df$ad_id)]
df_hc <- df_hc %>% relocate(text, .after = adid)

fwrite(df_hc, path_humancoded_output_data)
