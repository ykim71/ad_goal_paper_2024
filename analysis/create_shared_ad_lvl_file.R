library(data.table)
library(dplyr)
library(stringr)

fb22_vars <- fread("../data/fb_2022_adid_var.csv.gz", data.table = F)
fb22_vars <- fb22_vars %>% select(ad_id, page_id, pd_id, publisher_platforms, ad_delivery_start_time, ad_delivery_stop_time, spend, region_distribution)

fb22_vars$spend <- str_split_fixed(fb22_vars$spend, ",", 2) %>%
  apply(., 2, function(x){str_extract(x, "[0-9]+")}) %>%
  apply(., 1, function(x){mean(as.numeric(x))})

# Convert publisher_platforms to list
fb22_vars$publisher_platforms <- str_replace_all(fb22_vars$publisher_platforms, "\"", "")
fb22_vars$publisher_platforms <- str_replace_all(fb22_vars$publisher_platforms, "\\[", "")
fb22_vars$publisher_platforms <- str_replace_all(fb22_vars$publisher_platforms, "\\]", "")
fb22_vars$publisher_platforms <- str_split(fb22_vars$publisher_platforms, ",")

#----

extract_region <- function(x){
  
  json_str <- str_replace_all(x, "\"\"", "\"")
  df_region <- fromJSON(json_str)
  df_region$percentage <- as.numeric(df_region$percentage)
  df_region$region[!df_region$region %in% state.name] <- "Other"
  df_region <- df_region %>% group_by(region) %>% summarize(percentage = sum(percentage))
  
  return(df_region)
}

# Ad-level metadata
fb22_vars_region <- fb22_vars[fb22_vars$region_distribution != "",]
ads_region <- pblapply(fb22_vars_region$region_distribution, extract_region)

add_new_column <- function(df, value) {
  df$ad_id <- value
  return(df)
}

# Long dataframe, % of each region per ad
ads_region <- mapply(add_new_column, ads_region, fb22_vars_region$ad_id, SIMPLIFY = F)
ads_region <- rbindlist(ads_region)

# Save to file
save(ads_region, file = "../data/ads_region.rdata")

#----

# Now that region has been split out, remove it from main df and save it
fb22_vars <- fb22_vars %>% select(-region_distribution)
save(fb22_vars, file = "../data/fb_2022_adid_var_clean.rdata")

#----
# Prepare entity file
library(haven)
wmpent <- read_dta("../data/wmpentity_2022_012125_mergedFECids.dta")
# wmpent <- wmpent %>% 
#   filter((wmp_spontype == "campaign") & (wmp_office %in% c("us house", "us senate")))

# incumbency variables missing right now
wmpent <- wmpent %>% select(pd_id, wmp_spontype, wmp_office, party_all)#, hse_cdstatus, sen_cdstatus)
save(wmpent, file = "../data/wmpentities_2022.rdata")

# test = fread("../data/wmp_fb_2022_entities_v120122.csv", data.table = F)
