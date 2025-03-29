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

# Merge in incumbency
cands <- fread("../../datasets/candidates/wmpcand_120223_wmpid.csv")
cands <- cands %>% select(wmpid, cand_incumbent_challenger_open_s)
cands <- cands %>% rename(incumbency = "cand_incumbent_challenger_open_s")
cands <- cands %>% filter(incumbency != "")
# Remove duplicates
# First sort cands so that INCUMBENT and CHALLENGER come before OPEN and ""
# (this will keep incumbent and open over the other 2)
cands <- cands %>% mutate(incumbency = factor(incumbency, levels = c("INCUMBENT", "CHALLENGER", "OPEN", "")))
cands <- cands[order(cands$incumbency),]
cands <- cands[!duplicated(cands$wmpid),]
# Merge
wmpent <- left_join(wmpent, cands, by = "wmpid")

wmpent <- wmpent %>% select(pd_id, wmp_spontype, wmp_office, party_all, incumbency)
wmpent[wmpent == ""] <- NA

# Michael Young
wmpent$wmp_office[wmpent$pd_id == "pd-108849960534235-2"] <- "down ballot"
wmpent$party_all[wmpent$pd_id == "pd-108849960534235-2"] <- "OTHER"

 # Steven Sams
wmpent$party_all[wmpent$pd_id == "pd-105150411869111-1"] <- "REP"
wmpent$incumbency[wmpent$pd_id == "pd-105150411869111-1"] <- "CHALLENGER"

save(wmpent, file = "../data/wmpentities_2022.rdata")
