library(data.table)
library(stringr)
library(stringi)
library(dplyr)
library(tidyr)
library(SpeedReader)
library(quanteda)
library(slam)
library(xtable)

fb22 <- fread("../data/fb2022_predicted_goals_bert_all.csv.gz", data.table = F)
goal_names <- data.frame(wrong = c("DONATE", "CONTACT", "PURCHASE", "GOTV", "EVENT", "POLL", "GATHERINFO", "LEARNMORE", "PRIMARY_PERSUADE"),
                         correct = c("Donate", "Contact", "Purchase", "Vote", "Event", "Poll", "Info", "Learn", "Persuade"))
rename_strings <- function(x) {
  ifelse(x %in% goal_names$wrong, goal_names$correct[match(x, goal_names$wrong)], x)
}
names(fb22) <- rename_strings(names(fb22))
fb22$Nogoals <- fb22 %>% select(-ad_id) %>% apply(., 1, function(x){all(x == 0)}) %>% as.numeric()
fb22_text <- fread("../data/fb2022_prepared.csv.gz", data.table = F)

# Number of unique ads
length(unique(fb22_text$text))

# Top Fightin Words associated with each goal
# Tokenize the text
tks <- tokens(fb22_text$text, remove_punct = T)
tks <- tokens_select(tks, min_nchar = 3)

# Assign goal variables to the token object
docvars(tks, "Donate") <- fb22$Donate
docvars(tks, "Contact") <- fb22$Contact
docvars(tks, "Purchase") <- fb22$Purchase
docvars(tks, "Vote") <- fb22$Vote
docvars(tks, "Event") <- fb22$Event
docvars(tks, "Poll") <- fb22$Poll
docvars(tks, "Info") <- fb22$Info
docvars(tks, "Learn") <- fb22$Learn
docvars(tks, "Persuade") <- fb22$Persuade
fb22$nogoals <- fb22 %>% select(ends_with("_prediction")) %>% apply(., 1, function(x){all(x == 0)}) %>% as.numeric()
docvars(tks, "No goals") <- fb22$Nogoals # mostly about Ukraine

#Remove stopwords
stopw <- quanteda::stopwords()
tks <- tokens_remove(tks, stopw)

#Convert to document-feature-matrix, make a dataframe with the covariates
dtm <- dfm(tks)
df_vars <- docvars(dtm)
dtm <- slam::as.simple_triplet_matrix(dtm)

# Loop over goals, do fightin words for each
goals <- c("Donate",
           "Contact",
           "Purchase",
           "Vote",
           "Event",
           "Poll",
           "Info",
           "Learn",
           "Persuade",
           "No goals")

dfg <- matrix("", nrow = 20, ncol = length(goals))
dfg <- as.data.frame(dfg)
names(dfg) <- goals

for(i in 1:length(goals)){
  # Fightin Words
  cgt <- contingency_table(df_vars, dtm, variables_to_use = goals[i])
  fs <- feature_selection(cgt,
                          method = "informed Dirichlet")
  dfg[goals[i]] <- fs[['1']]$term[1:20]
}

dfg %>% rename(Acquisition = "Info")

# rename and reorder columns
dfg <- dfg %>% rename(Acquisition = "Info") %>% select("Acquisition", "Contact", "Donate", "Event", "Learn", "Poll", "Persuade", "Purchase", "Vote", "No goals")

fwrite(dfg, "tables/fightin_words_goal_top_20.csv")
print(xtable(dfg, 
             caption = "Top 20 words most associated with a given goal.",
             label = "tab:fightin_words"),
      "latex",
      "tables/fightin_words.tex",
      include.rownames = F)
