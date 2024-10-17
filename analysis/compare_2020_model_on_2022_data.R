library(data.table)
library(stringr)

files_2020 <- dir("../performance/rf/", full.names = T)
files_2022 <- dir("../performance/rf_on_2022/", full.names = T)

for(f in 1:length(files_2020)){
  df_2020 <- fread(files_2020[f], header = T)
  df_2022 <- fread(files_2022[f], header = T)
  print(basename(files_2020[f]))
  print(paste(round(df_2020[3,3], 2), " --> ", round(df_2022[3,3], 2)))
}
