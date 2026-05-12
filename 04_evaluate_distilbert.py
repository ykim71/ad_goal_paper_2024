from sklearn import metrics
import pandas as pd
import numpy as np

results_dir = 'performance'

test = pd.read_csv('data/test_2020_2022_2024.csv')
test_pred = pd.read_csv('data/test_predictions_bert_2024.csv')

model_name = 'bert'

goals = ["DONATE", "CONTACT", "PURCHASE", "GOTV", "EVENT", "POLL", "GATHERINFO", "LEARNMORE", "PRIMARY_PERSUADE"]

df_combined_perf = pd.DataFrame(np.nan, index=[0], columns=goals)
df_combined_perf_weighted_f1 = pd.DataFrame(np.nan, index=[0], columns=goals)

for g in goals:
  
  df_perf = pd.DataFrame(metrics.precision_recall_fscore_support(test[g], test_pred[g]))
  df_perf['weighted_avg'] = pd.Series(metrics.precision_recall_fscore_support(test[g], test_pred[g], average = "weighted"))
  df_perf = np.round(df_perf, 3)
  df_perf.index = ['Precision', 'Recall', 'F-Score', 'Support']
  df_perf.to_csv(results_dir + "/" + model_name + "/" + g + '.csv')
  
  df_combined_perf.at[0,g] = df_perf.iloc[2,1]
  df_combined_perf_weighted_f1.at[0,g] = df_perf['weighted_avg'].iloc[2]
  

df_combined_perf.to_csv("performance/bert_2020_2022_2024_feature_comparison_class1.csv")
df_combined_perf_weighted_f1.to_csv("performance/bert_2020_2022_2024_feature_comparison_weighted.csv")


# Calculate micro F1 score and then weigh by support
combined_micro_f1 = []

# Loop through 'goals' and append one-row DataFrames
for g in goals:
    combined_micro_f1.append(pd.DataFrame(
        data = {'micro_f1': [metrics.f1_score(test[g], test_pred[g], average="micro")], 
                'support': [test[g].sum()]}, 
        index = [g]
    ))

# Concatenate the list of DataFrames
combined_df = pd.concat(combined_micro_f1)
combined_df.to_csv("performance/bert_2020_2022_2024_combined_micro_f1.csv")

weighted_micro_f1 = (combined_df['micro_f1']*combined_df['support']).sum()/combined_df['support'].sum()


