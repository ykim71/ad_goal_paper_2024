import sklearn.model_selection as ms
from sklearn.pipeline import Pipeline
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier
from sklearn.calibration import CalibratedClassifierCV
from sklearn.naive_bayes import MultinomialNB
from sklearn import metrics
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfTransformer
import pandas as pd
import numpy as np
from joblib import dump, load
import os

# Read in the data
df = pd.read_csv('data/humancoded_2020_2022.csv.gz', encoding = 'UTF-8')

# Split
train, test = ms.train_test_split(df, test_size=0.2, random_state=123, stratify = df['year'])

results_dir = 'performance'

clf_rf = Pipeline([('vect', CountVectorizer()),
                    ('tfidf', TfidfTransformer()),
                    ('cal', CalibratedClassifierCV(RandomForestClassifier(n_estimators=500, random_state=123), cv=2, method="sigmoid"),)
])


model_name = 'rf_2020_2022_separate'
os.makedirs(os.path.dirname(results_dir + "/" + model_name + "/"), exist_ok=True)

goals = ["DONATE", "CONTACT", "PURCHASE", "GOTV", "EVENT", "POLL", "GATHERINFO", "LEARNMORE", "PRIMARY_PERSUADE"]

text_order1 = ['ad_creative_body', 'asr', 'ocr']
text_order2 = ['ad_creative_body', 'asr', 'ocr', 'page_name', 'disclaimer', 'ad_creative_link_caption', 'ad_creative_link_title', 'ad_creative_link_description']

train['combined_text'] = train[text_order1].apply(lambda row: ' '.join(row.dropna().astype(str).replace('', None).dropna()), axis=1)
test['combined_text'] = test[text_order1].apply(lambda row: ' '.join(row.dropna().astype(str).replace('', None).dropna()), axis=1)
train['combined_everything'] = train[text_order2].apply(lambda row: ' '.join(row.dropna().astype(str).replace('', None).dropna()), axis=1)
test['combined_everything'] = test[text_order2].apply(lambda row: ' '.join(row.dropna().astype(str).replace('', None).dropna()), axis=1)

train.to_csv('data/train_2020_2022.csv', index = False)
test.to_csv('data/test_2020_2022.csv', index = False)

fields = ['ad_creative_body', 'combined_text', 'combined_everything']

df_combined_perf = pd.DataFrame(np.nan, index=fields, columns=goals)
df_combined_perf_weighted_f1 = pd.DataFrame(np.nan, index=fields, columns=goals)

for f in fields:
  for g in goals:
    
    train_clean = train[[g,f]]
    train_clean = train_clean.dropna()
    test_clean = test[[g,f]]
    test_clean = test_clean.dropna()
    clf_rf.fit(train_clean[f], train_clean[g])
    predicted = clf_rf.predict(test_clean[f])
    
    #print(metrics.classification_report(test[g], predicted))
    
    df_perf = pd.DataFrame(metrics.precision_recall_fscore_support(test_clean[g], predicted))
    df_perf['weighted_avg'] = pd.DataFrame(metrics.precision_recall_fscore_support(test_clean[g], predicted, average = "weighted"))
    df_perf = np.round(df_perf, 2)
    df_perf.index = ['Precision', 'Recall', 'F-Score', 'Support']
    df_perf.to_csv(results_dir + "/" + model_name + "/" + g + "_" + f + '.csv')
    
    #df_perf = pd.read_csv(results_dir + "/" + model_name + "/" + g + "" + f + '.csv')
    df_combined_perf.at[f,g] = df_perf.iloc[2,1]
    df_combined_perf_weighted_f1.at[f,g] = df_perf['weighted_avg'][2]
    # Save model to disk
    #dump(clf_rf, 'models/goal_rf_' + g + '.joblib')


df_combined_perf.to_csv("performance/rf_2020_2022_feature_comparison_class1.csv")
df_combined_perf_weighted_f1.to_csv("performance/rf_2020_2022_feature_comparison_weighted.csv")


