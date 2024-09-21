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

results_dir = 'performance'

train = pd.read_csv('data/train.csv', names = ['ad_id', 'text', 'DONATE', 'CONTACT', 'PURCHASE', 'GOTV', 'EVENT', 'POLL', 'GATHERINFO', 'LEARNMORE', "PRIMARY_PERSUADE"])
test = pd.read_csv('data/fb2022_coded.csv.gz')

clf_rf = Pipeline([('vect', CountVectorizer()),
                    ('tfidf', TfidfTransformer()),
                    ('cal', CalibratedClassifierCV(RandomForestClassifier(n_estimators=500, random_state=123), cv=2, method="sigmoid"),)
])

model_name = 'rf_on_2022'

goals = ["DONATE", "CONTACT", "PURCHASE", "GOTV", "EVENT", "POLL", "GATHERINFO", "LEARNMORE", "PRIMARY_PERSUADE"]

for g in goals:
  
  clf_rf.fit(train['text'], train[g])
  predicted = clf_rf.predict(test['text'])
  
  #print(metrics.classification_report(test[g], predicted))
  
  df_perf = pd.DataFrame(metrics.precision_recall_fscore_support(test[g], predicted))
  df_perf.index = ['Precision', 'Recall', 'F-Score', 'Support']
  df_perf.to_csv(results_dir + "/" + model_name + "/" + g + '.csv')
  
