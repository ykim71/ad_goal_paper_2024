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

test = pd.read_csv('data/test.csv', names = ['ad_id', 'text', 'DONATE', 'CONTACT', 'PURCHASE', 'GOTV', 'EVENT', 'POLL', 'GATHERINFO', 'LEARNMORE', "PRIMARY_PERSUADE"])
test_pred = pd.read_csv('data/test_predictions_bert.csv')

model_name = 'bert'

goals = ["DONATE", "CONTACT", "PURCHASE", "GOTV", "EVENT", "POLL", "GATHERINFO", "LEARNMORE", "PRIMARY_PERSUADE"]

for g in goals:
  
  df_perf = pd.DataFrame(metrics.precision_recall_fscore_support(test[g], test_pred[g]))
  df_perf.index = ['Precision', 'Recall', 'F-Score', 'Support']
  df_perf.to_csv(results_dir + "/" + model_name + "/" + g + '.csv')
  

