import pandas as pd
import sklearn.model_selection as ms

# Read in the data
df = pd.read_csv('data/fbel_prepared.csv', encoding = 'UTF-8')

# Split
train, test = ms.train_test_split(df, test_size=0.2, random_state=123)

# Save
train.to_csv('data/train.csv', index = False, header = False)
test.to_csv('data/test.csv', index = False, header = False)
