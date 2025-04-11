# ad_goal_paper
Replication repo for the ad goal paper

The files in the main directory train the models on combined 2020 and 2022 data and the conduct inference.

`01_prepare_2020_2022.R` combines 2020 and 2022 text data with human-coded labels.

`02_baseline.py` creates the train/test split (creating `train_2020_2022.csv` and `test_2020_2022.csv`) and trains and tests the baseline (Random Forest) model.

`03_train_distilbert.ipynb` trains the BERT model.

`04_evaluate_distilbert.py` compares the BERT model's performance against the baseline model.

`05_inference.ipynb` runs inference using the models created in the step before. The resulting file is `fb2022_predicted_goals_bert_all.csv.gz`.

The files in analysis all create figures and tables for the paper.