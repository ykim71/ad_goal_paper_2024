# Replication Archive - Goals of US Election Ads
Replication archive for the paper:
> Neumann, Markus, Sebastian Zimmeck, Jielu Yao, Erika Franklin Fowler, Michael Franz, Breeze Floyd, and Travis Nelson Ridout. **"The Strategic Goals of US Election Ads on Social Media: A Descriptive Study of the 2022 Midterms."** *Journal of Quantitative Description: Digital Media* 5 (2025): 1–38.

## Figures and Tables
The scripts to replicate the tables and figures in the paper are located in `analysis`. Specifically:
| Figure | Filename |
|--------|----------|
| Figure 1 | sponsors.R |
| Figure 2 | basic_stats.R |
| Figure 3 | sponsors.R |
| Figure 4 | sponsors.R |
| Figure 5a | survival.R |
| Figure 5b | over_time_analysis.R |
| Figure 6 | age.R |
| Figure 7 | basic_stats.R |
| Figure 8 | overlap.R |

| Table | Filename |
|--------|----------|
| Table 1 | Manually compiled |
| Table 2 | basic_stats.R |
| Table 3 | pos_neg_labels_in_test_set.R, icr.R |
| Table 4 | compare_bert_vs_rf.R |
| Table 5 | compare_bert_vs_rf.R |
| Table 6 | compare_bert_vs_rf.R |
| Table 7 | fightin_words.R |
| Table 8 | example_ads.R |

## Machine Learning Replication
The files in the main directory train the models on combined 2020 and 2022 data and the conduct inference.

`01_prepare_2020_2022.R` combines 2020 and 2022 text data with human-coded labels.

`02_baseline.py` creates the train/test split (creating `train_2020_2022.csv` and `test_2020_2022.csv`) and trains and tests the baseline (Random Forest) model.

`03_train_distilbert.ipynb` trains the BERT model.

`04_evaluate_distilbert.py` compares the BERT model's performance against the baseline model.

`05_inference.ipynb` runs inference using the models created in the step before. The resulting file is `fb2022_predicted_goals_bert_all.csv.gz`.
