import pandas as pd
import re
import os

with open('data_prod/key_words.txt', 'r') as f:
    regex_lines = f.readlines()

# Nettoyer les expressions régulières (enlever les espaces et les retours à la ligne)
regex_terms = [line.strip() for line in regex_lines]

# Combiner les mots en une seule expression régulière avec 'OU' (|)
regex_pattern = '|'.join(regex_terms)

print(regex_pattern)

def filter(group):
    folder_path = os.path.join("/store/medialex/reproduction_wlwf/data_source", group)
    list_files = os.listdir(folder_path)
    total_rows = 0
    rows_rest = 0
    for file in list_files:
        path = os.path.join(folder_path, file)
        df = pd.read_csv(path)
        total_rows += df.shape[0]
        df_filtered = df[df['text'].str.contains(regex_pattern, case=False, na=False)]
        rows_rest = df_filtered.shape[0]
        out_path = os.path.join("data_source", group, file)
        df_filtered.to_csv(out_path, index=False)
    return((total_rows, rows_rest))

att = filter("attentive")
print("Attentive total rows", att[0], "rested rows", att[1])
gen = filter("general")
print("General total rows", gen[0], "rested rows", gen[1])