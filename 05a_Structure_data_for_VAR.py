'''
The aim of this script is to create a database to an accomodating format for VAR model implementation
'''

import os
import numpy as np 
import csv
import casanova
import argparse
from collections import defaultdict
from datetime import datetime, timedelta
import subprocess

from utils import (
    existing_dir_path,
    create_dir,
    count_nb_files, 
    iter_on_files,
)


def run_xan_command(topic_model):
    command = f"xan cat rows data_prod/dashboard/{topic_model}/data/*.csv | xan drop prop | xan groupby date,topic 'values(nb_tweets)' | xan sort -s topic,date"
    result = subprocess.run(command, shell=True, capture_output=True, text=True)
    return result.stdout

def parse_csv_data(csv_text):
    lines = csv_text.strip().split("\n")
    reader = csv.DictReader(lines)
    return [row for row in reader]

def split_val(data, groups):
    for row in data:
        values = row["values(nb_tweets)"].split("|")  # Séparation par '|'
        
        # Ajouter dynamiquement des colonnes
        for i, value in enumerate(values):
            row[f"{groups[i]}"] = value
        
        # Supprimer l'ancienne colonne
        del row["values(nb_tweets)"]

    return data

parser = argparse.ArgumentParser()

parser.add_argument(
    "--origin_path",
    help="Path to an origin of your code",
    type=existing_dir_path,
    default=os.getcwd(), 
)

parser.add_argument("--retweets", action = "store_true",
help = "Run the script counting the retweets")

args = parser.parse_args()

reader_G = casanova.reader("data_prod/dashboard/bertopic/data/bertopic_ts_1.csv")
groups = list(dict.fromkeys(list(reader_G.cells('party'))))

df = split_val(parse_csv_data(run_xan_command("bertopic")), groups)
output_file = f"data_prod/var/bertopic/general_TS.csv"
with open(output_file, "w", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=df[0].keys())
    writer.writeheader()
    writer.writerows(df)

raise(ValueError("STOP : We stop here but we want to keep previous version to avoid xan"))

input_path = os.path.join(args.origin_path, "data_prod", "dashboard", str(args.topic_model), "data")
files_TS =list(iter_on_files(input_path, count_nb_files(input_path))[1]) 

def count_dates(start_date, end_date):
    if isinstance(start_date, str):
        start_date = datetime.strptime(start_date, "%Y-%m-%d")
    if isinstance(end_date, str):
        end_date = datetime.strptime(end_date, "%Y-%m-%d")
    return (end_date - start_date).days + 1

nb_dates = count_dates('2022-06-20', '2023-03-14')
reader = casanova.reader(files_TS[0])
if args.topic_model=='bertopic':
    group_types = list(dict.fromkeys(list(reader.cells('party'))))
    index_attentive = group_types.index('attentive')
    index_general = group_types.index('general')
    index_media = group_types.index('media')
    index_lrsupp = group_types.index('lr_supp')
    index_majsupp = group_types.index('majority_supp')
    index_nupessupp = group_types.index('nupes_supp')
    index_rnsupp = group_types.index('rn_supp')
else:
    group_types = list(dict.fromkeys(list(reader.cells('actor'))))
    index_attentive = group_types.index('pub. attentif')
    index_general = group_types.index('pub. general')
    index_media = group_types.index('medias')
    index_lrsupp = group_types.index('sup. lr')
    index_majsupp = group_types.index('sup. majo.')
    index_nupessupp = group_types.index('sup. nupes')
    index_rnsupp = group_types.index('sup. rn')

print(group_types)
print((index_attentive, index_general, index_lrsupp))

with open(os.path.join(args.origin_path, "data_prod", "var", args.topic_model, "general_TS.csv"), 'w') as f: 
    fieldnames = ['date', 'topic', 'lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'general', 'media']
    writer = csv.DictWriter(f, fieldnames=fieldnames)
    writer.writeheader()

    for file in files_TS:
        with open(file, 'r') as file_reader:
            reader = csv.DictReader(file_reader)

            rows = list(reader)
            iter_dates=0

            for row in rows:
                if iter_dates==nb_dates:
                    break
                if args.topic_model == 'lda':
                    ind_lrs = iter_dates + index_lrsupp * nb_dates
                    ind_majs = iter_dates + index_majsupp * nb_dates
                    ind_nupess = iter_dates + index_nupessupp * nb_dates
                    ind_rns = iter_dates + index_rnsupp * nb_dates
                else:
                    ind_lrs = 4*iter_dates + index_lrsupp * nb_dates
                    ind_majs = ind_lrs +1
                    ind_nupess = ind_lrs +2
                    ind_rns = ind_lrs +3
                writer.writerow({
                        'date': rows[iter_dates*4]['date'],  
                        'topic': row['topic'],
                        'lr': rows[iter_dates]['prop'],
                        'majority': rows[iter_dates + 1]['prop'],
                        'nupes': rows[iter_dates + 2]['prop'],
                        'rn': rows[iter_dates + 3]['prop'],
                        'lr_supp': rows[ind_lrs]['prop'],
                        'majority_supp': rows[ind_majs]['prop'],
                        'nupes_supp': rows[ind_nupess]['prop'],
                        'rn_supp': rows[ind_rns]['prop'],
                        'attentive': rows[iter_dates + index_attentive * nb_dates]['prop'],
                        'general': rows[iter_dates + index_general * nb_dates]['prop'],
                        'media': rows[iter_dates + index_media * nb_dates]['prop'],
                    })
                iter_dates += 1