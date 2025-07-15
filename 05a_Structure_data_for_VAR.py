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
    command = f"xan cat rows data_prod/dashboard/bertopic/data/*.csv | xan drop prop | xan groupby date,topic 'values(nb_tweets)' | xan sort -s topic,date"
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

parser.add_argument("--retweets", 
    action = "store_true",
    help = "Run the script counting the retweets",
)

args = parser.parse_args()

reader_G = casanova.reader("data_prod/dashboard/bertopic/data/bertopic_ts_1.csv")
groups = list(dict.fromkeys(list(reader_G.cells('party'))))

df = split_val(parse_csv_data(run_xan_command("bertopic")), groups)
output_file = f"data_prod/var/general_TS.csv"
with open(output_file, "w", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=df[0].keys())
    writer.writeheader()
    writer.writerows(df)

#raise(ValueError("STOP : We stop here but we want to keep previous version to avoid xan"))

input_path = os.path.join(args.origin_path, "data_prod", "dashboard", "bertopic", "data")
files_TS =list(iter_on_files(input_path, count_nb_files(input_path))[1]) 

def list_dates(start_date: str, end_date: str):
    start = datetime.strptime(start_date, "%Y-%m-%d")
    end = datetime.strptime(end_date, "%Y-%m-%d")
    delta = (end - start).days

    return [(start + timedelta(days=i)).strftime("%Y-%m-%d") for i in range(delta + 1)]

# Exemple d'utilisation :
dates = list_dates("2022-06-20", "2023-03-14")
nb_dates = len(dates)

reader = casanova.reader(files_TS[0])
group_types = list(dict.fromkeys(list(reader.cells('party'))))
print(group_types)
index_attentive = group_types.index('attentive')
index_media = group_types.index('media')
index_supp = group_types.index('lr_supp')

index_dict = {
    'attentive': index_attentive,
    'media': index_media,
    'lr_supp': index_supp
}

print(index_dict)

sorted_items = sorted(index_dict.items(), key=lambda x: x[1])

print(sorted_items)

with open(os.path.join(args.origin_path, "data_prod", "var", args.topic_model, "general_TS.csv"), 'w') as f: 
    fieldnames = ['date', 'topic', 'lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'media']
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
                if row == ["date, party, topic, prop, nb_tweets"]:
                    continue
                if sorted_items['index_media'] < sorted_items['index_supp']:
                    index_media = (4 + sorted_items['media']) * nb_dates - 1
                else:
                    index_media = (8 + )
                
                ind_lrs = (4 + sorted_items['index_supp']) * nb_dates - 1
                ind_majs = ind_lrs +1
                ind_nupess = ind_lrs +2
                ind_rns = ind_lrs +3
                writer.writerow({
                        'date': dates[iter_dates],  
                        'topic': row['topic'],
                        'lr': rows[iter_dates]['nb_tweets'],
                        'majority': rows[iter_dates + 1]['nb_tweets'],
                        'nupes': rows[iter_dates + 2]['nb_tweets'],
                        'rn': rows[iter_dates + 3]['nb_tweets'],
                        'lr_supp': rows[ind_lrs]['nb_tweets'],
                        'majority_supp': rows[ind_majs]['nb_tweets'],
                        'nupes_supp': rows[ind_nupess]['nb_tweets'],
                        'rn_supp': rows[ind_rns]['nb_tweets'],
                        'attentive': rows[iter_dates + index_attentive * nb_dates]['nb_tweets'],
                        'media': rows[iter_dates + index_media * nb_dates]['nb_tweets'],
                    })
                iter_dates += 1