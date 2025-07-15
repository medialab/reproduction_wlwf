'''
The aim of this script is to create a database to an accomodating format for VAR model implementation
'''

import os
import numpy as np 
import csv
import casanova
from collections import defaultdict
from datetime import datetime, timedelta
import subprocess

from utils import (
    existing_dir_path,
    create_dir,
    count_nb_files, 
    iter_on_files,
    write_general_TS
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

reader_G = casanova.reader("data_prod/dashboard/bertopic/data/bertopic_ts_1.csv")
groups = list(dict.fromkeys(list(reader_G.cells('party'))))

df = split_val(parse_csv_data(run_xan_command("bertopic")), groups)
output_file = f"data_prod/var/general_TS_xan.csv"
with open(output_file, "w", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=df[0].keys())
    writer.writeheader()
    writer.writerows(df)