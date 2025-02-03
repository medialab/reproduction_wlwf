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

from utils import (
    existing_dir_path,
    create_dir,
    count_nb_files, 
    iter_on_files,
)

parser = argparse.ArgumentParser()

parser.add_argument(
    "topic_model",
    help="Name of the topic model chosen. Write lda or bertopic",
)

parser.add_argument(
    "--origin_path",
    help="Path to an origin of your code",
    type=existing_dir_path,
    default=os.getcwd(), 
)

args = parser.parse_args()

if args.topic_model not in ['lda', 'bertopic']:
    raise ValueError("The current topic model is incorrect. Choose lda or bertopic as input for model type")

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
                writer.writerow({
                        'date': rows[iter_dates*4]['date'],  
                        'topic': row['topic'],
                        'lr': rows[iter_dates]['prop'],
                        'majority': rows[iter_dates + 1]['prop'],
                        'nupes': rows[iter_dates + 2]['prop'],
                        'rn': rows[iter_dates + 3]['prop'],
                        'lr_supp': rows[iter_dates + index_lrsupp * nb_dates]['prop'],
                        'majority_supp': rows[iter_dates + index_majsupp * nb_dates]['prop'],
                        'nupes_supp': rows[iter_dates + index_nupessupp * nb_dates]['prop'],
                        'rn_supp': rows[iter_dates + index_rnsupp * nb_dates]['prop'],
                        'attentive': rows[iter_dates + index_attentive * nb_dates]['prop'],
                        'general': rows[iter_dates + index_general * nb_dates]['prop'],
                        'media': rows[iter_dates + index_media * nb_dates]['prop'],
                    })
                iter_dates += 1