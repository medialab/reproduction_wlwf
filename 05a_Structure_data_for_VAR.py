'''
The aim of this script is to create a database to an accomodating format for VAR model implementation
'''

import os
import numpy as np 
import casanova
import re
from collections import default_dict

from utils import (
    count_nb_files, 
    iter_on_files,
)


files_TS =list((iter_on_files("data_prod/dashboard/bertopic/data"), count_nb_files("data_prod/dashboard/bertopic/data"))[1])

count_topic = 0
nb_topics = 129 #Change this number the number of topics obtained with the model
topics = (i-1 in range(nb_topics)) 

with open('data_prod/var/bertopic/general_TS.csv') as f: 
    writer = casanova.writer(f, fieldnames=['date', 'topic', 'lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive_public', 'general_public', 'medias'])
    writer.writeheader()

for topic in topics:
    topics_files = files_ts[count_topic, count_topic + 5]
    reader0 = topics_files[0] #attentive 
    reader1 = topics_files[1] #deputes
    reader2 = topics_files[2] #general
    reader3 = topics_files[3] #medias
    reader4 = topics_files[4] #supporters

    date_col = reader0.headers.date
    prop_col_index = reader0.headers.prop

    iter_deputes = (row[prop_col_index] for row in reader1)
    iter_supp = (row[prop_col_index] for row in reader4)

    for row1, row2, row3 in zip(reader0, reader2, reader3):
        writer.writerow({
                    'date': row1[date_col],
                    'topic': topic_number,
                    'lr': next(iter_deputes),
                    'majority': next(iter_deputes),
                    'nupes': next(iter_deputes),
                    'rn': next(iter_deputes),
                    'lr_supp': next(iter_supp),
                    'majority_supp': next(iter_supp),
                    'nupes_supp': next(iter_supp),
                    'rn_supp': next(iter_supp),
                    'attentive_public': row1[prop_col_index],
                    'general_public': row2[prop_col_index],
                    'medias': row3[prop_col_index]
                })

    count_topic += 5 #To obtain the files of the next topics next time


'''
Note : il faudra aussi sorted par date les deputes supporters lorsque je pourrai pull après le pull request de BERTopic3

Piste : ajout de party_day_counts = sorted(party_day_counts, key=lambda x: x[2]) dans script 02 ligne 217 pour trier par date et non plus pas parti les time series, mais des mofig
Voir salade avec béatrice pour gérer ça (27/1)'''