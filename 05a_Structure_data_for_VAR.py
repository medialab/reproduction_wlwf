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
    

    for date in 

    count_topic += 5 #To obtain the files of the next topics next time

for file in files_TS:

    filename = os.path.basename(file)
    topic_number = re.search(r'-?\d+', filename)[0]
    reader = casanova.reader(open(file))

    for row in reader:
        if count_iter ==0: #Create date column one time
            casanova.writer()
            count_first += 1



'''

Récupérer tous les fichiers csv 
Les reformer pour avoir la grosse data base générale
Feature à mettre : ne pas prendre en compte le fichier général créé dans les calculs ``

Note : il faudra aussi sorted par date les supporters lorsque je pourrai pull après le pull request de BERTopic3

Piste : ajout de party_day_counts = sorted(party_day_counts, key=lambda x: x[2]) dans script 02 ligne 217 pour trier par date et non plus pas parti les time series, mais des mofig
Voir salade avec béatrice pour gérer ça (27/1)'''