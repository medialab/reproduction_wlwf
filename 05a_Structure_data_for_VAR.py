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

def list_dates(start_date: str, end_date: str):
    start = datetime.strptime(start_date, "%Y-%m-%d")
    end = datetime.strptime(end_date, "%Y-%m-%d")
    delta = (end - start).days

    return [(start + timedelta(days=i)).strftime("%Y-%m-%d") for i in range(delta + 1)]

# Exemple d'utilisation :
dates = list_dates("2022-06-20", "2023-03-14")
nb_dates = len(dates)

write_general_TS('bertopic', nb_dates, 'nb_tweets', dates)
write_general_TS('bertopic', nb_dates, 'prop', dates)
#write_general_TS('lda', nb_dates, 'prop', dates)