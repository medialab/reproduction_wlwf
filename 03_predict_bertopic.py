import os
import json
import csv
import random
import argparse
from collections import defaultdict

from bertopic import BERTopic
import bertopic._save_utils as save_utils
import numpy as np

from utils import (
    count_nb_files,
    create_dir,
    existing_dir_path,
    vectorizer,
    preprocess,
    load_embeddings,
    SBERT_NAME,
    DEFAULT_SAVE_SIZE,
    RANDOM_SEED,
)

parser = argparse.ArgumentParser()
parser.add_argument(
    "input_path",
    help="Path to a folder or to a .tar.xz archive containing all input csv files that we want to predict topics",
    type=existing_dir_path,
)
parser.add_argument(
    "embeddings_folder",
    help="Path to a folder containing .npz embeddings. It is the output_folder arg in 01_encode_with_sbert.py",
)

parser.add_argument(
    "model_path",
    help = "Path to a folder containing the model. It is the output_folder arg in 02_run_bertopic.py" #Il faudra peut-être être plus précis sur ce contenu
)

'''parser.add_argument(
    "output_folder",
    help="Path to a folder that will be created and contain the predictions",
    type=create_dir,
)
'''

parser.add_argument(
    "--save-size",
    help="Size of saved files (in embeddings_folder) in number of vectors",
    type=int,
    default=DEFAULT_SAVE_SIZE,
)
parser.add_argument(
    "--small",
    help=("run the script on one week of data"),
    action="store_true",
)
args = parser.parse_args()
sbert_name_string = SBERT_NAME.replace("/", "_")
embeddings_path = os.path.join(
    args.embeddings_folder, "{}.npz".format(sbert_name_string)
)

party_day_counts = []

docs = np.array(
    [
        doc  # Stocke uniquement group_name et filename
        for doc in preprocess(
            args.input_path,
            count_nb_files(args.input_path),
            party_day_counts=party_day_counts,
            apply_unidecode=True,
            small = args.small
        )
    ]
)

max_index, embeddings = load_embeddings(
    embeddings_path,
    args.save_size,
    docs.shape[0],
)

print(f"Predict model from _{args.model_path}")

topic_model = BERTopic.load(args.model_path, embedding_model = SBERT_NAME)

if args.small:
    #indices = random.choices(range(len(docs)), k=1000)
    #docs = docs[indices]
    #embeddings = embeddings[indices]
    topics, probs = topic_model.transform(docs, embeddings)
else:
    topics, probs = topic_model.transform(docs, embeddings)
    
print(topic_model.get_topic_info())

#Résultats sous forme de Time Series
file_index = 0
doc_count, day = party_day_counts[file_index]

print(day)


topics_info = defaultdict(lambda: defaultdict(int))
for i, topic in enumerate(topics):
    #if args.small:
        #doc_index = indices[i]
    #else:
    doc_index = i

    while doc_index >= doc_count:
        file_index += 1

        doc_count, day = party_day_counts[file_index]

    topics_info[topic][day] += 1

# Open one CSV file per topic
last_part = os.path.basename(args.embeddings_folder)

for topic, info in topics_info.items():
    with open(
    os.path.join(
        "data_prod",
        "dashboard",
        "bertopic",
        "data",
        f"bertopic_ts_{topic}_{last_part}.csv", 
    ),
    "w",
    ) as f:
        writer = csv.writer(f)
        writer.writerow(["date", "prop"])
        previous_doc_count = 0
        for doc_count, day in party_day_counts:
            writer.writerow(
                [
                    day,
                    round(info[day] / (doc_count - previous_doc_count), 5),
                ]
            )
            previous_doc_count = doc_count