import os
import csv
import json
import random
import argparse
from collections import defaultdict
from figures_utils import draw_topic_keywords
from hdbscan import HDBSCAN
from umap import UMAP
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
    help="Path to a folder or to a .tar.xz archive containing all input csv files",
    type=existing_dir_path,
)
parser.add_argument(
    "embeddings_folder",
    help="Path to a folder containing .npz embeddings. It is the output_folder arg in 01_encode_with_sbert.py",
)
parser.add_argument(
    "output_folder",
    help="Path to a folder that will be created and contain the BERTopic model",
    type=create_dir,
)
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

hdbscan_model = HDBSCAN(
    min_cluster_size=2 if args.small else 100,
    metric="euclidean",
    cluster_selection_method="eom",
    prediction_data=True,
)

umap_model = UMAP(
    n_neighbors=15,
    n_components=5,
    min_dist=0.0,
    metric="cosine",
    low_memory=False,
    random_state=RANDOM_SEED,
)

if args.small:
    vectorizer.min_df = 1

topic_model = BERTopic(
    vectorizer_model=vectorizer,
    hdbscan_model=hdbscan_model,
    umap_model=umap_model,
    # Hyperparameters
    top_n_words=10,
    verbose=True,
    calculate_probabilities=True,
)


class NpEncoder(json.JSONEncoder):
    """From https://stackoverflow.com/a/57915246/6053864"""

    def default(self, obj):
        if isinstance(obj, np.integer):
            return int(obj)
        if isinstance(obj, np.floating):
            return float(obj)
        if isinstance(obj, np.ndarray):
            return obj.tolist()
        return super(NpEncoder, self).default(obj)


def save_ctfidf_config(model, path):
    """Save parameters to recreate CountVectorizer and c-TF-IDF."""
    config = {}

    # Recreate ClassTfidfTransformer
    config["ctfidf_model"] = {
        "bm25_weighting": model.ctfidf_model.bm25_weighting,
        "reduce_frequent_words": model.ctfidf_model.reduce_frequent_words,
    }

    # Recreate CountVectorizer
    cv_params = model.vectorizer_model.get_params()
    del cv_params["tokenizer"], cv_params["preprocessor"], cv_params["dtype"]
    if not isinstance(cv_params["analyzer"], str):
        del cv_params["analyzer"]

    config["vectorizer_model"] = {
        "params": cv_params,
        "vocab": model.vectorizer_model.vocabulary_,
    }

    with path.open("w") as f:
        json.dump(config, f, indent=2, cls=NpEncoder)


save_utils.save_ctfidf_config = save_ctfidf_config

party_day_counts = []

docs = np.array(
    [
        doc  # Stocke uniquement group_name et filename
        for doc in preprocess(
            args.input_path,
            count_nb_files(args.input_path),
            party_day_counts=party_day_counts,
            apply_unidecode=True,
        )
    ]
)

max_index, embeddings = load_embeddings(
    embeddings_path,
    args.save_size,
    docs.shape[0],
)

print("Fitting topic model with params: {}".format(topic_model.hdbscan_model.__dict__))

if args.small:
    random.seed(a=RANDOM_SEED)
    indices = sorted(random.sample(range(len(docs)), k=1000))
    docs = docs[indices]
    embeddings = embeddings[indices]

    topics, probs = topic_model.fit_transform(docs, embeddings)
    topic_model.save(
        os.path.join(args.output_folder, "small"),
        serialization="safetensors",
        save_ctfidf=True,
        save_embedding_model=SBERT_NAME,
    )
    print(topic_model.get_topic_info())
else:
    topics, probs = topic_model.fit_transform(docs, embeddings)
    print(topic_model.get_topic_info())
    # Save model
    topic_model.save(
        args.output_folder,
        serialization="safetensors",
        save_ctfidf=True,
        save_embedding_model=SBERT_NAME,
    )

new_topics = topic_model.reduce_outliers(
    docs,  # type: ignore
    topics,
    probabilities=probs,  # type: ignore
    strategy="probabilities",
    threshold=0.01,
)
topic_model.update_topics(docs, topics=new_topics, vectorizer_model=vectorizer)

print(topic_model.get_topic_info())

for i, row in topic_model.get_topic_info().iterrows():
    topic = row["Topic"]
    top_list = topic_model.get_topic(topic)
    top_words, top_ctfidf = zip(*top_list)
    draw_topic_keywords(topic, top_words, top_ctfidf)

# Création du tableau au format adapté pour les graphiques TS

file_index = 0
"""
party_day_count is a list with the following structure:
[
    (29, 'lr', '2022-06-20'),
    (46, 'lr', '2022-06-21'),
    (83, 'lr', '2022-06-22'),
    (117, 'lr', '2022-06-23'),
    ...
]
"""
doc_count, party, day = party_day_counts[file_index]


topics_info = defaultdict(lambda: defaultdict(lambda: defaultdict(int)))
for i, topic in enumerate(topics):
    if args.small:
        doc_index = indices[i]
    else:
        doc_index = i

    while doc_index >= doc_count:
        file_index += 1

        doc_count, party, day = party_day_counts[file_index]

    topics_info[topic][party][day] += 1

# Open one CSV file per topic
for topic, info in topics_info.items():
    with open(
        os.path.join(
            "data_prod",
            "dashboard",
            "bertopic",
            "data",
            "bertopic_ts_{}.csv".format(topic),
        ),
        "w",
    ) as f:
        writer = csv.writer(f)
        writer.writerow(["date", "party", "prop"])
        previous_doc_count = 0
        for doc_count, party, day in party_day_counts:
            writer.writerow(
                [
                    day,
                    party,
                    round(info[party][day] / (doc_count - previous_doc_count), 5),
                ]
            )
            previous_doc_count = doc_count
