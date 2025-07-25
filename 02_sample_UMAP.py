import os
import sys
import json
from hdbscan import HDBSCAN
from bertopic import BERTopic
from bertopic.dimensionality import BaseDimensionalityReduction
import numpy as np
import csv
import argparse
import random
from figures_utils import draw_topic_keywords

from utils import (
    count_nb_files,
    count_topics_info,
    create_dir,
    existing_dir_path,
    extract_representative_docs,
    load_docs_embeddings,
    write_ids_and_representative_docs,
    write_bertopic_TS,
    vectorizer,
    SBERT_NAME,
    RANDOM_SEED,
    DEFAULT_SAVE_SIZE,
    NB_DOCS_SMALL_TRAIN,
    NB_DOCS_SMALL_INFER, 
    write_sample_BERTOPIC,
    N_COMPONENT
)

random.seed(RANDOM_SEED)

sbert_name_string = SBERT_NAME.replace("/", "_")

def get_paths(root, public):
    input_path = os.path.join(root, "data_source", public)
    embeddings_path = os.path.join(
        root,
        "data_prod",
        "reduced_embeddings",
        public,
        "{}.npz".format(sbert_name_string),
    )
    return input_path, embeddings_path



parser = argparse.ArgumentParser()

parser.add_argument(
    "--origin_path",
    help="Path to an origin of your code",
    type=existing_dir_path,
    default=os.getcwd(),
)

parser.add_argument(
    "--save-size",
    help="Size of saved files (in embeddings_folder) in number of vectors",
    type=int,
    default=DEFAULT_SAVE_SIZE,
)

parser.add_argument(
    "--small",
    help=(
        "run the script on a reduced number of tweets fixed in utils.py by NB_DOCS_SMALL_TRAIN variable"
    ),
    action="store_true",
)


args = parser.parse_args()

if args.small and DEFAULT_SAVE_SIZE < NB_DOCS_SMALL_TRAIN:
    print(
        """Please change value of DEFAULT_SAVE_SIZE or NB_DOCS_SMALL_TRAIN in file utils.py.
        DEFAULT_SAVE_SIZE should be greater than NB_DOCS_SMAL_TRAIN"""
    )
    sys.exit(1)

print("Train BERTOPIC")
if args.small:
    n_tweets_congress = 1
    n_tweets_per_pred = 1
else:
    n_tweets_congress = 10
    n_tweets_per_pred = 5

hdbscan_model = HDBSCAN(
    min_cluster_size=2 if args.small else 100,
    cluster_selection_epsilon=0.2,
    min_samples=30,
    metric="euclidean",
    cluster_selection_method="eom",
    prediction_data=True,
)

umap_model = BaseDimensionalityReduction() #Empty model 

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

party_day_counts = []
print("Create tweets sample for congress")
input_path, embeddings_path = get_paths(args.origin_path, "congress")
docs, max_index, embeddings = load_docs_embeddings(
    input_path,
    count_nb_files(input_path),
    embeddings_path,
    args.save_size,
    party_day_counts=party_day_counts,
    apply_unidecode=True,
    small=args.small,
    small_size=NB_DOCS_SMALL_TRAIN, 
    PCA = True, 
    n_component=N_COMPONENT
)

print(
    "Fitting topic model with params: {}".format(topic_model.hdbscan_model.__dict__)
)
topics, probs = topic_model.fit_transform(docs, embeddings)
print(topic_model.get_topic_info())

#Random sample for congress
new_topics = topic_model.reduce_outliers(
    docs,  # type: ignore
    topics,
    probabilities=probs,  # type: ignore
    strategy="probabilities",
    threshold=0.001,
)
topic_model.update_topics(docs, topics=new_topics, vectorizer_model=vectorizer)
write_sample_BERTOPIC(args.origin_path, "congress", new_topics, docs, n_tweets_congress)
repr_docs_ids = extract_representative_docs(docs, new_topics, topic_model)

# Write representative docs for one public in one file
write_ids_and_representative_docs(
    repr_docs_ids,
    new_topics,
    party_day_counts,
    "congress",
    args.origin_path,
    args.small,
    NB_DOCS_SMALL_TRAIN,
)

print(topic_model.get_topic_info())
topic_ids_list = []
for i, row in topic_model.get_topic_info().iterrows():
    topic = row["Topic"]
    topic_ids_list.append(topic)
    top_list = topic_model.get_topic(topic)
    top_words, top_ctfidf = zip(*top_list)
    draw_topic_keywords(topic, top_words, top_ctfidf, args.origin_path)

# Create tables in a format adapted to Time Series
topics_info = count_topics_info(topics, party_day_counts, "congress")

# Sort by day so that all resulting files have the same order
party_day_counts = sorted(party_day_counts, key=lambda x: x[2])

# Open one CSV file per topic for congress
write_bertopic_TS(
    topic_ids_list, topics_info, "congress", party_day_counts, args.origin_path
)

topic_model.calculate_probabilities = False #Ne pas calculer quand on prédit, ce n'est pas utile. 

raise ValueError("STOP AT TRAINING")
#Create random sample for predict
print("Loading predict info")

for group in ["media", "attentive", "supporter"]:
    print("Prediction for " + group)
    party_day_counts = []
    topic_ids_list = list(topic_model.get_topic_info()["Topic"])
    input_path, embeddings_path = get_paths(args.origin_path, group)
    docs, max_index, embeddings = load_docs_embeddings(
        input_path,
        count_nb_files(input_path),
        embeddings_path,
        args.save_size,
        party_day_counts=party_day_counts,
        apply_unidecode=True,
        small=args.small,
        small_size=NB_DOCS_SMALL_INFER,
        PCA = True, 
        n_component=N_COMPONENT
    )

    print("Make topics prediction")

    topics_pred, probs = topic_model.transform(docs, embeddings)

    print("Writing")
    repr_docs_ids = extract_representative_docs(docs, topics_pred, topic_model)
    write_ids_and_representative_docs(
        repr_docs_ids,
        topics_pred,
        party_day_counts,
        group,
        args.origin_path,
        args.small,
        NB_DOCS_SMALL_INFER,
    )
    write_sample_BERTOPIC(args.origin_path, group, topics_pred, docs, n_tweets_per_pred)
    print(topic_model.get_topic_info())

    # Time Series Results
    topics_info = count_topics_info(topics, party_day_counts, group)
    # Sort by day so that all resulting files have the same order
    party_day_counts = sorted(party_day_counts, key=lambda x: x[-1])

    # Complete TS data base with the new counts :
    write_bertopic_TS(
        topic_ids_list, topics_info, group, party_day_counts, args.origin_path
    )
