import os
import sys
import json
import argparse
from figures_utils import draw_topic_keywords
from hdbscan import HDBSCAN
from umap import UMAP
from bertopic import BERTopic
import bertopic._save_utils as save_utils
import numpy as np
import random
import csv 

from utils import (
    choices,
    count_nb_files,
    count_topics_info,
    create_dir,
    existing_dir_path,
    extract_representative_docs,
    load_docs_embeddings,
    vectorizer,
    write_bertopic_TS,
    write_ids_and_representative_docs,
    SBERT_NAME,
    DEFAULT_SAVE_SIZE,
    RANDOM_SEED,
    NB_DOCS_SMALL_TRAIN,
    NB_DOCS_SMALL_INFER,
)

sbert_name_string = SBERT_NAME.replace("/", "_")
n_tweets_congress = 10
n_tweets_per_pred = 5

def write_sample_BERTOPIC(group, topics, docs, n_tweets,reduced=False):
    if reduced:
        open_path = f"data_prod/topics/bert-sample/reduced/sample_{group}.csv"
    else:
        open_path = f"data_prod/topics/bert-sample/sample_{group}.csv"
    existing_topics = np.unique(topics)
    topics_max = int(max(existing_topics)) + 1 
    index_sample = []
    for topic in range(0, topics_max):
        index_topic = [i for i, val in enumerate(topics) if val == topic]
        if len(index_topic) < n_tweets:
            sample = index_topic + [np.nan] * (n_tweets - len(index_topic))
        else:
            sample = random.sample(index_topic, n_tweets)
        index_sample.append(sample)
    index_sample_list = [value for topic_list in index_sample for value in topic_list]
    list_docs = [docs[i] if not np.isnan(i) else np.nan for i in index_sample_list]
    list_topic = [i for i in range(0, topics_max) for _ in range(n_tweets)]
    print(len(index_sample_list))
    print(len(list_docs))
    print(len(list_topic))
    with open(
           open_path,
            "w", 
        ) as f:
            writer = csv.writer(f)
            writer.writerow(["tweet", "topic"])
            for j in range(len(list_topic)):
                writer.writerow(
                        [
                            list_docs[j],
                            list_topic[j]
                        ]
                )

def get_paths(root, public):
    input_path = os.path.join(root, "data_source", public)
    embeddings_path = os.path.join(
        root,
        "data_prod",
        "embeddings",
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
    "--public",
    help=(
        "List the political group you want to compute in the following format : group1,group2,group3. Choose group names in the following terms : congress, general, attentive, supporter, media"
    ),
    default=",".join(choices),
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

group_list = set(args.public.split(","))

for elem in group_list:
    if elem not in choices:
        raise ValueError(
            "You used an innacurate name of group in your group argument."
            "Choose group names in the following terms : {}".format(choices)
        )

if "congress" in group_list:
    input_path, embeddings_path = get_paths(args.origin_path, "congress")

    if args.small and DEFAULT_SAVE_SIZE < NB_DOCS_SMALL_TRAIN:
        print(
            """Please change value of DEFAULT_SAVE_SIZE or NB_DOCS_SMALL_TRAIN in file utils.py.
            DEFAULT_SAVE_SIZE should be greater than NB_DOCS_SMAL_TRAIN"""
        )
        sys.exit(1)

    hdbscan_model = HDBSCAN(
        min_cluster_size=2 if args.small else 100,
        cluster_selection_epsilon=0.2,
        min_samples=30,
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

    party_day_counts = []

    docs, max_index, embeddings = load_docs_embeddings(
        input_path,
        count_nb_files(input_path),
        embeddings_path,
        args.save_size,
        party_day_counts=party_day_counts,
        apply_unidecode=True,
        small=args.small,
    )

    print(
        "Fitting topic model with params: {}".format(topic_model.hdbscan_model.__dict__)
    )
    topics, probs = topic_model.fit_transform(docs, embeddings)

    new_topics = topic_model.reduce_outliers(
        docs,  # type: ignore
        topics,
        probabilities=probs,  # type: ignore
        strategy="probabilities",
        threshold=0.001,
    )
    topic_model.update_topics(docs, topics=new_topics, vectorizer_model=vectorizer)
    write_sample_BERTOPIC("congress", new_topics, docs, n_tweets_congress, reduced=False)
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

    choices.remove("congress")
    group_list.remove("congress")


if group_list & set(choices):
    for group in group_list:
        # Reload model at each iteration to avoid topics contamination
        #topic_model = BERTopic.load(model_path, embedding_model=SBERT_NAME)
        #topic_ids_list = list(topic_model.get_topic_info()["Topic"])

        input_path, embeddings_path = get_paths(args.origin_path, group)

        party_day_counts = []

        docs, max_index, embeddings = load_docs_embeddings(
            input_path,
            count_nb_files(input_path),
            embeddings_path,
            args.save_size,
            party_day_counts=party_day_counts,
            apply_unidecode=True,
            small=args.small,
            small_size=NB_DOCS_SMALL_INFER,
        )

        #print(f"Predict model from {model_path}")
        topics, probs = topic_model.transform(docs, embeddings)

        write_sample_BERTOPIC("media", topics, docs, n_tweets_per_pred, reduced=False)

        repr_docs_ids = extract_representative_docs(docs, topics, topic_model)

        # Write representative docs for one public in one file
        write_ids_and_representative_docs(
            repr_docs_ids,
            topics,
            party_day_counts,
            group,
            args.origin_path,
            args.small,
            NB_DOCS_SMALL_INFER,
        )
