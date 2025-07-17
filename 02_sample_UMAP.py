import os
import sys
import json
import hdbscan
from hdbscan import HDBSCAN
from bertopic import BERTopic
from bertopic.dimensionality import BaseDimensionalityReduction
import numpy as np
import random
import csv
import torch
import gc

from utils import (
    count_nb_files,
    count_topics_info,
    create_dir,
    existing_dir_path,
    extract_representative_docs,
    load_docs_embeddings,
    write_ids_and_representative_docs,
    vectorizer,
    SBERT_NAME,
    DEFAULT_SAVE_SIZE,
    RANDOM_SEED,
    NB_DOCS_SMALL_TRAIN,
    NB_DOCS_SMALL_INFER
)

os.environ["PYTORCH_CUDA_ALLOC_CONF"] = "expandable_segments:True"
sbert_name_string = SBERT_NAME.replace("/", "_")
n_tweets_congress = 10
n_tweets_per_pred = 5
party_day_counts = []

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

try:
    print("Train BERTOPIC")
    hdbscan_model = HDBSCAN(
        min_cluster_size= 100,
        cluster_selection_epsilon=0.2,
        min_samples= 30,
        metric="euclidean",
        cluster_selection_method="eom",
        prediction_data=True,
    )

    umap_model = BaseDimensionalityReduction() #Empty model 
    topic_model = BERTopic(
        vectorizer_model=vectorizer,
        hdbscan_model=hdbscan_model,
        umap_model=umap_model,
        # Hyperparameters
        top_n_words=10,
        verbose=True,
        calculate_probabilities=True,
    )

    print("Create tweets sample for congress")
    input_path, embeddings_path = get_paths("/store/medialex/v2_data_reproduction_wlwf", "congress")
    docs, max_index, embeddings = load_docs_embeddings(
        input_path,
        count_nb_files(input_path),
        embeddings_path,
        DEFAULT_SAVE_SIZE,
        party_day_counts=party_day_counts,
        apply_unidecode=True,
        small=False,
        small_size=NB_DOCS_SMALL_TRAIN, 
        UMAP = True
    )

    topics_pred, probs = topic_model.fit_transform(docs, embeddings)
    #Random sample for congress
    new_topics = topic_model.reduce_outliers(
        docs,  # type: ignore
        topics_pred,
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
        "/store/medialex/v2_data_reproduction_wlwf/",
        False,
        NB_DOCS_SMALL_TRAIN,
    )

    raise(ValueError("ONLY TRAIN"))

    #Create random sample for predict
    print("Loading predict info")

    for group in ["media"]: #, "attentive", "supporter"]:
        print(group)
        party_day_counts = []
        input_path, embeddings_path = get_paths("/store/medialex/v2_data_reproduction_wlwf", group)
        docs, max_index, embeddings = load_docs_embeddings(
            input_path,
            count_nb_files(input_path),
            embeddings_path,
            DEFAULT_SAVE_SIZE,
            party_day_counts=party_day_counts,
            apply_unidecode=True,
            small=False,
            small_size=NB_DOCS_SMALL_INFER,
            UMAP = True
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
            "/store/medialex/v2_data_reproduction_wlwf/",
            False,
            NB_DOCS_SMALL_INFER,
        )
        write_sample_BERTOPIC(group, topics_pred, docs, n_tweets_per_pred, reduced=False)

finally:
    print("in")
    gc.collect()
    torch.cuda.empty_cache()
    torch.cuda.ipc_collect()