import os
import sys
import json
from hdbscan import HDBSCAN
from umap import UMAP
from bertopic import BERTopic
import bertopic._save_utils as save_utils
import numpy as np
import random
import csv
import torch
import gc

from utils import (
    choices,
    count_nb_files,
    count_topics_info,
    create_dir,
    existing_dir_path,
    extract_representative_docs,
    load_docs_embeddings,
    vectorizer,
    SBERT_NAME,
    DEFAULT_SAVE_SIZE,
    RANDOM_SEED,
    NB_DOCS_SMALL_TRAIN,
    NB_DOCS_SMALL_INFER,
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
        "embeddings",
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

#model_path = "data_prod/topics/bert-model"
#model_path_reduced = "data_prod/topics/bert-model/reduced"

try:
    print("Train BERTOPIC")
    hdbscan_model = HDBSCAN(
        min_cluster_size= 100,
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

    topic_model = BERTopic(
        vectorizer_model=vectorizer,
        hdbscan_model=hdbscan_model,
        umap_model=umap_model,
        # Hyperparameters
        top_n_words=10,
        verbose=True,
        calculate_probabilities=False,
    )
    #topic_model = BERTopic.load(model_path, embedding_model=SBERT_NAME)
    #topic_model_r = BERTopic.load(model_path_reduced, embedding_model=SBERT_NAME)

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
        small_size=NB_DOCS_SMALL_TRAIN
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
    #write_sample_BERTOPIC("congress", topic_pred_r, docs, n_tweets_congress, reduced=True)

    #Create random sample for predict
    print("Loading predict info")

    for group in ["media", "attentive", "supporter"]:
        print(group)
        input_path, embeddings_path = get_paths("/store/medialex/v2_data_reproduction_wlwf", group)
        docs, max_index, embeddings = load_docs_embeddings(
            input_path,
            count_nb_files(input_path),
            embeddings_path,
            DEFAULT_SAVE_SIZE,
            party_day_counts=None,
            apply_unidecode=True,
            small=False,
            small_size=NB_DOCS_SMALL_TRAIN,
        )

        print("Make topics prediction")

        topic_pred, probs = topic_model.transform(docs, embeddings)
        #topic_pred_r, probs = topic_model_r.transform(docs, embeddings)

        print("Writing")

        write_sample_BERTOPIC(group, topic_pred, docs, n_tweets_per_pred, reduced=False)
        #write_sample_BERTOPIC(group, topic_pred_r, docs, n_tweets_per_pred, reduced=True)

finally:
    print("in")
    gc.collect()
    torch.cuda.empty_cache()
    torch.cuda.ipc_collect()