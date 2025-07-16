import os
import sys
import json
import hdbscan
from hdbscan import HDBSCAN
from hdbscan.flat import HDBSCAN_flat, approximate_predict_flat, all_points_membership_vectors_flat
from umap import UMAP
from bertopic import BERTopic
from bertopic._utils import MyLogger
from bertopic._bertopic import TopicMapper
import bertopic._save_utils as save_utils
from bertopic.cluster._utils import is_supported_hdbscan
from bertopic.backend._utils import select_backend
import numpy as np
import random
import csv
import torch
import gc
from typing import List, Tuple, Union, Mapping, Any, Callable, Iterable
import pandas as pd 

from utils import (
    choices,
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

NB_DOCS_SMALL_INFER = 100000

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

logger = MyLogger()
logger.configure("WARNING")

#Création d'un BERTopic patché pour gérer HDBSCAN_flat 
def patched_hdbscan_delegator(model, func: str, embeddings: np.ndarray = None):
    # Approximate predict
    if func == "approximate_predict":
        if isinstance(model, hdbscan.HDBSCAN):
            predictions, probabilities = approximate_predict_flat(model, embeddings)
            return predictions, probabilities

    # All points membership
    if func == "all_points_membership_vectors":
        if isinstance(model, hdbscan.HDBSCAN):
            return all_points_membership_vectors_flat(model)

class PatchedBERTopic(BERTopic): #On patche la classe pour créer des fonctions adaptées à HDBSCAN_flat 
    def _cluster_embeddings(
        self,
        umap_embeddings: np.ndarray,
        documents: pd.DataFrame,
        partial_fit: bool = False,
        y: np.ndarray = None,
    ) -> Tuple[pd.DataFrame, np.ndarray]:
        logger.info("Cluster - Start clustering the reduced embeddings")
        old_clusterer = self.hdbscan_model
        self.hdbscan_model = HDBSCAN_flat(
            X=umap_embeddings,
            clusterer=old_clusterer,
            inplace=True,
            cluster_selection_epsilon=0.2
        )
        try:
            self.hdbscan_model.fit(umap_embeddings, y=y)
        except TypeError:
            self.hdbscan_model.fit(umap_embeddings)

        try:
            labels = self.hdbscan_model.labels_
        except AttributeError:
            labels = y
        
        documents["Topic"] = labels
        super()._update_topic_size(documents)

        # Extract probabilities
        probabilities = None
        if hasattr(self.hdbscan_model, "probabilities_"):
            probabilities = self.hdbscan_model.probabilities_

            if self.calculate_probabilities and is_supported_hdbscan(self.hdbscan_model):
                probabilities = patched_hdbscan_delegator(self.hdbscan_model, "all_points_membership_vectors")

        if not partial_fit:
            self.topic_mapper_ = TopicMapper(self.topics_)
        logger.info("Cluster - Completed \u2713")
        return documents, probabilities

    def fit_transform(
        self,
        documents: List[str],
        embeddings: np.ndarray = None,
        images: List[str] = None,
        y: Union[List[int], np.ndarray] = None,
    ) -> Tuple[List[int], Union[np.ndarray, None]]:
    
        doc_ids = range(len(documents)) if documents is not None else range(len(images))
        documents = pd.DataFrame({"Document": documents, "ID": doc_ids, "Topic": None, "Image": images})
        self.embedding_model = select_backend(
            self.embedding_model, language=self.language, verbose=self.verbose
        )

        # Reduce dimensionality and fit UMAP model
        umap_embeddings = super()._reduce_dimensionality(embeddings, y)

        documents, probabilities = self._cluster_embeddings(umap_embeddings, documents, y=y)

        self._extract_topics(
            documents, embeddings=embeddings, verbose=self.verbose, fine_tune_representation=not self.nr_topics
        )
        if self.nr_topics:
            documents = super()._reduce_topics(documents)

        # Save the top 3 most representative documents per topic
        super()._save_representative_docs(documents)

        # In the case of zero-shot topics, probability will come from cosine similarity,
        # and the HDBSCAN model will be removed
        self.probabilities_ = super()._map_probabilities(probabilities, original_topics=True)
        predictions = documents.Topic.to_list()

        return predictions, self.probabilities_

    def transform(
        self,
        documents: Union[str, List[str]],
        embeddings: np.ndarray = None,
        images: List[str] = None,
    ) -> Tuple[List[int], np.ndarray]:

        if isinstance(documents, str) or documents is None:
            documents = [documents]

        logger.info("Dimensionality - Reducing dimensionality of input embeddings.")
        umap_embeddings = self.umap_model.transform(embeddings)
        logger.info("Dimensionality - Completed \u2713")

        # Extract predictions and probabilities if it is a HDBSCAN-like model
        logger.info("Clustering - Approximating new points with `hdbscan_model`")
        predictions, probabilities = patched_hdbscan_delegator(
            self.hdbscan_model, "approximate_predict", umap_embeddings
        )

        # Calculate probabilities
        if self.calculate_probabilities:
            logger.info("Probabilities - Start calculation of probabilities with HDBSCAN")
            probabilities = patched_hdbscan_delegator(self.hdbscan_model, "membership_vector", umap_embeddings)
            logger.info("Probabilities - Completed \u2713")
        logger.info("Cluster - Completed \u2713")

        # Map probabilities and predictions
        probabilities = super()._map_probabilities(probabilities, original_topics=True)
        predictions = super()._map_predictions(predictions)
        return predictions, probabilities

try:
    print("Train BERTOPIC")
    hdbscan_model = HDBSCAN(
        min_cluster_size= 2,#100,
        cluster_selection_epsilon=0.2,
        min_samples= 1, #30,
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

    topic_model = PatchedBERTopic(
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

    #Create random sample for predict
    print("Loading predict info")

    for group in ["media", "attentive", "supporter"]:
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