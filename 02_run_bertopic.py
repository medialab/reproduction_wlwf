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
from typing import List, Tuple, Union
from bertopic._utils import (
     MyLogger,
    check_documents_type,
    check_embeddings_shape,
)
from bertopic.backend._utils import select_backend
from bertopic.cluster import BaseCluster
from sklearn.metrics.pairwise import cosine_similarity
import pandas as pd 

from utils import (
    count_nb_files,
    create_dir,
    existing_dir_path,
    vectorizer,
    load_docs_embeddings,
    count_topics_info,
    write_bertopic_TS,
    SBERT_NAME,
    DEFAULT_SAVE_SIZE,
    RANDOM_SEED,
    NB_DOCS_SMALL_SCRIPT02,
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
    help=(
        "run the script on a reduced number of tweets fixed in utils.py by NB_DOCS_SMALL_SCRIPT02 variable"
    ),
    action="store_true",
)
args = parser.parse_args()
sbert_name_string = SBERT_NAME.replace("/", "_")
embeddings_path = os.path.join(
    args.embeddings_folder, "{}.npz".format(sbert_name_string)
)

if args.small and DEFAULT_SAVE_SIZE < NB_DOCS_SMALL_SCRIPT02:
    print(
        """Please change value of DEFAULT_SAVE_SIZE or NB_DOCS_SMALL_SCRIPT02 in file utils.py.
        DEFAULT_SAVE_SIZE should be greater than NB_DOCS_SMALL_SCRIPT02"""
    )
    sys.exit(1)

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

def fit_transform(
    self,
    documents: List[str],
    embeddings: np.ndarray = None,
    images: List[str] = None,
    y: Union[List[int], np.ndarray] = None,
) -> Tuple[List[int], Union[np.ndarray, None]]:
    """Fit the model and allow to return umap_embeddings 
    """
    if documents is not None:
        check_documents_type(documents)
        check_embeddings_shape(embeddings, documents)

    doc_ids = range(len(documents)) if documents is not None else range(len(images))
    documents = pd.DataFrame({"Document": documents, "ID": doc_ids, "Topic": None, "Image": images})

    # Extract embeddings
    if embeddings is None:
        logger.info("Embedding - Transforming documents to embeddings.")
        self.embedding_model = select_backend(self.embedding_model, language=self.language, verbose=self.verbose)
        embeddings = self._extract_embeddings(
            documents.Document.values.tolist(),
            images=images,
            method="document",
            verbose=self.verbose,
        )
        logger.info("Embedding - Completed \u2713")
    else:
        if self.embedding_model is not None:
            self.embedding_model = select_backend(
                self.embedding_model, language=self.language, verbose=self.verbose
            )

    # Guided Topic Modeling
    if self.seed_topic_list is not None and self.embedding_model is not None:
        y, embeddings = self._guided_topic_modeling(embeddings)

    # Reduce dimensionality and fit UMAP model
    umap_embeddings = self._reduce_dimensionality(embeddings, y)

    # Zero-shot Topic Modeling
    if self._is_zeroshot():
        documents, embeddings, assigned_documents, assigned_embeddings = self._zeroshot_topic_modeling(
            documents, embeddings
        )
        # Filter UMAP embeddings to only non-assigned embeddings to be used for clustering
        umap_embeddings = self.umap_model.transform(embeddings)

    if len(documents) > 0:  # No zero-shot topics matched
        # Cluster reduced embeddings
        documents, probabilities = self._cluster_embeddings(umap_embeddings, documents, y=y)
        if self._is_zeroshot() and len(assigned_documents) > 0:
            documents, embeddings = self._combine_zeroshot_topics(
                documents, embeddings, assigned_documents, assigned_embeddings
            )
    else:
        # All documents matches zero-shot topics
        documents = assigned_documents
        embeddings = assigned_embeddings
    topics_before_reduction = self.topics_

    # Sort and Map Topic IDs by their frequency
    if not self.nr_topics:
        documents = self._sort_mappings_by_frequency(documents)

    # Create documents from images if we have images only
    if documents.Document.values[0] is None:
        custom_documents = self._images_to_text(documents, embeddings)

        # Extract topics by calculating c-TF-IDF
        self._extract_topics(custom_documents, embeddings=embeddings)
        self._create_topic_vectors(documents=documents, embeddings=embeddings)

        # Reduce topics
        if self.nr_topics:
            custom_documents = self._reduce_topics(custom_documents)

        # Save the top 3 most representative documents per topic
        self._save_representative_docs(custom_documents)
    else:
        # Extract topics by calculating c-TF-IDF
        self._extract_topics(documents, embeddings=embeddings, verbose=self.verbose)

        # Reduce topics
        if self.nr_topics:
            documents = self._reduce_topics(documents)

        # Save the top 3 most representative documents per topic
        self._save_representative_docs(documents)

    # In the case of zero-shot topics, probability will come from cosine similarity,
    # and the HDBSCAN model will be removed
    if self._is_zeroshot() and len(assigned_documents) > 0:
        self.hdbscan_model = BaseCluster()
        sim_matrix = cosine_similarity(embeddings, np.array(self.topic_embeddings_))

        if self.calculate_probabilities:
            probabilities = sim_matrix
        else:
            # Use `topics_before_reduction` because `self.topics_` may have already been updated from
            # reducing topics, and the original probabilities are needed for `self._map_probabilities()`
            probabilities = sim_matrix[
                np.arange(len(documents)),
                np.array(topics_before_reduction) + self._outliers,
            ]

    # Resulting output
    self.probabilities_ = self._map_probabilities(probabilities, original_topics=True)
    predictions = documents.Topic.to_list()

    return predictions, self.probabilities_, umap_embeddings

BERTopic.fit_transform = fit_transform

party_day_counts = []

docs, max_index, embeddings = load_docs_embeddings(
    args.input_path,
    count_nb_files(args.input_path),
    embeddings_path,
    args.save_size,
    party_day_counts=party_day_counts,
    apply_unidecode=True,
    small=args.small,
)

print("Fitting topic model with params: {}".format(topic_model.hdbscan_model.__dict__))
topics, probs, umap_embeddings = topic_model.fit_transform(docs, embeddings)
np.savetxt('data_prod/embeddings/umap/umap_embeddings.csv', umap_embeddings, delimiter =",")
print(topic_model.get_topic_info())

if args.small:
    output_folder = os.path.join(args.output_folder, "_small")
else:
    output_folder = args.output_folder

topic_model.save(
    output_folder,
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

# Create tables in a format adapted to Time Series

topics_info = count_topics_info(topics, party_day_counts, "deputes")

# Open one CSV file per topic

write_bertopic_TS(topics_info, "deputes", party_day_counts)
