import sys
import json
import time
import random

from hdbscan import HDBSCAN
from bertopic import BERTopic
import bertopic._save_utils as save_utils
import numpy as np
import matplotlib.pyplot as plt

from utils import count_nb_files, vectorizer, preprocess, SBERT_NAME

hdbscan_model = HDBSCAN(min_cluster_size=3, metric='euclidean', cluster_selection_method='eom', prediction_data=False)

topic_model = BERTopic(
    vectorizer_model=vectorizer,
    hdbscan_model=hdbscan_model,
    nr_topics=100,
    # Hyperparameters
    top_n_words=10,
    verbose=True,
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

path = sys.argv[1]
load_path = "data_prod/embeddings/tweets_from_deputesXVI_220620-230313_sentence-camembert-large.npz"


embeddings = np.load(load_path)["embeddings"]

docs = np.array(doc for doc in preprocess(path, count_nb_files(path), apply_unidecode=True))

topic_model.fit(docs, embeddings)

#Save model
topic_model.save("data_prod/bertopic/fit_on_deputesXVI_220620-230313_sentence-camembert-large", serialization="safetensors", save_ctfidf=True, save_embedding_model=SBERT_NAME)

