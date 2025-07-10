import os
from bertopic import BERTopic
import bertopic._save_utils as save_utils
import numpy as np

from utils import (
    load_docs_embeddings,
    DEFAULT_SAVE_SIZE,
    NB_DOCS_SMALL_INFER,
    count_nb_files,
    SBERT_NAME,
)

sbert_name_string = SBERT_NAME.replace("/", "_")

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

input_path, embeddings_path = get_paths("/store/medialex/reproduction_wlwf", "congress")
model_path = "data_prod/topics/bert-model"
party_day_counts=[]

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

topic_model = BERTopic.load(model_path, embedding_model=SBERT_NAME)
topics, probs = topic_model.transform(docs, embeddings)
array_topic = np.array(topics).reshape(-1, 1)
export = np.hstack((array_topic, probs))
header_cols = ["Topic"] + list(range(-1, 92))
header = ",".join(map(str, header_cols))
np.savetxt("topics_probs.csv", export, delimiter=",", fmt="%s", header=header, comments="")
