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

model_path = "data_prod/topics/bert-model"
party_day_counts=[]

docs, max_index, embeddings = load_docs_embeddings(
            "/store/medialex/reproduction_wlwf/data_source/congress",
            count_nb_files("/store/medialex/reproduction_wlwf/data_source/congress"),
            "/store/medialex/reproduction_wlwf/data_prod/embeddings/congress",
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
