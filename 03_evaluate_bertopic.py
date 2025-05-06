import pandas as pd
from bertopic import BERTopic
import os
from sklearn.metrics import homogeneity_completeness_v_measure
from sentence_transformers import SentenceTransformer
import kaleido

from utils import existing_dir_path, SBERT_NAME, reduce_doc_size, count_nb_files, preprocess

topic_model = BERTopic.load("data_prod/topics/bert-model", embedding_model=SBERT_NAME)

input_path = "/store/medialex/reproduction_wlwf/data_source/congress"

docs = [doc for doc in preprocess(input_path, count_nb_files(input_path))]
hierarchical_topics = topic_model.hierarchical_topics(docs)
fig = topic_model.visualize_hierarchy(hierarchical_topics=hierarchical_topics)
fig.write_image("data_prod/figures/hdbscan_hierarchy.png")