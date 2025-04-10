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
    write_representative_docs,
    SBERT_NAME,
    DEFAULT_SAVE_SIZE,
    RANDOM_SEED,
    NB_DOCS_SMALL_TRAIN,
    NB_DOCS_SMALL_INFER,
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

parser = argparse.ArgumentParser()

parser.add_argument(
    "model_path",
    help="Path to a folder that will be created and contain the BERTopic model",
    type=existing_dir_path,
)

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

args = parser.parse_args()

save_utils.save_ctfidf_config = save_ctfidf_config


input_path, embeddings_path = get_paths(args.origin_path, "congress")
party_day_counts = []

docs, max_index, embeddings = load_docs_embeddings(
    input_path,
    count_nb_files(input_path),
    embeddings_path,
    args.save_size,
    party_day_counts=party_day_counts,
    apply_unidecode=True,
    small=False,
)

print("Reduce model with congress")

topic_model = BERTopic.load(args.model_path, embedding_model=SBERT_NAME)
topic_model.reduce_topics(docs, nr_topics=30)
topics = topic_model.topics_
probs = topic_model.probabilities_

topic_model.save(
    "data_prod/topics/bert-model/reduced",
    serialization="safetensors",
    save_ctfidf=True,
    save_embedding_model=SBERT_NAME,
)

repr_docs_ids = extract_representative_docs(docs, topics, topic_model)

print(topic_model.get_topic_info())
topic_ids_list = []
for i, row in topic_model.get_topic_info().iterrows():
    topic = row["Topic"]
    topic_ids_list.append(topic)
    top_list = topic_model.get_topic(topic)
    top_words, top_ctfidf = zip(*top_list)
    draw_topic_keywords(topic, top_words, top_ctfidf, args.origin_path, reduced=True)

write_representative_docs(
    repr_docs_ids,
    party_day_counts,
    "congress",
    args.origin_path,
    False,
    NB_DOCS_SMALL_TRAIN,
    reduced=True
)

 # Create tables in a format adapted to Time Series
topics_info = count_topics_info(topics, party_day_counts, "congress")

# Sort by day so that all resulting files have the same order
party_day_counts = sorted(party_day_counts, key=lambda x: x[2])

# Open one CSV file per topic for congress
write_bertopic_TS(topic_ids_list, topics_info, "congress", party_day_counts, args.origin_path, reduced=True)

print("Start new predictions")

group_list = ["attentive", "general", "supporter", "media"]

for group in group_list:
    print("Run prediction for", group)
    # Reload model at each iteration to avoid topics contamination
    topic_model = BERTopic.load("data_prod/topics/bert-model/reduced", embedding_model=SBERT_NAME)
    topic_ids_list = list(topic_model.get_topic_info()["Topic"])

    input_path, embeddings_path = get_paths(args.origin_path, group)

    party_day_counts = []

    docs, max_index, embeddings = load_docs_embeddings(
        input_path,
        count_nb_files(input_path),
        embeddings_path,
        args.save_size,
        party_day_counts=party_day_counts,
        apply_unidecode=True,
        small=False,
        small_size=NB_DOCS_SMALL_INFER,
    )

    topics, probs = topic_model.transform(docs, embeddings)

    # Write representative docs for one public in one file
    if group == "media":
        repr_docs_ids = extract_representative_docs(docs, topics, topic_model)
        
        write_representative_docs(
            repr_docs_ids,
            party_day_counts,
            group,
            args.origin_path,
            False,
            NB_DOCS_SMALL_INFER,
            reduced=True
        )

    print(topic_model.get_topic_info())

    # Time Series Results
    topics_info = count_topics_info(topics, party_day_counts, group)
    # Sort by day so that all resulting files have the same order
    party_day_counts = sorted(party_day_counts, key=lambda x: x[-1])

    # Complete TS data base with the new counts :
    write_bertopic_TS(
        topic_ids_list, topics_info, group, party_day_counts, args.origin_path, reduced=True
    )



