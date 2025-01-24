import os
import argparse
import pandas as pd

from bertopic import BERTopic
import numpy as np

from utils import (
    existing_dir_path,
    count_nb_files,
    load_docs_embeddings,
    count_topics_info,
    write_bertopic_TS, 
    SBERT_NAME,
    DEFAULT_SAVE_SIZE,
    NB_DOCS_SMALL_SCRIPT03,
)

parser = argparse.ArgumentParser()
parser.add_argument(
    "input_path",
    help="Path to a folder or to a .tar.xz archive containing all input csv files that we want to predict topics",
    type=existing_dir_path,
)
parser.add_argument(
    "embeddings_folder",
    help="Path to a folder containing .npz embeddings. It is the output_folder arg in 01_encode_with_sbert.py",
)

parser.add_argument(
    "model_path",
    help = "Path to a folder containing the model. It is the output_folder arg in 02_run_bertopic.py" 
)

parser.add_argument(
    "output_folder",
    help="Path to a folder that will be used to contain the time series data after prediction",
    type=existing_dir_path, #We want to put TS in the same folder than Times Series from script 02 
)

parser.add_argument(
    "--save-size",
    help="Size of saved files (in embeddings_folder) in number of vectors",
    type=int,
    default=DEFAULT_SAVE_SIZE,
)
parser.add_argument(
    "--small",
    help=("run the script on a reduced number of tweets fixed in utils by NB_DOCS_SMALL03 variable"),
    action="store_true",
)
args = parser.parse_args()
sbert_name_string = SBERT_NAME.replace("/", "_")
embeddings_path = os.path.join(
    args.embeddings_folder, "{}.npz".format(sbert_name_string)
)

party_day_counts = []

docs, max_index, embeddings = load_docs_embeddings(
    args.input_path, 
    count_nb_files(args.input_path),
    embeddings_path,
    args.save_size,
    party_day_counts=party_day_counts,
    apply_unidecode=True,
    small=args.small,
    small_size= NB_DOCS_SMALL_SCRIPT03, 
    )


print(f"Predict model from {args.model_path}")

topic_model = BERTopic.load(args.model_path, embedding_model = SBERT_NAME)

topics, probs = topic_model.transform(docs, embeddings)

# Get infos about topic, and extract documents in another way. Warning : the 4 following lines change the topic names and representations. 
documents_df = pd.DataFrame({"Document": docs, "ID": range(len(docs)), "Topic": topics, "Image": None})
topic_model._extract_topics(documents_df, embeddings=embeddings, verbose=True)
topic_model._save_representative_docs(documents_df)
topic_model.get_representative_docs()

print(topic_model.get_topic_info())

#Time Series Results 
last_part = os.path.basename(args.embeddings_folder.rstrip('/')) #Give the group category 

topics_info = count_topics_info(topics, party_day_counts, last_part, topic_model.topics_)

write_bertopic_TS(topics_info, last_part, party_day_counts)