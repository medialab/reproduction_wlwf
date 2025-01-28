import os
import argparse
import pandas as pd
import csv
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
    "model_path",
    help = "Path to a folder containing the model. It is the output_folder arg in 02_run_bertopic.py" 
)

parser.add_argument(
    "origin_path",
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
parser.add_argument(
    "--small",
    help=("run the script on a reduced number of tweets fixed in utils by NB_DOCS_SMALL03 variable"),
    action="store_true",
)
args = parser.parse_args()

sbert_name_string = SBERT_NAME.replace("/", "_")

for input_files, group_type in zip(['attentive_public_nort', 'general_public_clean', 'media_IPG', 'supporters_public'] ,['attentive_public', 'general_public', 'medias', 'supporters_public']): 
    input_path = os.path.join(args.origin_path, f"data_source/{input_files}/")
    
    embeddings_path = os.path.join(args.origin_path, 
    f"data_prod/embeddings/{group_type}/", "{}.npz".format(sbert_name_string)
    )
    
    party_day_counts = []

    docs, max_index, embeddings = load_docs_embeddings(
        input_path, 
        count_nb_files(input_path),
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
    topics_info = count_topics_info(topics, party_day_counts, group_type, topic_model.topics_)

    #Complet TS data base with the new counts : 
    write_bertopic_TS(topics_info, group_type, party_day_counts)