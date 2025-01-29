import os
import sys
import json
import csv
import argparse
from figures_utils import draw_topic_keywords
from hdbscan import HDBSCAN
from umap import UMAP
from bertopic import BERTopic
import bertopic._save_utils as save_utils
import numpy as np
#import pandas as pd (put in comment because pandas part code is currently in comments)

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
    NB_DOCS_SMALL_SCRIPT03
)

parser = argparse.ArgumentParser()

parser.add_argument(
    "model_path",
    help="Path to a folder that will be created and contain the BERTopic model",
    type=create_dir,
)

parser.add_argument(
    "--origin_path",
    help="Path to an origin of your code",
    type=existing_dir_path,
    default=os.getcwd(), 
)

parser.add_argument(
    "--group",
    help=(
        "List the groups you want to compute in the following format : group1,group2,group3. Choose group names in the following terms : congress, general_public, attentive_public, supporters_public, medias"
    ), 
    default='congress,general_public,attentive_public,supporters_public,medias', 
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

group_list = args.group.split(",")
group_list = set(group_list)
normal_elem = ['congress', 'general_public', 'attentive_public', 'supporters_public', 'medias']

for elem in group_list:
    if elem not in normal_elem:
        raise ValueError('You used an innacurate name of group in your group argument. Choose group names in the following terms : congress, general_public, attentive_public, supporters_public, medias')

sbert_name_string = SBERT_NAME.replace("/", "_")

if 'congress' in group_list:
    input_path = os.path.join(args.origin_path, "data_source/congress/")
    embeddings_path = os.path.join(args.origin_path,
        "data_prod/embeddings/congress/", "{}.npz".format(sbert_name_string)
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

    party_day_counts = []

    docs, max_index, embeddings = load_docs_embeddings(
        input_path,
        count_nb_files(input_path),
        embeddings_path,
        args.save_size,
        party_day_counts=party_day_counts,
        apply_unidecode=True,
        small=args.small,
    )

    print("Fitting topic model with params: {}".format(topic_model.hdbscan_model.__dict__))
    topics, probs = topic_model.fit_transform(docs, embeddings)
    print(topic_model.get_topic_info())

    if args.small:
        output_folder = os.path.join(args.model_path, "_small")
    else:
        output_folder = args.model_path

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

    topics_info = count_topics_info(topics, party_day_counts, "congress")
    party_day_counts = sorted(party_day_counts, key=lambda x: x[2])

    # Open one CSV file per topic for congress

    write_bertopic_TS(topics_info, "congress", party_day_counts, args.origin_path)

    normal_elem.remove('congress')
    group_list.remove('congress')

else:
    if os.listdir(args.model_path)==[]:
        raise ValueError('Not trained model was found in the model path directory')

infer = False

for elem in group_list:
    if elem in normal_elem:
        infer = True

if infer==True:
    if args.small:
        model_path = os.path.join(args.model_path, "_small/")
        topic_model = BERTopic.load(model_path, embedding_model = SBERT_NAME) 
    else:
        model_path = args.model_path
        topic_model = BERTopic.load(args.model_path, embedding_model = SBERT_NAME)

    for group in group_list:
        input_path = os.path.join(args.origin_path, f"data_source/{group}/")
        
        embeddings_path = os.path.join(args.origin_path, 
        f"data_prod/embeddings/{group}/", "{}.npz".format(sbert_name_string)
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
            
        print(f"Predict model from {model_path}")
        topics, probs = topic_model.transform(docs, embeddings)

        print(topic_model.get_topic_info())

        #Time Series Results 
        topics_info = count_topics_info(topics, party_day_counts, group, topic_model.topics_)

        #Complet TS data base with the new counts : 
        write_bertopic_TS(topics_info, group, party_day_counts, args.origin_path)
'''
To add representative docs 

        # Get infos about topic, and extract documents in another way. Warning : the 4 following lines change the topic names and representations. 
        documents_df = pd.DataFrame({"Document": docs, "ID": range(len(docs)), "Topic": topics, "Image": None})
        topic_model._extract_topics(documents_df, embeddings=embeddings, verbose=True)
        topic_model._save_representative_docs(documents_df)
        topic_model.get_representative_docs()
'''
        