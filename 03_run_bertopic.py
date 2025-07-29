import os
import sys
import argparse
from figures_utils import draw_topic_keywords
from hdbscan import HDBSCAN
from bertopic import BERTopic
from bertopic.dimensionality import BaseDimensionalityReduction
import numpy as np


from utils import (
    choices,
    count_nb_files,
    count_topics_info,
    existing_dir_path,
    extract_representative_docs,
    preprocess,
    vectorizer,
    write_bertopic_TS,
    write_ids_and_representative_docs,
    DEFAULT_SAVE_SIZE,
    NB_DOCS_SMALL_TRAIN,
)


def get_paths(root, public):
    input_path = os.path.join(root, "data_source", public)
    embeddings_path = os.path.join(
        root,
        "data_prod",
        "reduced_embeddings",
        public,
        "umap.npz",
    )
    return input_path, embeddings_path


parser = argparse.ArgumentParser()

parser.add_argument(
    "--origin_path",
    help="Path to an origin of your code",
    type=existing_dir_path,
    default=os.getcwd(),
)

parser.add_argument(
    "--public",
    help=(
        "List the political group you want to compute in the following format : group1,group2,group3. Choose group names in the following terms : congress, general, attentive, supporter, media"
    ),
    default=",".join(choices),
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
        "run the script on a reduced number of tweets fixed in utils.py by NB_DOCS_SMALL_TRAIN variable"
    ),
    action="store_true",
)


args = parser.parse_args()

group_list = args.public.split(",")

for elem in group_list:
    if elem not in choices:
        raise ValueError(
            "You used an innacurate name of group in your group argument."
            "Choose group names in the following terms : {}".format(choices)
        )

empty_dimensionality_model = BaseDimensionalityReduction()

if args.small:
    vectorizer.min_df = 1

hdbscan_model = HDBSCAN(
    min_cluster_size=2 if args.small else 100,
    cluster_selection_epsilon=0.1,
    min_samples=5,
    metric="euclidean",
    cluster_selection_method="eom",
    prediction_data=True,
)

topic_model = BERTopic(
    vectorizer_model=vectorizer,
    hdbscan_model=hdbscan_model,
    umap_model=empty_dimensionality_model,
    # Hyperparameters
    top_n_words=10,
    verbose=True,
    calculate_probabilities=True,
)


if args.small and DEFAULT_SAVE_SIZE < NB_DOCS_SMALL_TRAIN:
    print(
        """Please change value of DEFAULT_SAVE_SIZE or NB_DOCS_SMALL_TRAIN in file utils.py.
        DEFAULT_SAVE_SIZE should be greater than NB_DOCS_SMAL_TRAIN"""
    )
    sys.exit(1)

# Put congress at the beginning of group_list
if "congress" in group_list:
    group_list.remove("congress")
else:
    print(
        "BERTopic model will be trained on congress data, then infered on other publics"
    )
group_list = ["congress"] + group_list

for group in group_list:
    input_path, embeddings_path = get_paths(args.origin_path, group)

    party_day_counts = []

    docs = np.array(
        [
            doc
            for doc in preprocess(
                input_path,
                count_nb_files(input_path),
                party_day_counts,
                apply_unidecode=True,
                write_files=False,
                small=args.small,
            )
        ]
    )

    embeddings = np.load(embeddings_path)["embeddings"]

    if group == "congress":
        print(
            "Fitting topic model with params: {}".format(
                topic_model.hdbscan_model.__dict__
            )
        )
        topics, probs = topic_model.fit_transform(docs, embeddings)
        print(topic_model.get_topic_info())

        topics = topic_model.reduce_outliers(
            docs,
            topics,
            probabilities=probs,
            strategy="probabilities",
            threshold=0.001,
        )
        topic_model.update_topics(docs, topics=topics, vectorizer_model=vectorizer)

        topic_ids_list = []
        for i, row in topic_model.get_topic_info().iterrows():
            topic = row["Topic"]
            topic_ids_list.append(topic)
            top_list = topic_model.get_topic(topic)
            top_words, top_ctfidf = zip(*top_list)
            draw_topic_keywords(topic, top_words, top_ctfidf, args.origin_path)
    else:
        topics, probs = topic_model.transform(docs, embeddings)

    print(topic_model.get_topic_info())

    repr_docs_ids = extract_representative_docs(docs, topics, topic_model)

    # Write representative docs for one public in one file
    write_ids_and_representative_docs(
        repr_docs_ids,
        topics,
        party_day_counts,
        group,
        args.origin_path,
        args.small,
        NB_DOCS_SMALL_TRAIN,
    )

    # Create tables in a format adapted to Time Series
    topics_info = count_topics_info(topics, party_day_counts, group)

    # Sort by day so that all resulting files have the same order
    party_day_counts = sorted(party_day_counts, key=lambda x: x[-1])

    # Open one CSV file per topic for congress
    write_bertopic_TS(
        topic_ids_list, topics_info, group, party_day_counts, args.origin_path
    )
