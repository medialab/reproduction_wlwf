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

group_list = set(args.public.split(","))

for elem in group_list:
    if elem not in choices:
        raise ValueError(
            "You used an innacurate name of group in your group argument."
            "Choose group names in the following terms : {}".format(choices)
        )

if "congress" in group_list:
    input_path, embeddings_path = get_paths(args.origin_path, "congress")

    if args.small and DEFAULT_SAVE_SIZE < NB_DOCS_SMALL_TRAIN:
        print(
            """Please change value of DEFAULT_SAVE_SIZE or NB_DOCS_SMALL_TRAIN in file utils.py.
            DEFAULT_SAVE_SIZE should be greater than NB_DOCS_SMAL_TRAIN"""
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

    print(
        "Fitting topic model with params: {}".format(topic_model.hdbscan_model.__dict__)
    )
    topics, probs = topic_model.fit_transform(docs, embeddings)
    print(topic_model.get_topic_info())

    if args.small:
        output_folder = create_dir(os.path.join(args.model_path, "_small"))
    else:
        output_folder = create_dir(args.model_path)

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

    repr_docs_ids = extract_representative_docs(docs, topics, topic_model)

    # Write representative docs for one public in one file
    write_representative_docs(
        repr_docs_ids,
        party_day_counts,
        "congress",
        args.origin_path,
        args.small,
        NB_DOCS_SMALL_TRAIN,
    )

    print(topic_model.get_topic_info())
    topic_ids_list = []
    for i, row in topic_model.get_topic_info().iterrows():
        topic = row["Topic"]
        topic_ids_list.append(topic)
        top_list = topic_model.get_topic(topic)
        top_words, top_ctfidf = zip(*top_list)
        draw_topic_keywords(topic, top_words, top_ctfidf, args.origin_path)

    # Create tables in a format adapted to Time Series
    topics_info = count_topics_info(topics, party_day_counts, "congress")

    # Sort by day so that all resulting files have the same order
    party_day_counts = sorted(party_day_counts, key=lambda x: x[2])

    # Open one CSV file per topic for congress
    write_bertopic_TS(
        topic_ids_list, topics_info, "congress", party_day_counts, args.origin_path
    )

    choices.remove("congress")
    group_list.remove("congress")

elif os.listdir(args.model_path) == []:
    raise ValueError("Not trained model was found in the model path directory")


if group_list & set(choices):
    if args.small:
        model_path = os.path.join(args.model_path, "_small")
    else:
        model_path = args.model_path
    topic_model = BERTopic.load(model_path, embedding_model=SBERT_NAME)
    topic_ids_list = list(topic_model.get_topic_info()["Topic"])

    for group in group_list:
        input_path, embeddings_path = get_paths(args.origin_path, group)

        party_day_counts = []

        docs, max_index, embeddings = load_docs_embeddings(
            input_path,
            count_nb_files(input_path),
            embeddings_path,
            args.save_size,
            party_day_counts=party_day_counts,
            apply_unidecode=True,
            small=args.small,
            small_size=NB_DOCS_SMALL_INFER,
        )

        print(f"Predict model from {model_path}")
        topics, probs = topic_model.transform(docs, embeddings)

        repr_docs_ids = extract_representative_docs(docs, topics, topic_model)

        # Write representative docs for one public in one file
        write_representative_docs(
            repr_docs_ids,
            party_day_counts,
            group,
            args.origin_path,
            args.small,
            NB_DOCS_SMALL_INFER,
        )

        print(topic_model.get_topic_info())

        # Time Series Results
        topics_info = count_topics_info(topics, party_day_counts, group)
        # Sort by day so that all resulting files have the same order
        party_day_counts = sorted(party_day_counts, key=lambda x: x[-1])

        # Complete TS data base with the new counts :
        write_bertopic_TS(
            topic_ids_list, topics_info, group, party_day_counts, args.origin_path
        )
