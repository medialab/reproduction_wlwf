import os
import json
import random
import argparse
import pandas as pd

from figures_utils import draw_topic_keywords 
from hdbscan import HDBSCAN
from umap import UMAP
from bertopic import BERTopic
import bertopic._save_utils as save_utils
import numpy as np

from utils import (
    count_nb_files,
    create_dir,
    existing_dir_path,
    vectorizer,
    preprocess,
    load_embeddings,
    SBERT_NAME,
    DEFAULT_SAVE_SIZE,
    RANDOM_SEED
)

parser = argparse.ArgumentParser()
parser.add_argument(
    "input_path",
    help="Path to a folder or to a .tar.xz archive containing all input csv files",
    type=existing_dir_path,
)
parser.add_argument(
    "embeddings_folder",
    help="Path to a folder containing .npz embeddings. It is the output_folder arg in 01_encode_with_sbert.py",
)
parser.add_argument(
    "output_folder",
    help="Path to a folder that will be created and contain the BERTopic model",
    type=create_dir,
)
parser.add_argument(
    "--save-size",
    help="Size of saved files (in embeddings_folder) in number of vectors",
    type=int,
    default=DEFAULT_SAVE_SIZE,
)
parser.add_argument(
    "--small",
    help=("run the script on one week of data"),
    action="store_true",
)
args = parser.parse_args()
embeddings_path = os.path.join(
    args.embeddings_folder, "tweets_sentence-camembert-large.npz"
)

hdbscan_model = HDBSCAN(
    min_cluster_size=10,
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
    vectorizer.min_df = 3

topic_model = BERTopic(
    vectorizer_model=vectorizer,
    hdbscan_model=hdbscan_model,
    umap_model=umap_model,
    nr_topics=10 if args.small else 100,
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

docs_infos = np.array(
    [
        (doc, group_name, filename)  # Stocke uniquement group_name et filename
        for doc, group_name, filename in preprocess(
            args.input_path, count_nb_files(args.input_path), apply_unidecode=True
        )
    ]
)

docs = docs_infos[:,0]

max_index, embeddings = load_embeddings(
    embeddings_path,
    args.save_size,
    docs.shape[0],
)

print("Fitting topic model with params: {}".format(topic_model.hdbscan_model.__dict__))

if args.small:
    random.seed(a=RANDOM_SEED)
    indices = random.choices(range(len(docs)), k=1000)
    docs = docs[indices]
    docs_infos = docs_infos[indices]
    embeddings = embeddings[indices]

    topics, probs = topic_model.fit_transform(docs, embeddings)
    topic_model.save(
        os.path.join(args.output_folder, "small"),
        serialization="safetensors",
        save_ctfidf=True,
        save_embedding_model=SBERT_NAME,
    )

else:
    topics, probs = topic_model.fit_transform(docs, embeddings)

    # Save model
    topic_model.save(
        args.output_folder,
        serialization="safetensors",
        save_ctfidf=True,
        save_embedding_model=SBERT_NAME,
    )

new_topics = topic_model.reduce_outliers(
    docs, topics, probabilities=probs, strategy="probabilities", threshold=0.3
)
topic_model.update_topics(docs, topics=new_topics, vectorizer_model=vectorizer)

print(topic_model.get_topic_info())

for i, row in topic_model.get_topic_info().iterrows():
    topic = row["Topic"]
    top_list = topic_model.get_topic(topic)
    top_words, top_ctfidf = zip(*top_list)
    draw_topic_keywords(
        topic, top_words, top_ctfidf, sorted(range(len(top_words)), reverse=True)
    )

#Création du tableau au format adapté pour les graphiques TS d'Antoine 
#Avec preprocess : on découpe les documents en tweet unique, et la date est dans le nom, et le groupe et le nom du dossier. Ca pourrait donc être un premier pas. 
#Topics contient le topic associé à chaque tweet 

#Création d'une matrice contenant le group et le numéro de chaque document
infos = docs_infos[:, 1:3]

#Ajout du topic associé à chaque tweet daté et associé à un groupe
infos = np.c_[infos, topics]

print(infos)

#Conversion d'infos en datafrale 
df = pd.DataFrame(infos, columns=['party', 'date', 'topic'])


#df = pd.DataFrame(infos, columns=['party', 'date', 'topic']) #Conversion to facilitate operations 
df['date'] = df['date'].str.extract(r'(\d{8})')
df['date'] = df['date'].str.slice(0, 4) + '-' + df['date'].str.slice(4, 6) + '-' + df['date'].str.slice(6, 8)

df = df.groupby(['party', 'date', 'topic']).size().reset_index(name='count') #Aggrégation des données pour mettre ensemble les jours/topics/groupe similaires en leur associant leur occurence dans la dataframe 

df['total'] = df.groupby(['party', 'date'])['count'].transform('sum') #Compte le nombre de tweets totaux par groupe 

df['prop'] = df['count'] / df['total'] #Calcul de proportion
df.drop(['count', 'total'], axis=1, inplace=True)  #On enlève les colonnes inutiles 
df.to_csv('data_prod/dashboard/files/df_forTS.csv')

#Ajouter des sort by pour vérifier 
