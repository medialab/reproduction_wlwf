import openpyxl
import pandas as pd 
import argparse
from cdbw import CDbw
from hdbscan.validity import validity_index
from bertopic import BERTopic
import numpy as np

from utils import(
    existing_dir_path,
    SBERT_NAME
)

parser = argparse.ArgumentParser()

parser.add_argument(
    "model_path",
    help = "Path to a folder containing the model",
    type=existing_dir_path,
)

parser.add_argument(
    "--load_annot",
    help="If you want to create the dataframe for human annotation, use this option",
    action = 'store_true',
)
args = parser.parse_args()

topic_model = BERTopic.load(args.model_path, embedding_model = SBERT_NAME)

if args.load_annot:
    list_df = []
    nb_annotateurs = 9

    for i in range(nb_annotateurs):
        df_annotateur = pd.read_excel('data_source/BERTopic_evaluation_collective.xlsx', sheet_name = i+1)
        list_df.append(df_annotateur)
    #Il faudra gérer ces duplicates 

    df_merged = pd.concat(list_df, ignore_index=True) 
    df_merged = df_merged.rename(columns={'Topic' : 'Topic_doc', 'pertinence' : 'pertinence_doc', 'coherence' : 'coherence_doc', 'Topic.1' : 'Topic_gen', 'pertinence.1' : 'pertinence_gen', 'coherence.1' : 'coherence_gen'})

    df_merged[['Document', 'Topic_doc', 'Top_n_words', 'Representative_Docs', 'pertinence_doc', 'coherence_doc']].to_csv("data_prod/topics/bert-model/evaluation_doc_merged.csv")
    df_merged[['Topic_gen', 'pertinence_gen', 'coherence_gen', 'nom']].to_csv("data_prod/topics/bert-model/evaluation_topic_merged.csv")

#Coherence measure for internal validity
umap_embeddings = np.genfromtxt('data_prod/embeddings/umap/umap_embeddings.csv', delimiter=',')
X = umap_embeddings
topics = [int(t) for t in topic_model.topics_]
labels = np.asarray(topics)
subset_X = X[labels == 1, :]

cdbw_score = CDbw(X, labels)
dbcv_score = validity_index(X, labels, d=5)
print(f"CDbw measure score is: {cdbw_score}")
print(f"DBCV measure score is: {dbcv_score}")

#Ground truth measure 
