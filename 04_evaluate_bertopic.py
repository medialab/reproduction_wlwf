import openpyxl
import pandas as pd 
import argparse
from cdbw_copy import (
    comb_noise_lab,
    gen_dist_func,
    prep,
    rep,
    closest_rep,
    compactness,
    separation
)
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

print("Load BERTOPIC model")

topic_model = BERTopic.load(args.model_path, embedding_model = SBERT_NAME)

if args.load_annot:
    print("Load annotation data")
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
print("Take umap embeddings and topics")
umap_embeddings = np.genfromtxt('data_prod/embeddings/umap/umap_embeddings.csv', delimiter=',')
X = umap_embeddings
topics = [int(t) for t in topic_model.topics_]
labels = np.asarray(topics)
subset_X = X[labels == 1, :]

#print('Calculate DBCV Score')
#dbcv_score = validity_index(X, labels, d=5)
#print(f"DBCV measure score is: {dbcv_score}")


def CDbw(X, labels, metric="euclidean", alg_noise='comb', intra_dens_inf=False, s=3, multipliers=False):
    if len(set(labels)) < 2 or len(set(labels)) > len(X) - 1:
        raise ValueError("No. of unique labels must be > 1 and < n_samples")
    if s < 2:
        raise ValueError("Parameter s must be > 2")
    elif alg_noise == 'bind':
        labels = bind_noise_lab(X, labels, metric=metric)
    elif alg_noise == 'comb':
        labels = comb_noise_lab(labels)
    elif alg_noise == 'filter':
        labels, X = filter_noise_lab(X, labels)
    labels = np.asarray(labels)
    distvec = gen_dist_func(metric)
    n_clusters, stdev, dimension, n_points_in_cl, n_max, coord_in_cl, labels_in_cl = prep(X, labels)
    print("Rep")
    mean_arr, n_rep, n_rep_max, rep_in_cl = rep(n_clusters, dimension, n_points_in_cl, coord_in_cl, labels_in_cl)
    print(f"nombre clusters : f'{n_clusters})
    print(rep_in_cl)
    print(n_rep)
    print(distvec)
    print("Closest rep")
    middle_point, dist_min, n_cl_rep = closest_rep(X, n_clusters, rep_in_cl, n_rep, metric, distvec)
    print("Try")
    try:
        print('TRY OK')
        a_rep_shell = art_rep(X, n_clusters, rep_in_cl, n_rep, n_rep_max, mean_arr, s, dimension)
    except ValueError:
        print("Except run")
        return 0
    compact, cohesion = compactness(n_clusters, stdev, a_rep_shell, n_rep, n_points_in_cl, s, coord_in_cl, n_max,
                                    n_rep_max, metric)
    print("Check compact")
    if (np.isinf(compact) or np.isnan(compact)) and not intra_dens_inf:
        return 0
    print("Calculate sep")
    sep = separation(n_clusters, stdev, middle_point, dist_min, n_cl_rep, n_points_in_cl, coord_in_cl)
    print("Final calculation")
    cdbw = compact * cohesion * sep
    print("Decide return multipliers")
    if multipliers:
        return compact, cohesion, sep, cdbw
    else:
        return cdbw


print('Calculate CDbw Score')
cdbw_score = CDbw(X, labels, alg_noise='comb')
print(f"CDbw measure score is: {cdbw_score}")


#Ground truth measure 
