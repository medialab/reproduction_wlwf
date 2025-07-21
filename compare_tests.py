from sklearn.metrics.pairwise import cosine_similarity 
import numpy as np
import argparse
import casanova
import os
from sentence_transformers import SentenceTransformer

parser = argparse.ArgumentParser()

parser.add_argument(
    "--embed",
    help=(
        "Encode representative and sample docs"
    ),
    action="store_true",
)

args = parser.parse_args()

from utils import (
    SBERT_NAME,
    reduce_doc_size, 
    count_nb_files,
    preprocess,
    existing_dir_path,
    create_dir,
    load_embeddings,
    EMB_DIMENSION,
    DEFAULT_SAVE_SIZE,
    format_npz_output,
)

#Gérer UMAP topic 97 qui a qu'un seul tweet
dict_path = {'noload_congress_rep': 'data_prod/tests/noload/representative_docs_congress_sorted.csv',
            'noload_media_rep': 'data_prod/tests/noload/representative_docs_media_sorted.csv',
            'noload_congress_sample': 'data_prod/tests/noload/sample_congress.csv',
            'noload_media_sample': 'data_prod/tests/noload/sample_media.csv',
            'UMAP_congress_rep': 'data_prod/tests/UMAP/representative_docs_congress_sorted.csv',
            'UMAP_media_rep': 'data_prod/tests/UMAP/representative_docs_media_sorted.csv',
            'UMAP_congress_sample': 'data_prod/tests/UMAP/sample_congress.csv',
            'UMAP_media_sample': 'data_prod/tests/UMAP/sample_media.csv',
            'FLAT_congress_rep': 'data_prod/tests/FLAT/representative_docs_congress_sorted.csv',
            'FLAT_media_rep': 'data_prod/tests/FLAT/representative_docs_media_sorted.csv',
            'FLAT_congress_sample': 'data_prod/tests/FLAT/sample_congress.csv',
            'FLAT_media_sample': 'data_prod/tests/FLAT/sample_media.csv',
            }

if args.embed:
    #Reemebed representative docs 
    embedding_model = SentenceTransformer(SBERT_NAME)
    for key, value in dict_path.items():
        file_content = key
        print(file_content)
        input_path = value
        reader = casanova.reader(input_path)
        if key.endswith("sample"):
            text_pos = reader.headers.tweet
        else: 
            text_pos = reader.headers.text
        docs = [row[text_pos] for row in reader]
        for i in range(len(docs)):
            docs[i] = reduce_doc_size(docs[i], length=500)
        embeddings = embedding_model.encode(docs)
        print(embeddings.shape)
        name_file = file_content + "embeddings_array.npy"
        save_path = os.path.join("data_prod", "tests", "embeddings", name_file)
        with open(save_path, 'wb') as f:
            np.save(f, embeddings)

else:
    for key in dict_path.keys():
        name_file = key + "embeddings_array.npy"
        save_path = os.path.join("data_prod", "tests", "embeddings", name_file)
        if not os.path.exists(save_path):
            raise ValueError("Embed you data before please")

#Calculate cosine similarity by pair of congress-media
print("Start iter")
index_iter= [0, 2, 4, 6, 8, 10]
keys_list = list(dict_path.keys())
for i in index_iter: 
    name_embed_congress = keys_list[i] + "embeddings_array.npy"
    name_embed_media = keys_list[i+1] + "embeddings_array.npy"
    path_embed_congress = os.path.join("data_prod", "tests", "embeddings", name_embed_congress)
    path_embed_media = os.path.join("data_prod", "tests", "embeddings", name_embed_media)
    with open(path_embed_congress, 'rb') as f:
        congress_embeddings = np.load(f)
    with open(path_embed_media, 'rb') as f:
        media_embeddings = np.load(f)
    if keys_list[i+1].endswith("sample"):
        add_med=  5
    else: 
        add_med = 10
    number_iter = int(congress_embeddings.shape[0] / 10)
    cong_idx = 0
    med_idx = 0
    cosine_by_topic = np.empty(shape=(number_iter, 2))
    for iter in range(0, number_iter):
        #Take submatrix
        cong_end = cong_idx + 10
        med_end = med_idx + add_med 
        subcong = congress_embeddings[cong_idx:cong_end]
        submed = media_embeddings[med_idx:med_end]
        #Calculus
        sim_mat = cosine_similarity(subcong, submed)
        mean_score = np.mean(sim_mat)
        cosine_by_topic[iter] = [int(iter), mean_score]
        #Update index
        cong_idx += 10
        med_idx += add_med
    splitted_key = keys_list[i].split('_')
    name_save_file = "results_" + splitted_key[0] + "_" + splitted_key[2] + ".csv"
    cosine_save_path = os.path.join("data_prod", "tests", name_save_file)
    np.savetxt(cosine_save_path, cosine_by_topic, delimiter = ",", header="topic,mean_cosine_score",  fmt=['%d', '%.6f'])

