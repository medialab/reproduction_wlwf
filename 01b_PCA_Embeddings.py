import os
import numpy as np
from sklearn.processing import StandardScaler
from sklearn.decomposition import IncrementalPCA    

from utils import (
    choices,
    count_nb_files,
    preprocess,
    load_docs_embeddings,
    format_npz_output,
    EMB_DIMENSION,
    SBERT_NAME,
    DEFAULT_SAVE_SIZE
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

print("Load embeddings")
sizes = {}
total_row = 0 
for group in choices:
    print(group)
    input_path, embed_path = get_paths("/store/medialex/v2_data_reproduction_wlwf/", group)
    docs = [doc for doc in preprocess(input_path, count_nb_files(input_path))]
    size = len(docs)
    dict_size[group] = size
    total_row += size
    del size
    del docs
    del input_path
    del embed_path

total_matrix = np.empty(shape=(total_row, EMB_DIMENSION)) 

i=0
for group in choices:
    input_path, embed_path = get_paths("/store/medialex/v2_data_reproduction_wlwf/", group)
    nb_docs = dict_size[group]
    max_index, total_matrix[i:i+nb_docs] = load_embeddings(embed_path, DEFAULT_SAVE_SIZE, nb_docs)
    i+=nb_docs
    del max_index
    del nb_docs
    del input_path
    del embed_path 


print("Start STD")
scaler = StandardScaler(copy=False)
total_matrix.fit_transform()
del scaler 

print("Fit Incremental PCA")
pca_model = IncrementalPCA(n_components = 5, batch_size = 500) 
embed_groups = pca_model.fit_transform(total_matrix)
del total_matrix 
del pca_model 

print("Start saving process")
start_index = 0 
for group in choices: 
    print(group)
    #Select submatrix by group
    end_index = start_index + sizes[group] 
    embed_groups = reduced_mat[start_index:end_index]
    start_index = end_index

    #Saving process
    output_folder = "/store/medialex/v2_data_reproduction_wlwf/data_prod/reduced_embeddings/" + group + "/"
    SAVE_PATH = os.path.join(output_folder, "{}.npz".format(sbert_name_string))
    number_seuils = sizes[group] // DEFAULT_SAVE_SIZE 
    for i in range(1, number_seuils+2):
        if i == number_seuils + 1:
            start_save = (i-1) * DEFAULT_SAVE_SIZE
            end_save = sizes[group]
            mat_to_save = embed_groups[start_save:end_save]
            np.savez_compressed(
            format_npz_output(SAVE_PATH, sizes[group]),
            embeddings=mat_to_save
            )
        else: 
            nb_save = DEFAULT_SAVE_SIZE * i 
            start_save = (i-1) * DEFAULT_SAVE_SIZE
            mat_to_save = embed_groups[start_save:nb_save]
            np.savez_compressed(
            format_npz_output(SAVE_PATH, nb_save),
            embeddings=mat_to_save
            )

