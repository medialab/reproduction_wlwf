import os
import numpy as np
from sklearn.preprocessing import StandardScaler
import sklearn.decomposition._incremental_pca as ipca_module
from sklearn.decomposition import IncrementalPCA
from sklearn.utils import gen_batches
import sklearn.utils
import argparse
from tqdm import tqdm
import matplotlib.pyplot as plt
from figures_utils import draw_PCA_evolve

from utils import (
    choices,
    count_nb_files,
    existing_dir_path, 
    create_dir, 
    load_embeddings,
    format_npz_output,
    EMB_DIMENSION,
    SBERT_NAME,
    DEFAULT_SAVE_SIZE, 
    NB_DOCS_SMALL_TRAIN, 
    NB_DOCS_SMALL_INFER,
    n_component
)

parser = argparse.ArgumentParser()

parser.add_argument(
    "--origin_path",
    help="Path to a the source of the data you collect",
    type=existing_dir_path,
    default=os.getcwd(),
)

parser.add_argument(
    "--small",
    help=(
        "run the script on a reduced number of tweets fixed in utils.py by NB_DOCS_SMALL_TRAIN variable"
    ),
    action="store_true",
)

parser.add_argument(
    "--plot",
    help=(
        "Plot the singular value and the explained variance ratio according the each component index"
    ),
    action="store_true",
)

args = parser.parse_args()

sbert_name_string = SBERT_NAME.replace("/", "_")

def get_embed_paths(root, public):
    embeddings_path = os.path.join(
        root,
        "data_prod",
        "embeddings",
        public,
        "{}.npz".format(sbert_name_string),
    )
    return embeddings_path

def gen_batches_with_tqdm(n, batch_size, min_batch_size=0):
    return tqdm(
        gen_batches(n, batch_size, min_batch_size=n_component),
        desc="Batching",
        total= n // batch_size + 1
    )

ipca_module.gen_batches = gen_batches_with_tqdm

print("Load embeddings")
dict_size = {}
total_row = 0 
for group in choices:
    if args.small: 
        if group == "congress":
            size = NB_DOCS_SMALL_TRAIN
        else: 
            size = NB_DOCS_SMALL_INFER
    else:
        embed_path = os.path.join(args.origin_path, "data_prod", "embeddings", group)
        file_list = []
        for r, d, file in os.walk(embed_path):
            file_list.append(file)
        embed_num = []
        for file_b in file_list:
            for file in file_b:
                split_part = file.split('_')
                last_part = split_part[2]
                number_part = last_part.split('.')
                number = int(number_part[0])
                embed_num.append(number)
        size = max(embed_num)
        del embed_path
        del file_list 
        del embed_num
        del split_part 
        del number_part
    dict_size[group] = size 
    total_row += size 
    del size 



total_matrix = np.empty(shape=(total_row, EMB_DIMENSION)) 
print("Filling a matrix of dimensions :")
print(total_matrix.shape)
del total_row

i=0
for group in choices:
    embed_path = get_embed_paths(args.origin_path, group)
    nb_docs = dict_size[group]
    max_index, total_matrix[i:i+nb_docs] = load_embeddings(embed_path, DEFAULT_SAVE_SIZE, nb_docs, small = args.small)
    i+=nb_docs
    del max_index
    del nb_docs
    del embed_path 


print("Start STD")
scaler = StandardScaler(copy=False)
scaler.fit_transform(total_matrix)
del scaler 

print("Start IPCA")
pca_model = IncrementalPCA(n_components = n_component, batch_size = 100_000) 
reduced_mat = pca_model.fit_transform(total_matrix)
del total_matrix 
if args.plot:
    draw_PCA_evolve(args.origin_path, pca_model)
del pca_model 

print("Start saving process")

start_index = 0 
for group in choices: 
    #Select submatrix by group
    end_index = start_index + dict_size[group] 
    embed_groups = reduced_mat[start_index:end_index]
    start_index = end_index

    #Saving process
    if args.small:
        output_folder = create_dir(os.path.join(args.origin_path, "data_prod", "reduced_embeddings", "small", group))
    else:
        output_folder = create_dir(os.path.join(args.origin_path, "data_prod", "reduced_embeddings", group))

    SAVE_PATH = os.path.join(output_folder, "{}.npz".format(sbert_name_string))
    number_seuils = dict_size[group] // DEFAULT_SAVE_SIZE 
    for i in range(1, number_seuils+2):
        if i == number_seuils + 1:
            start_save = (i-1) * DEFAULT_SAVE_SIZE
            end_save = dict_size[group]
            mat_to_save = embed_groups[start_save:end_save]
            np.savez_compressed(
            format_npz_output(SAVE_PATH, dict_size[group]),
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

