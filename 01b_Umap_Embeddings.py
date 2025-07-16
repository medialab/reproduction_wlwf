from umap import UMAP
import os
import numpy as np

from utils import (
    choices,
    count_nb_files,
    load_docs_embeddings,
    format_npz_output,
    EMB_DIMENSION,
    SBERT_NAME,
    DEFAULT_SAVE_SIZE,
    RANDOM_SEED
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

n_comp = 5 
umap_model = UMAP(
    n_neighbors=15,
    n_components=n_comp,
    min_dist=0.0,
    metric="cosine",
    low_memory=False,
    random_state=RANDOM_SEED,
)

print("Load embeddings")
embeddings = np.empty(shape = (0, EMB_DIMENSION))
sizes = {}
for group in choices:
    print(group)
    input_path, embed_path = get_paths("/store/medialex/v2_data_reproduction_wlwf/", group)
    docs, max_index, new_embeddings = load_docs_embeddings(input_path, count_nb_files(input_path), embed_path, DEFAULT_SAVE_SIZE, resume_encoding=False, small=False)
    print(new_embeddings.shape)
    embeddings = np.vstack((embeddings, new_embeddings))
    sizes[group] = len(docs)
print("Run UMAP")
reduced_mat = umap_model.fit_transform(embeddings)

start_index = 0 

print("Start saving process")
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

