import os
import argparse
import numpy as np

from cuml.manifold import UMAP
# from umap import UMAP


from utils import (
    choices,
    create_dir,
    existing_dir_path,
    load_embeddings,
    get_max_index,
    get_paths,
    DEFAULT_SAVE_SIZE,
    RANDOM_SEED,
    EMB_DIMENSION,
)

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

umap_model = UMAP(
    n_neighbors=15,
    n_components=5,
    min_dist=0.0,
    metric="cosine",
    low_memory=False,
    init="random",
    random_state=RANDOM_SEED,
)

args = parser.parse_args()

group_list = set(args.public.split(","))

for public in group_list:
    if public not in choices:
        raise ValueError(
            "You used an innacurate name of group in your group argument."
            "Choose group names in the following terms : {}".format(choices)
        )

nb_docs_per_public = {}
paths_per_public = {"text": {}, "embs": {}}
for public in group_list:
    paths_per_public["text"][public], paths_per_public["embs"][public] = get_paths(
        args.origin_path, public
    )
    max_index = get_max_index(paths_per_public["embs"][public])
    nb_docs_per_public[public] = max_index

all_public_matrix = np.empty((sum(nb_docs_per_public.values()), EMB_DIMENSION))

start_index = 0
for public in group_list:
    end_index, embeddings = load_embeddings(
        paths_per_public["embs"][public],
        args.save_size,
        nb_docs_per_public[public],
    )
    all_public_matrix[start_index : start_index + end_index] = embeddings
    start_index += end_index

print(f"Run dimensionality reduction with {umap_model}")
reduced_matrix = umap_model.fit_transform(all_public_matrix)

start_index = 0
for public in group_list:
    output_path = create_dir(
        os.path.join(args.origin_path, "data_prod", "reduced_embeddings", public)
    )
    end_index = nb_docs_per_public[public]
    np.savez_compressed(
        os.path.join(output_path, "umap.npz"),
        embeddings=reduced_matrix[start_index : start_index + end_index],
    )
    start_index += end_index
