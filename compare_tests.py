from sklearn.metrics.pairwise import cosine_similarity 
import numpy as np
from sentence_transformers import SentenceTransformer
import argparse

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
    count_nb_files,
    preprocess,
    existing_dir_path,
    create_dir,
    load_embeddings,
    DEFAULT_SAVE_SIZE,
    format_npz_output,
)

if args.embed:
    #Reemebed representative docs 
    embedding_model = SentenceTransformer(SBERT_NAME)
    sbert_name_string = SBERT_NAME.replace("/", "_")

    dict_path = {'noload_congress_rep': 'data_prod/tests/noload/representative_docs_congress.csv',
                'noload_media_rep': 'data_prod/tests/noload/representative_docs_media.csv',
                'noload_congress_sample': 'data_prod/tests/noload/sample_congress.csv',
                'noload_media_sample': 'data_prod/tests/noload/sample_media.csv',
                'UMAP_congress_rep': 'data_prod/tests/UMAP/representative_docs_congress.csv',
                'UMAP_media_rep': 'data_prod/tests/UMAP/representative_docs_media.csv',
                'UMAP_congress_sample': 'data_prod/tests/UMAP/sample_congress.csv',
                'UMAP_media_sample': 'data_prod/tests/UMAP/sample_media.csv',
                'FLAT_congress_rep': 'data_prod/tests/FLAT/representative_docs_congress.csv',
                'FLAT_media_rep': 'data_prod/tests/FLAT/representative_docs_media.csv',
                'FLAT_congress_sample': 'data_prod/tests/FLAT/sample_congress.csv',
                'FLAT_media_sample': 'data_prod/tests/FLAT/sample_media.csv',
                }

    for key, value in dict_path.items():
        file_content = key
        input_path = value
        docs = np.genfromtxt(input_path, delimiter=',')
else:
    #Vérifier si ils sont là