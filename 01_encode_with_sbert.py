"""
# 01_encode_with_sbert.py

Date: 2024-08-21
Author: Béatrice Mazoyer

Takes csv files with tweets and encode them using Sentence-BERT
(this time, one document = one thread, as opposed to
the strategy in 01-create-dtm.py)

NOTE: running this script requires access to the original corpus of
tweets, which we cannot share publicly in order to comply with Twitter's
terms of service. For advice on how to re-run this script, please
contact the corresponding author.

"""

from tqdm import tqdm
import glob
import os
import sys
import argparse
import numpy as np
from sentence_transformers import SentenceTransformer

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


batch_size = 1_000

parser = argparse.ArgumentParser()
parser.add_argument(
    "input_path",
    help="Path to a folder or to a .tar.xz archive containing all input csv files",
    type=existing_dir_path,
)
parser.add_argument(
    "output_folder",
    help="Path to a folder that will be created and contain all encoded vectors",
    type=create_dir,
)

args = parser.parse_args()
embedding_model = SentenceTransformer(SBERT_NAME)
sbert_name_string = SBERT_NAME.replace("/", "_")

SAVE_PATH = os.path.join(args.output_folder, "{}.npz".format(sbert_name_string))


if os.path.isfile(format_npz_output(SAVE_PATH, DEFAULT_SAVE_SIZE)):
    answer = input(
        """Files in the output folder already exist, do you want to resume from there?
          y resume from last file
          n cancel this script
          """
    ).lower()

    if answer == "n" or answer == "no":
        sys.exit(0)

docs = [doc for doc in preprocess(args.input_path, count_nb_files(args.input_path))]


if len(docs)==0:
    raise ValueError("No csv files found in your directory or compressed file")


# Here, loading means checking what part of the data was already encoded,
# hence the resume_encoding=True
max_index, embeddings = load_embeddings(
    SAVE_PATH, DEFAULT_SAVE_SIZE, len(docs), resume_encoding=True
)


# Encode docs
for i in tqdm(
    range(max_index, len(docs), batch_size),
    desc="Encode sentences using {}".format(sbert_name_string),
):
    if i % DEFAULT_SAVE_SIZE == 0 and i > 0:
        np.savez_compressed(
            format_npz_output(SAVE_PATH, i),
            embeddings=embeddings,
        )

    if i + batch_size >= len(docs):  # last iteration
        embeddings[i % DEFAULT_SAVE_SIZE : i % DEFAULT_SAVE_SIZE + len(docs) % batch_size] = (
            embedding_model.encode(docs[i : i + batch_size])
        )
    else:
        embeddings[i % DEFAULT_SAVE_SIZE : i % DEFAULT_SAVE_SIZE+ batch_size] = (
            embedding_model.encode(docs[i : i + batch_size])
        )

np.savez_compressed(
    format_npz_output(SAVE_PATH, len(docs)),
    embeddings=embeddings[: i % DEFAULT_SAVE_SIZE + len(docs) % batch_size],
)
