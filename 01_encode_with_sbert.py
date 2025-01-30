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
    "group",
    help="Choose the group you want to encode : congress, attentive, media, general, supporters",
)

parser.add_argument(
    "--origin_path",
    help="Path to a the source of the data you collect",
    type=existing_dir_path,
    default=os.getcwd(),
)


args = parser.parse_args()
embedding_model = SentenceTransformer(SBERT_NAME)
sbert_name_string = SBERT_NAME.replace("/", "_")

output_folder = create_dir(os.path.join(args.origin_path, f"data_prod/embeddings/{args.group}"))

SAVE_PATH = os.path.join(output_folder, "{}.npz".format(sbert_name_string))

if os.path.isfile(format_npz_output(SAVE_PATH, DEFAULT_SAVE_SIZE)):
    answer = input(
        """Files in the output folder already exist, do you want to resume from there?
          y resume from last file
          n cancel this script
          """
    ).lower()

    if answer == "n" or answer == "no":
        sys.exit(0)

input_path = os.path.join(args.origin_path, f"data_source/{args.group}")

docs = [doc for doc in preprocess(input_path, count_nb_files(input_path))]

if len(docs) == 0:
    if count_nb_files(input_path) == 0:
        raise ValueError(f"No csv files found in {input_path}")


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
        embeddings[
            i % DEFAULT_SAVE_SIZE : i % DEFAULT_SAVE_SIZE + len(docs) % batch_size
        ] = embedding_model.encode(docs[i : i + batch_size])
    else:
        embeddings[i % DEFAULT_SAVE_SIZE : i % DEFAULT_SAVE_SIZE + batch_size] = (
            embedding_model.encode(docs[i : i + batch_size])
        )

np.savez_compressed(
    format_npz_output(SAVE_PATH, len(docs)),
    embeddings=embeddings[: i % DEFAULT_SAVE_SIZE + len(docs) % batch_size],
)
