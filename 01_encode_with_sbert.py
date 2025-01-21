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
parser.add_argument(
    "--save-size",
    help="Size of saved files in number of vectors",
    type=int,
    default=DEFAULT_SAVE_SIZE,
)
args = parser.parse_args()

embedding_model = SentenceTransformer(SBERT_NAME)
sbert_name_string = SBERT_NAME.replace("/", "_")

SAVE_PATH = os.path.join(args.output_folder, "{}.npz".format(sbert_name_string))


def format_output(size):
    return SAVE_PATH.replace(".npz", "_" + str(size) + ".npz")


if os.path.isfile(format_output(args.save_size)):
    answer = input(
        """Files in the output folder already exist, do you want to resume from there?
          y resume from last file
          n cancel this script
          """
    ).lower()

    if answer == "n" or answer == "no":
        sys.exit(0)

elif os.path.isfile(format_output(DEFAULT_SAVE_SIZE)):
    raise ValueError(
        """Files in the output folder have a different save_size than the input save_size ({}).
        It is impossible to resume from there.""".format(args.save_size)
    )

else:
    files_contain_save_size = []
    for file in glob.glob(SAVE_PATH.replace(".npz", "_*")):
        index = int(file[len(SAVE_PATH) - 3 : -len(".npz")])
        files_contain_save_size.append(index == args.save_size)
    if len(files_contain_save_size) > 0 and not any(files_contain_save_size):
        raise ValueError(
            """Files in the output folder have a different save_size than the input save_size ({}).
            It is impossible to resume from there.""".format(args.save_size)
        )


docs = [doc for doc in preprocess(args.input_path, count_nb_files(args.input_path))]

# Here, loading means checking what part of the data was already encoded,
# hence the resume_encoding=True
max_index, embeddings = load_embeddings(
    SAVE_PATH,
    args.save_size,
    len(docs),
    resume_encoding=True
)


# Encode docs
for i in tqdm(
    range(max_index, len(docs), batch_size),
    desc="Encode sentences using {}".format(sbert_name_string),
):
    if i % args.save_size == 0 and i > 0:
        np.savez_compressed(
            format_output(i),
            embeddings=embeddings,
        )

    if i + batch_size >= len(docs): # last iteration
        embeddings[i % args.save_size: i % args.save_size + len(docs) % batch_size ] = embedding_model.encode(
        docs[i : i + batch_size]
    )
    else:
        embeddings[i % args.save_size: i % args.save_size + batch_size ] = embedding_model.encode(
            docs[i : i + batch_size]
        )

np.savez_compressed(
    format_output(len(docs)),
    embeddings=embeddings[:i % args.save_size + len(docs) % batch_size],
)
