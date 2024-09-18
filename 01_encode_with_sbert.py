'''
# 01_encode_with_sbert.py

Date: 2024-08-21
Author: Béatrice Mazoyer

Takes csv files with tweets and encode them using Sentence-BERT
(this time, one document = one tweet, as opposed to
the strategy in 01-create-dtm.py)

NOTE: running this script requires access to the original corpus of
tweets, which we cannot share publicly in order to comply with Twitter's
terms of service. For advice on how to re-run this script, please
contact the corresponding author.

All other scripts can be run without running this one first, since we
are providing the files required to replicate the document-feature matrices
we use in the analysis.

'''
import casanova
from tqdm import tqdm
import glob
import os
import sys
import numpy as np
from sklearn.feature_extraction.text import CountVectorizer
from sentence_transformers import SentenceTransformer

from utils import SBERT_NAME, EMB_DIMENSION, count_nb_files, preprocess


embedding_model = SentenceTransformer(SBERT_NAME)

path = sys.argv[1]

docs = [doc for doc in preprocess(path, count_nb_files(path))]

batch_size = 1_000
save_size = 100_000
embeddings = np.zeros((len(docs), EMB_DIMENSION))
save_path = "data_prod/embeddings/tweets_sentence-camembert-large.npz"
max_index = 0

if os.path.isfile(save_path):
    input("""{} already exists, do you want to overwrite file?
          y overwrite file
          n cancel this script
          """.format(save_path))

    if input == "n":
        sys.exit(0)

for file in glob.glob(save_path.replace(".npz", "_*")):
    index = int(file[len(save_path) - 3:-len(".npz")])
    if index > max_index:
        max_index = index

    if index % save_size == 0:
        embeddings[index - save_size : index] = np.load(file)["embeddings"]
    else:
        embeddings[embeddings.shape[0] - (embeddings.shape[0] % save_size) :] = np.load(file)["embeddings"]

print("Loaded {} previously encoded rows".format(np.any(embeddings, axis=1).sum()))

# Encode docs
for i in tqdm(range(max_index, len(docs), batch_size), desc="Encode sentences using CamemBERT large"):
    if i % save_size == 0 and i > 0:
        np.savez_compressed(save_path.replace(".npz", "_" + str(i)), embeddings=embeddings[i - save_size : i])
    embeddings[i:min(len(docs), i + batch_size)] = embedding_model.encode(docs[i:i + batch_size])

np.savez_compressed(save_path.replace(".npz", "_" + str(len(docs))), embeddings=embeddings[len(docs) - (len(docs) % save_size) :])


