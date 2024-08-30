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
import numpy as np
from sklearn.feature_extraction.text import CountVectorizer
from sentence_transformers import SentenceTransformer

from utils import TWEETS_FOLDER, SBERT_NAME, EMB_DIMENSION, count_nb_files, vectorizer, preprocess


embedding_model = SentenceTransformer(SBERT_NAME)

docs = preprocess(TWEETS_FOLDER)

batch_size = 1000
embeddings = np.zeros((len(docs), EMB_DIMENSION))
save_path = "data/embeddings/tweets_from_deputesXVI_220620-230313_sentence-camembert-large.npz"
max_index = 0

# Check if a temporary dump exists
for file_path in glob.glob(save_path.replace(".npz", "*.npy")):
    if file_path != save_path:
        idx = int(file_path[len(save_path) - len(".npy") +1:-len(".npy")])
        if idx > max_index:
            max_index = idx

# Load temporary dump
if max_index > 0:
    print("Loading previous archive")
    embeddings = np.load(save_path.replace(".npz", "-{}.npy".format(max_index)))
    print(embeddings[max_index - 1:max_index + 1])

# Encode docs
for i in tqdm(range(max_index, len(docs), batch_size), desc="Encode sentences using CamemBERT large"):
    try:
        embeddings[i:min(len(docs), i + batch_size)] = embedding_model.encode(docs[i:i + batch_size])
    except RuntimeError as e:
        np.save(save_path.replace(".npz", "-{}.npy".format(i)), embeddings)
        print(e)
        raise

# Save encoded docs
np.savez_compressed(save_path, embeddings=embeddings)

# Remove other dumps
for file_path in glob.glob(save_path.replace(".npz", "*.npy")):
    if file_path != save_path:
        os.remove(file_path)


