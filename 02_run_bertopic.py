'''
# 02-run-BertTopic.py

Date: 2024-08-21
Author: Béatrice Mazoyer

Takes csv files with tweets and run BERTopic 
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
import random
import time
import casanova
from tqdm import tqdm
import glob
import os
import numpy as np
from sklearn.feature_extraction.text import CountVectorizer
from sentence_transformers import SentenceTransformer

from bertopic import BERTopic

from utils import TWEETS_FOLDER, count_nb_files, vectorizer
os.environ["TOKENIZERS_PARALLELISM"] = "false"
EMB_DIMENSION = 1024

def preprocess(root):
    nb_files = count_nb_files(TWEETS_FOLDER)
    docs = []
    for filename in tqdm(glob.iglob(root + '/**/*.csv', recursive=True), total=nb_files, desc="Read csv files"):
        reader = casanova.reader(filename)
        text_pos = reader.headers.text
        for row in reader:
            try:
                 docs.append(row[text_pos].replace("\n", " "))
            except IndexError:
                print(filename)
                print(row)
                continue
    return docs


def st_time(func):
    """
        decorator to compute the total time of a function
    """

    def st_func(*args, **keyArgs):
        t1 = time.time()
        r = func(*args, **keyArgs)
        t2 = time.time()
        print("Function=%s, Time=%s" % (func.__name__, t2 - t1))
        return r

    return st_func

@st_time
def compute_topics(docs, embeddings):
    print("Nb documents: ", len(docs))
    topics, probs = topic_model.fit_transform(docs, embeddings)
    print(topic_model.get_topic_info())

embedding_model = SentenceTransformer("dangvantuan/sentence-camembert-large")

topic_model = BERTopic(

  # Pipeline models
  vectorizer_model=vectorizer,

  # Hyperparameters
  top_n_words=10,
  verbose=True

)

docs = preprocess(TWEETS_FOLDER)

batch_size = 1000
embeddings = np.zeros((len(docs), EMB_DIMENSION))
save_path = "data/embeddings/tweets_from_deputesXVI_220620-230313_sentence-camembert-large.npz"

for i in tqdm(range(0, len(docs), batch_size), desc="Encode sentences using CamemBERT large"):
    try:
        embeddings[i:min(len(docs), i + batch_size)] = embedding_model.encode(docs[i:i + batch_size])
    except RuntimeError as e:
        np.savez_compressed(save_path.replace(".npz", "-{}.npz".format(i)), embeddings=embeddings)
        print(e)
        print("Trying to find source of error")
        for j in range(batch_size):
            idx = i + j
            try:
                embeddings[idx] = embedding_model.encode(docs[idx])
            except RuntimeError:
                print(idx, docs[idx])
                raise

np.savez_compressed(save_path, embeddings=embeddings)
for file_path in glob.glob(save_path.replace(".npz", "*")):
    if file_path != save_path:
        os.remove(file_path)

docs = np.array(docs)
for k in [100, 1_000, 10_000, 100_000]:
    sampled_idx = random.sample(range(1, len(docs)), k)
    sampled_docs = docs[sampled_idx]
    sampled_embeddings = embeddings[sampled_idx]
    
    compute_topics(sampled_docs, sampled_embeddings)


