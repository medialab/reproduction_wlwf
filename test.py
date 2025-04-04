import numpy as np
import io
import os
import random
import numpy as np
from scipy.sparse import csr_matrix
import pandas as pd
from sklearn.decomposition import LatentDirichletAllocation

from utils import (
    choices,
    vectorizer,
    count_nb_files,
    clean_text,
    existing_dir_path,
    iter_on_files,
    grep_group_name,
    RANDOM_SEED,
)
NB_RANDOM_TWEETS = 100000
ORIGINAL_TWEET_COUNT = 0

def create_df_sparse(public, userday=False):
    if userday:
        indices = np.loadtxt(f"data_prod/dfm/{public}-rs-dtm-indices.txt", dtype=int)
        indptr = np.loadtxt(f"data_prod/dfm/{public}-rs-dtm-pointers.txt", dtype=int)
        data = np.loadtxt(f"data_prod/dfm/{public}-rs-dtm-values.txt", dtype=int)
    else:
        indices = np.loadtxt(f"data_prod/dfm/{public}-day-dtm-indices.txt", dtype=int)
        indptr = np.loadtxt(f"data_prod/dfm/{public}-day-dtm-pointers.txt", dtype=int)
        data = np.loadtxt(f"data_prod/dfm/{public}-day-dtm-values.txt", dtype=int)

    csr_mat = csr_matrix((data, indices, indptr))
    return csr_mat

def ntopwlst(model, features, ntopwords):
    '''create a list of the top topc words'''
    output = []
    for topic_idx, topic in enumerate(model.components_): # compose output message with top words
        output.append(str(topic_idx))
        output += [features[i] for i in topic.argsort()[:-ntopwords - 1:-1]] # [start (0 if omitted): end : slicing increment]
    return output

pub_pred = ["media", "supporter", "general", "attentive", "media_rs", "congress_rs"]
num_topics = 100
pol_groups = ['lr', 'majority', 'nupes', 'rn']
lda = LatentDirichletAllocation(n_components=num_topics)

print("Training")


with open("data_prod/dfm/congress-words.txt", "r") as f:
    words = [term.strip() for term in f]

X = create_df_sparse("congress")
lda_topic_doc_matrix = lda.fit_transform(X)
lda_topic_doc_matrix = pd.DataFrame(lda_topic_doc_matrix)
print("Dim matrice congress lda", lda_topic_doc_matrix.shape)
lda_topic_doc_matrix.to_csv("data_prod/topics/lda-python/results-congress.csv", index=False)

print("Create top words")
ntopwords = 15 

topwds = ntopwlst(lda, words, ntopwords)
with open("data_prod/topics/lda-python/topwords.txt", "w") as f:
    for item in topwds:
        f.write("%s\n" % item)

for pub in pub_pred:
    print(f"predict for {pub}")
    userday = pub.endswith("_rs")
    print(userday)
    if pub == "supporter":
        print("in supp loop")
        X = create_df_sparse(pub, userday)
        df = pd.DataFrame(X.todense())
        df1, df2, df3, df4 = np.array_split(df, 4)
        for df_part, group in zip([df1, df2, df3, df4], pol_groups):
            public = group + "_supporters"
            array_part = df_part.to_numpy()
            X = csr_matrix(array_part)
            lda_topic_doc_matrix = lda.transform(X)
            lda_topic_doc_matrix = pd.DataFrame(lda_topic_doc_matrix)
            print(f"Dim matrice {public} lda", lda_topic_doc_matrix.shape)
            save_path = f"data_prod/topics/lda-python/results-{public}.csv"
            lda_topic_doc_matrix.to_csv(save_path, index=False)
    else: 
        print("out of supp loop")
        if userday:
            publicn = pub.split("_")[0] 
            X = create_df_sparse(publicn, userday)
        else:
            X = create_df_sparse(pub, userday)
        lda_topic_doc_matrix = lda.transform(X)
        lda_topic_doc_matrix = pd.DataFrame(lda_topic_doc_matrix)
        print(f"Dim matrice {pub} lda", lda_topic_doc_matrix.shape)
        save_path = f"data_prod/topics/lda-python/results-{pub}.csv"
        lda_topic_doc_matrix.to_csv(save_path, index=False)




