import time
import random

from bertopic import BERTopic
import numpy as np
import matplotlib.pyplot as plt

from utils import TWEETS_FOLDER, count_nb_files, vectorizer, preprocess

def st_time(func):
    """
        decorator to compute the total time of a function
    """

    def st_func(*args, times=[]):
        t1 = time.time()
        r = func(*args, times=times)
        t2 = time.time()
        print("Function=%s, Time=%s" % (func.__name__, t2 - t1))
        times.append(t2 - t1)
        return r

    return st_func


@st_time
def compute_topics(docs, embeddings, times=[]):
    print("Nb documents: ", len(docs))
    topics, probs = topic_model.fit_transform(docs, embeddings)
    print(topic_model.get_topic_info())


topic_model = BERTopic(

  # Pipeline models
  vectorizer_model=vectorizer,

  # Hyperparameters
  top_n_words=10,
  verbose=True

)


def plot_time(x, y):
    fig, ax = plt.subplots()
    ax.plot(x, y)
    
    ax.set(xlabel='nb_docs', ylabel='time (s)')
    ax.grid()
    
    fig.savefig("plots/bertopic_time.png")
    plt.show()



docs = preprocess(TWEETS_FOLDER)
save_path = "data/embeddings/tweets_from_deputesXVI_220620-230313_sentence-camembert-large.npz"

embeddings = np.load(save_path)["embeddings"]

docs = np.array(docs)

sizes = [1_000, 10_000, 100_000, len(docs)]
times = []

for k in [1_000, 10_000, 100_000, len(docs)]:
    if k == len(docs):
        sampled_idx = range(len(docs))
    else:
        sampled_idx = random.sample(range(1, len(docs)), k)
    sampled_docs = docs[sampled_idx]
    sampled_embeddings = embeddings[sampled_idx]
    
    compute_topics(sampled_docs, sampled_embeddings, times=times)

plot_time(sizes, times)