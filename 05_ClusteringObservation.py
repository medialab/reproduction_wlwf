import pandas as pd
import numpy as np
from tslearn.metrics import dtw
from tslearn.clustering import TimeSeriesKMeans, silhouette_score

db = pd.read_csv("data_prod/var/bertopic/general_TS_clean.csv")
variables = ['lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'general', 'media']
grouped = db.groupby('topic')[variables]
X = np.array([group.values for name,group in grouped])
print(X.shape)
for k in range(2,11):
    print(k)
    model = TimeSeriesKMeans(n_clusters=k, metric="dtw", random_state=0)
    labels = model.fit_predict(X)
    print(labels)
    print(silhouette_score(X, labels, metric="dtw"))