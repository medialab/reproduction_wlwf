import pandas as pd
import numpy as np
from tslearn.metrics import dtw
from tslearn.clustering import TimeSeriesKMeans, silhouette_score
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler

db = pd.read_csv("data_prod/var/bertopic/general_TS_clean.csv")
variables = ['lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'general', 'media']
grouped = db.groupby('topic')[variables]
X = np.array([group.values for name,group in grouped])
print(X.shape)
for k in range(2,2): #2 semble meilleure si on regarde silhouette 
    print(k)
    model = TimeSeriesKMeans(n_clusters=k, metric="dtw", random_state=0)
    labels = model.fit_predict(X)
    print(labels)
    print(silhouette_score(X, labels, metric="dtw"))

for v in variables:
    print(v)
    df_pivot = db.pivot_table(index='topic', columns="date", values=v, aggfunc='mean')
    X_scaled = StandardScaler().fit_transform(df_pivot.values) 
    pca = PCA()
    X_pca = pca.fit_transform(X_scaled)
    print("SV")
    print(np.sum(pca.singular_values_ >= 1))

