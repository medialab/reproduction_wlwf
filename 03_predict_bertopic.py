import os
import json
import random
import argparse

from bertopic import BERTopic
import bertopic._save_utils as save_utils
import numpy as np

from utils import (
    count_nb_files,
    create_dir,
    existing_dir_path,
    vectorizer,
    preprocess,
    load_embeddings,
    SBERT_NAME,
    DEFAULT_SAVE_SIZE,
    RANDOM_SEED,
)

parser = argparse.ArgumentParser()
parser.add_argument(
    "input_path",
    help="Path to a folder or to a .tar.xz archive containing all input csv files that we want to predict topics",
    type=existing_dir_path,
)
parser.add_argument(
    "embeddings_folder",
    help="Path to a folder containing .npz embeddings. It is the output_folder arg in 01_encode_with_sbert.py",
)

parser.add_argument(
    "model_path",
    help = "Path to a folder containing the model. It is the output_folder arg in 02_run_bertopic.py" #Il faudra peut-être être plus précis sur ce contenu
)

'''parser.add_argument(
    "output_folder",
    help="Path to a folder that will be created and contain the predictions",
    type=create_dir,
)
'''

parser.add_argument(
    "--save-size",
    help="Size of saved files (in embeddings_folder) in number of vectors",
    type=int,
    default=DEFAULT_SAVE_SIZE,
)
parser.add_argument(
    "--small",
    help=("run the script on one week of data"),
    action="store_true",
)
args = parser.parse_args()
sbert_name_string = SBERT_NAME.replace("/", "_")
embeddings_path = os.path.join(
    args.embeddings_folder, "{}.npz".format(sbert_name_string)
)

#Add a dict_file here + dans le code preprocess quand on aura fait le lien avec la branche principale 

docs = np.array(
    [
        doc
        for doc in preprocess(
            args.input_path, count_nb_files(args.input_path), apply_unidecode=True
        )
    ]
)

max_index, embeddings = load_embeddings(
    embeddings_path,
    args.save_size,
    docs.shape[0],
)

print("Predict model")

topic_model = BERTopic.load(args.model_path, embedding_model = SBERT_NAME)

if args.small:
    indices = random.choices(range(len(docs)), k=1000)
    docs = docs[indices]
    embeddings = embeddings[indices]
    topics, probs = topic_model.transform(docs, embeddings)
    # Préparer les données sous forme d'un tableau numpy
    data = np.column_stack((topics, probs))
    header = "Topic,Probability"  # En-tête du fichier
    last_part = os.path.basename(embeddings_path)
    output_file = f"data_prod/topics/{last_part}_topics_pred_results_SMALL.csv"
else:
    topics, probs = topic_model.transform(docs, embeddings)
    # Préparer les données sous forme d'un tableau numpy
    data = np.column_stack((topics, probs))
    header = "Topic,Probability"  # En-tête du fichier
    last_part = os.path.basename(embeddings_path)
    output_file = f"data_prod/topics/{last_part}_topics_pred_results.csv"
    
np.savetxt(output_file, data, fmt="%s", delimiter=",", header=header, comments="")  
print(topic_model.get_topic_info())

'''
#Résultats sous forme de Time Series

proportion = []
start_idx = 0
for key, value in dict_file.items():
    end_idx = int(key) #On récupère l'index de fin
    if args.small: #On récupère le groupe considéré 
        elements_group = [item for item in topics_ind if start_idx <= item[1] < end_idx] 
    else:
        elements_group = topics_ind[start_idx:end_idx]
    if elements_group != []:
        for topic_number in np.unique(topics): #Calcul de la proportion pour chaque topic 
            elem_in_topic = sum(1 for elem in elements_group if elem[0]==topic_number) #Compte le nombre d'éléments dans le topic

            proportion.append((value[0], value[1], topic_number, elem_in_topic/ len(elements_group)))

            start_idx = end_idx #Mise à jour de l'indice

proportion = np.array(proportion)

for topic_number in np.unique(topics):
    to_export = [item for item in proportion if item[2] == topic_number] #Sélection des lignes correspondant au topic envisagé
    noms_col = np.array(['date', 'party', 'topic', 'prop']) #Création d'une ligne pour les noms de colonne
    export_pathname = f"data_prod/dashboard/files/BERT_TS_Topic_{topic_number}.csv"
    np.savetxt("output.csv", to_export, delimiter=",", fmt="%s", header="date,party,topic,prop", comments='')

'''