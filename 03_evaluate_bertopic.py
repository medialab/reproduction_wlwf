import pandas as pd
import argparse
from bertopic import BERTopic
import os
from sklearn.metrics import homogeneity_completeness_v_measure

from utils import existing_dir_path, SBERT_NAME

parser = argparse.ArgumentParser()

parser.add_argument(
    "model_path",
    help="Path to a folder containing the model",
    type=existing_dir_path,
)

parser.add_argument(
    "--load_annot",
    help="If you want to create the dataframe for human annotation, use this option",
    action="store_true",
)

parser.add_argument(
    "--outliers",
    help="If you want to include outliers to calculate ground truth based metrics",
    action="store_true",
)
args = parser.parse_args()

print("Load BERTOPIC model")

topic_model = BERTopic.load(args.model_path, embedding_model=SBERT_NAME)

if args.load_annot:
    print("Load annotation data")
    list_df = []
    nb_annotateurs = 9

    for i in range(nb_annotateurs):
        path = os.path.join(
            "data_prod", "evaluation", "BERTopic_evaluation_collective.xlsx"
        )
        df_annotateur = pd.read_excel(path, sheet_name=i + 1)
        list_df.append(df_annotateur)
    # Il faudra gérer ces duplicates

    df_merged = pd.concat(list_df, ignore_index=True)
    df_merged = df_merged.rename(
        columns={
            "Topic": "Topic_doc",
            "pertinence": "pertinence_doc",
            "coherence": "coherence_doc",
            "Topic.1": "Topic_gen",
            "pertinence.1": "pertinence_gen",
            "coherence.1": "coherence_gen",
        }
    )

    path_doc = os.path.join("data_prod", "evaluation", "evaluation_doc_merged.csv")
    path_topic = os.path.join("data_prod", "evaluation", "evaluation_topic_merged.csv")
    df_merged[
        [
            "Document",
            "Topic_doc",
            "Top_n_words",
            "Representative_Docs",
            "pertinence_doc",
            "coherence_doc",
        ]
    ].to_csv(path_doc, index=False)
    df_merged[["Topic_gen", "pertinence_gen", "coherence_gen", "nom"]].to_csv(
        path_topic, index=False
    )

# Ground truth measure
path_doc = os.path.join("data_prod", "evaluation", "evaluation_doc_merged.csv")
df_annot = pd.read_csv(path_doc)
df_annot = df_annot.drop([])


if not args.outliers:
    df_annot = df_annot[df_annot["Topic_doc"] != -1]

df_annot = df_annot.dropna(subset=["pertinence_doc"])

df_ground = pd.DataFrame()
df_ground["topic_pred"] = df_annot["Topic_doc"]


def create_topic_true(row):
    if df_annot.loc[row, "pertinence_doc"] == "oui":
        return df_annot.loc[row, "Topic_doc"]
    elif df_annot.loc[row, "pertinence_doc"] == "non":
        return 500  # Choose a number that doesn't correpsond to an existing topic
    else:
        raise ValueError("One of your annotation have a wrong format")


df_ground["topic_true"] = df_ground.index.to_series().apply(create_topic_true)

ground_measure = homogeneity_completeness_v_measure(
    df_ground["topic_true"], df_ground["topic_pred"]
)
print(ground_measure)
