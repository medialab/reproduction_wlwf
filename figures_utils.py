import os
import matplotlib.pyplot as plt
import numpy as np


def draw_topic_keywords(topic, words, x, root=os.getcwd(), reduced=False):
    """
    topic: the topic number
    words: a list of words
    x: list of values on the x-axis
    y: list of values on the y-axis
    """
    fig = plt.figure()
    ax = fig.add_subplot()

    interval_x = (max(x) - min(x)) / len(x)
    interval_y = range(len(words))

    max_word_length = max(
        len(word) for word in words
    )  # Création d'un espace pour l'axe des absisses afin que celui-ci s'adapte à la longueur des top words
    extra_space = max_word_length * 0.0003

    ax.axis(
        [
            min(x) - interval_x,
            max(x) + interval_x + extra_space,
            0,
            len(words) + 1,
        ]
    )

    ax.invert_yaxis()

    ax.set_xlabel("c-TF-IDF scores")
    ax.set_ylabel("Ranking")

    sorted_indices = np.argsort(x)[::-1] + 1

    for word, xvalue, rank in zip(words, x, sorted_indices):
        ax.text(xvalue, rank, word)

    ax.set_yticks(np.arange(1, len(words) + 1))  # On fixe les ticks pour l'axe y
    ax.get_yaxis().set_tick_params(which="both", left=False, right=False)

    if reduced:
        open_path = os.path.join("data_prod", "dashboard", "bertopic", "reduced", "img", "bertopic_{}.png".format(topic)
        )
    else:
        open_path = os.path.join(root,
            "data_prod", "dashboard", "bertopic", "img", "bertopic_{}.png".format(topic)
        )

    fig.savefig(
        os.path.join(open_path)
    )
    plt.close()

def draw_PCA_evolve(root, pca_model):
    component_numbers = list(range(1, len(pca_model.components_)+1))
    cumulative_variance = np.cumsum(pca_model.explained_variance_ratio_)
    fig, ax = plt.subplots()
    ax.plot(component_numbers, pca_model.singular_values_, "b")
    ax.set_xlabel("Component index", fontsize=14)
    ax.set_ylabel("Singular_values", fontsize=14)
    ax2 = ax.twinx()
    ax2.plot(component_numbers, pca_model.explained_variance_ratio_, "r")
    ax2.plot(component_numbers, cumulative_variance, "g", label="Variance expliquée cumulée (ratio)")
    ax2.set_ylabel("Explained variance (ratio)", fontsize=14)
    lines = [ax.get_lines()[0], ax2.get_lines()[0]]
    plt.legend(lines, ["Valeurs singulières", "Variance expliquée (ratio)", "Variance expliquée cumulée (ratio)"], loc="upper right")
    path_save = os.path.join(root, "data_prod", "figures", "Explained_VAR_PCA_graph.jpg")
    plt.savefig(path_save)
    plt.close()
