import os
import matplotlib.pyplot as plt
import numpy as np
from utils import create_dir


def draw_topic_keywords(topic, words, x, root=os.getcwd()):
    """
    topic: the topic number
    words: a list of words
    x: list of values on the x-axis
    y: list of values on the y-axis
    """
    fig = plt.figure()
    ax = fig.add_subplot()

    interval_x = (max(x) - min(x)) / len(x)

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

    output_folder = create_dir(
        os.path.join(root, "data_prod", "dashboard", "bertopic", "img")
    )

    fig.savefig(
        os.path.join(
            output_folder,
            "bertopic_{}.png".format(topic),
        )
    )
    plt.close()
