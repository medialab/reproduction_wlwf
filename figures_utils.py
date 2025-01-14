import os
import matplotlib.pyplot as plt


def draw_topic_keywords(topic, words, x, y):
    """
    topic: the topic number
    words: a list of words
    x: list of values on the x-axis
    y: list of values on the y-axis
    """
    fig = plt.figure()
    ax = fig.add_subplot()

    interval_x = (max(x) - min(x)) / len(x)
    interval_y = (max(y) - min(y)) / len(y)

    ax.axis(
        [
            min(x) - interval_x,
            max(x) + interval_x,
            min(y) - interval_y,
            max(y) + interval_y,
        ]
    )

    ax.set_xlabel("c-TF-IDF scores")

    for word, xvalue, yvalue in zip(words, x, y):
        ax.text(xvalue, yvalue, word)

    fig.savefig(
        os.path.join(
            "data_prod", "dashboard", "img", "bertopic_{}.png".format(topic)
        )
    )
    plt.close()
