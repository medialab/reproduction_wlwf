"""
# 01-create-dtm.py

Date: 21/11/2024
Author: Béatrice Mazoyer, from Pablo Barberá's script

Takes csv files with tweets grouped by day and party and prepares
a sparse matrix with unigram + bigram counts per document.

A document can be :
    - all tweets contained in a file (i.e. 1 doc = 1 day)
    - all tweets emitted by a given user in a file (i.e. 1 doc = 1 user on 1 day)
    - a tweet

Options are available to use the same vocabulary (computed on MP's tweets) on other corpora

NOTE: running this script requires access to the original corpus of
tweets, which we cannot share publicly in order to comply with Twitter's
terms of service. For advice on how to re-run this script, please
contact the corresponding author.

All other scripts can be run without running this one first, since we
are providing the files required to replicate the document-feature matrices
we use in the analysis.

"""

import io
import random
import numpy as np
import casanova
import argparse
from unidecode import unidecode
from collections import defaultdict

from utils import (
    vectorizer,
    count_nb_files,
    clean_text,
    existing_dir_path,
    iter_on_files,
    grep_group_name,
    RANDOM_SEED,
)

ORIGINAL_TWEET_COUNT = 0


def sort_row_data(X):
    """Sort indices and data in a fixed order,
    so that the order remains the same if --vocab is provided when we re-run
    the script.
    """
    for i in range(len(X.indptr) - 1):
        row = X.indices[X.indptr[i] : X.indptr[i + 1]]
        ind = np.argsort(row)
        X.indices[X.indptr[i] : X.indptr[i + 1]] = np.take_along_axis(row, ind, axis=0)
        X.data[X.indptr[i] : X.indptr[i + 1]] = np.take_along_axis(
            X.data[X.indptr[i] : X.indptr[i + 1]], ind, axis=0
        )


def group_by_file_and_user(root, nb_files, public, random_tweets=False):
    # Preprocess tweets from 'one file (per party if relevant) per day' to 'one document per user per file'
    names_file = open("data_prod/dfm/{}-userday-user-list.txt".format(public), "w")
    if random_tweets:
        random_tweets_file = open(
            "data_prod/dfm/{}-rs-tweet-list.csv".format(public), "w"
        )
        random.seed(RANDOM_SEED)
        random_tweet_indices = sorted(
            random.sample(range(ORIGINAL_TWEET_COUNT), k=10000)
        )
        current_index = 0

    counter_original = 0
    counter_all = 0

    tar, loop, compressed = iter_on_files(root, nb_files)

    for file in loop:
        if compressed:
            filename = file.name
            filestream = io.TextIOWrapper(tar.extractfile(file))
        else:
            filename = file
            filestream = open(file)
        loop.set_description(filename)

        if random_tweets:
            reader = casanova.enricher(
                filestream,
                random_tweets_file,
                write_header=not bool(counter_all),
                select=["id", "local_time", "text", "user_screen_name"],
            )
        else:
            reader = casanova.reader(filestream)

        text_pos = reader.headers.text
        user_id_pos = reader.headers.user_id
        user_sn_pos = reader.headers.user_screen_name
        rt_pos = reader.headers.retweeted_id
        loop.set_description(filename)
        users = defaultdict(lambda: {"text": "", "screen_names": {}})

        for row in reader:
            counter_all += 1
            if not row[rt_pos]:
                if (
                    random_tweets
                    and current_index < len(random_tweet_indices)
                    and counter_original == random_tweet_indices[current_index]
                ):
                    reader.writerow(row)
                    current_index += 1
                counter_original += 1
                row_text = unidecode(clean_text(row[text_pos].replace("\n", "")))
                row_user_id = int(row[user_id_pos])

                user_text = users[row_user_id]["text"]
                screen_names = users[row_user_id]["screen_names"]

                user_text += row_text + " "
                screen_names[row[user_sn_pos]] = None

        for user_id, user_values in users.items():
            names_file.write(
                "{} {} {}\n".format(
                    grep_group_name(filename),
                    user_id,
                    list(user_values["screen_names"])[0],
                )
            )
            yield user_values["text"][:-1]

    names_file.close()
    if random_tweets:
        random_tweets_file.close()


def group_by_file(root, nb_files, write_party, public):
    global ORIGINAL_TWEET_COUNT
    # Preprocess tweets from 'one file (per party if relevant) per day' to 'one document per file'
    counter_all = 0
    if write_party:
        group_names_file = open("data_prod/dfm/{}-party-list.txt".format(public), "w")

    tar, loop, compressed = iter_on_files(root, nb_files)

    for file in loop:
        if compressed:
            filename = file.name
        else:
            filename = file

        loop.set_description(filename)

        if write_party:
            group_names_file.write("%s\n" % grep_group_name(filename))

        reader = casanova.reader(filename)
        text_pos = reader.headers.text
        rt_pos = reader.headers.retweeted_id
        loop.set_description(filename)

        file_text = ""
        for row in reader:
            counter_all += 1
            if not row[rt_pos]:
                ORIGINAL_TWEET_COUNT += 1
                file_text += (
                    unidecode(clean_text(row[text_pos].replace("\n", ""))) + " "
                )

        # yield the text of all tweets of the day, remove last character - which is a space
        yield file_text[:-1]

    if write_party:
        group_names_file.close()


def export_dfm_matrix(args, X, words, granularity):
    save_pattern = args.name + "-" + granularity

    np.savetxt(
        "data_prod/dfm/{}-dtm-indices.txt".format(save_pattern),
        X.indices,
        fmt="%.0f",
    )
    np.savetxt(
        "data_prod/dfm/{}-dtm-pointers.txt".format(save_pattern),
        X.indptr,
        fmt="%.0f",
    )
    np.savetxt(
        "data_prod/dfm/{}-dtm-values.txt".format(save_pattern),
        X.data,
        fmt="%.0f",
    )

    ## words
    if granularity == "day":
        with open("data_prod/dfm/{}-nb_files.txt".format(args.name), "w") as f:
            f.write(str(nb_files))
        with open("data_prod/dfm/{}-words.txt".format(args.name), "w") as f:
            for item in words:
                f.write("%s\n" % item)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "folder",
        help="Path to a folder containing all input csv files",
        type=existing_dir_path,
    )
    parser.add_argument(
        "name",
        help="""Name of the output files. For example, if the name is 'congress',
        the output files will be 'congress-dtm-indices.txt', 'congress-dtm-pointers.txt', etc.""",
    )
    parser.add_argument(
        "--vocab",
        help="""file in .txt with one token per line.
        By default, the vocabulary will be infered from the csv files.
        """,
        required=False,
        type=argparse.FileType("r"),
    )
    parser.add_argument(
        "--party",
        help="Write the party of each csv file inferred from csv file path",
        action="store_true",
    )
    parser.add_argument(
        "--granularity",
        choices=["day", "user", "tweet"],
        default=["day", "user", "tweet"],
        nargs="*",
        help="""Group documents by day, by user per day, or by tweets. By default, compute all granularities.
        You can choose several values separated by a space, e.g. --granularity day user
        If day is not one of the chosen value, a vocabulary should be passed using the --vocab flag.
        """,
    )

    args = parser.parse_args()

    nb_files = count_nb_files(args.folder)

    if "day" not in args.granularity and not args.vocab:
        message = "Consider running the script with '--granularity day' level or providing a --vocab file before "
        "creating the document-term matrix with a different granularity"
        raise argparse.ArgumentError(None, message)

    if args.vocab:
        vectorizer.vocabulary = [term.strip() for term in args.vocab]

    if "day" in args.granularity:
        # print("Compute dtm matrix at day granularity")
        X = vectorizer.fit_transform(
            group_by_file(args.folder, nb_files, args.party, args.name)
        )
        if not args.vocab:
            sort_row_data(X)
        export_dfm_matrix(args, X, vectorizer.get_feature_names_out(), "day")

    if "user" in args.granularity:
        random_tweets = "tweet" in args.granularity
        # print("Compute dtm matrix at user per day granularity")
        X = vectorizer.transform(
            group_by_file_and_user(
                args.folder, nb_files, args.name, random_tweets=random_tweets
            )
        )
        export_dfm_matrix(args, X, vectorizer.get_feature_names_out(), "userday")

    if "tweet" in args.granularity:
        X = vectorizer.transform(
            (
                row[2]
                for row in casanova.reader(
                    "data_prod/dfm/{}-rs-tweet-list.csv".format(args.name)
                )
            )
        )
        export_dfm_matrix(args, X, vectorizer.get_feature_names_out(), "rs")
