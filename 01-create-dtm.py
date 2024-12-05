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
import os
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
STORE_PATH = os.path.join("data_prod", "dfm")


def sort_row_data(X):
    """Sort indices and data in a fixed order"""
    for i in range(len(X.indptr) - 1):
        row = X.indices[X.indptr[i] : X.indptr[i + 1]]
        ind = np.argsort(row)
        X.indices[X.indptr[i] : X.indptr[i + 1]] = np.take_along_axis(row, ind, axis=0)
        X.data[X.indptr[i] : X.indptr[i + 1]] = np.take_along_axis(
            X.data[X.indptr[i] : X.indptr[i + 1]], ind, axis=0
        )


def grep_date(filename):
    head, date = os.path.split(filename)
    if "-" not in date:
        dashed_date = date[:4] + "-" + date[4:6] + "-" + date[6:]
    else:
        dashed_date = date
    head, partyname = os.path.split(head)
    return os.path.join(partyname, dashed_date)


def generate_path(file):
    return os.path.join(STORE_PATH, file)


def group_by_file_and_user(root, nb_files, public, random_tweets=False):
    # Preprocess tweets from 'one file (per party if relevant) per day' to 'one document per user per file'
    names_file = open(generate_path("{}-userday-user-list.txt".format(public)), "w")
    if random_tweets:
        random_tweets_file = open(
            generate_path("{}-rs-tweet-list.csv".format(public)), "w"
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

                users[row_user_id]["text"] += row_text + " "
                users[row_user_id]["screen_names"][row[user_sn_pos]] = None

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
        group_names_file = open(
            generate_path("{}-day-party-list.txt".format(public)), "w"
        )

    tar, loop, compressed = iter_on_files(root, nb_files)

    for file in loop:
        if compressed:
            filename = file.name
        else:
            filename = file

        loop.set_description(filename)

        if write_party:
            group_names_file.write("%s\n" % grep_date(filename))

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


def export_dfm_matrix(public, X, granularity):
    save_pattern = public + "-" + granularity

    np.savetxt(
        generate_path("{}-dtm-indices.txt".format(save_pattern)),
        X.indices,
        fmt="%.0f",
    )
    np.savetxt(
        generate_path("{}-dtm-pointers.txt".format(save_pattern)),
        X.indptr,
        fmt="%.0f",
    )
    np.savetxt(
        generate_path("{}-dtm-values.txt".format(save_pattern)),
        X.data,
        fmt="%.0f",
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "public",
        choices=["congress", "media", "supporter", "attentive", "general"],
        help="""Name of the publics. For example, if the public is 'congress',
        the output files will be 'congress-day-dtm-indices.txt', etc.""",
    )

    parser.add_argument(
        "folder",
        help="Path to a folder containing tweets files for the chosen public, in csv format",
        type=existing_dir_path,
    )

    args = parser.parse_args()

    nb_files = count_nb_files(args.folder)

    vocab_file_path = generate_path("congress-words.txt")
    public_is_congress = args.public == "congress"
    if not public_is_congress:
        if os.path.isfile(vocab_file_path):
            with open(vocab_file_path, "r") as f:
                vectorizer.vocabulary = [term.strip() for term in f]

    print("Compute dtm matrix at day granularity")
    X = vectorizer.fit_transform(
        group_by_file(
            args.folder,
            nb_files,
            args.public in ["congress", "supporters"],
            args.public,
        )
    )
    if public_is_congress:
        sort_row_data(X)

        words = vectorizer.get_feature_names_out()
        with open(vocab_file_path, "w") as f:
            for item in words:
                f.write("%s\n" % item)

    with open(generate_path("{}-nb-files.txt".format(args.public)), "w") as f:
        f.write(str(nb_files))

    export_dfm_matrix(args.public, X, "day")

    if args.public in ["congress", "media"]:
        print("Compute dtm matrix at user per day granularity")
        X = vectorizer.transform(
            group_by_file_and_user(
                args.folder, nb_files, args.public, random_tweets=True
            )
        )
        export_dfm_matrix(args.public, X, "userday")

        print("Compute dtm matrix at tweet granularity")
        X = vectorizer.transform(
            (
                row[2]
                for row in casanova.reader(
                    generate_path("{}-rs-tweet-list.csv".format(args.public))
                )
            )
        )
        export_dfm_matrix(args.public, X, "rs")
