"""
# 01-create-dtm.py

Date: 6/6/2015
Author: Pablo Barberá

Takes txt files with tweets grouped by chamber and party and prepares
a sparse matrix with unigram + bigram counts per document.

Then applies same tokenizer to user and media corpus

NOTE: running this script requires access to the original corpus of
tweets, which we cannot share publicly in order to comply with Twitter's
terms of service. For advice on how to re-run this script, please
contact the corresponding author.

All other scripts can be run without running this one first, since we
are providing the files required to replicate the document-feature matrices
we use in the analysis.

"""

import glob
import numpy as np
import casanova
import argparse
from unidecode import unidecode
from tqdm import tqdm

from utils import (
    vectorizer,
    count_nb_files,
    clean_text,
    existing_dir_path,
    iter_on_files,
    grep_group_name,
)


def preprocess(root, nb_files, write_party):
    counter_all = 0
    counter_original = 0
    if write_party:
        group_names_file = open("data_prod/dfm/supporter-users-list.txt", "w")

    tar, compressed, loop = iter_on_files(root, nb_files)

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
                counter_original += 1
                try:
                    row_text = unidecode(clean_text(row[text_pos].replace("\n", "")))
                except IndexError:
                    print(filename)
                    print(row)
                    continue
                file_text += row_text + " "

        # yield the text of all tweets of the day, remove last character - which is a space
        yield file_text[:-1]
    if write_party:
        group_names_file.close()
    print(
        "nb of tweets: {}, nb of original tweets: {}".format(
            counter_all, counter_original
        )
    )


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
    args = parser.parse_args()

    nb_files = count_nb_files(args.folder)

    if args.vocab:
        vectorizer.vocabulary = [term.strip() for term in args.vocab]

    X = vectorizer.fit_transform(preprocess(args.folder, nb_files, args.party))

    # checking words
    words = vectorizer.get_feature_names_out()
    print(words[:10], words[-10:])

    ####################################
    # exporting DFM matrix
    ####################################

    np.savetxt(
        "data_prod/dfm/{}-dtm-indices.txt".format(args.name), X.indices, fmt="%.0f"
    )
    np.savetxt(
        "data_prod/dfm/{}-dtm-pointers.txt".format(args.name), X.indptr, fmt="%.0f"
    )
    np.savetxt("data_prod/dfm/{}-dtm-values.txt".format(args.name), X.data, fmt="%.0f")
    with open("data_prod/dfm/{}-nb_files.txt".format(args.name), "w") as f:
        f.write(str(nb_files))

    ## words
    with open("data_prod/dfm/{}-words.txt".format(args.name), "w") as f:
        for item in words:
            f.write("%s\n" % item)
